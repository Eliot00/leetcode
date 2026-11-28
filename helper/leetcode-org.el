;;; leetcode-org.el --- LeetCode workflow from org-mode -*- lexical-binding: t -*-

;;; Commentary:

;; Keep writing problem headings yourself.  The commands here find the problem
;; id (and slug, when the heading is a LeetCode link) on the nearest heading,
;; show the statement in a side window, and drive `leetcode test' / `leetcode
;; exec' for the source block at point.  Submission statistics are written
;; back to the heading as org properties, so the org file stays the notebook.

;;; Code:

(require 'subr-x)
(require 'cl-lib)
(require 'org)
(require 'org-element)
(require 'ob-core)
(require 'sqlite nil t)

(defgroup leetcode-org nil
  "LeetCode workflow from org-mode."
  :group 'org
  :prefix "leetcode-org-")

(defcustom leetcode-org-cli nil
  "Absolute path or executable name of the LeetCode CLI, or nil to search.
When nil, `exec-path' and a few common Nix locations are tried."
  :type '(choice (const nil) string))

(defcustom leetcode-org-db (expand-file-name "~/.leetcode/Problems")
  "SQLite problem cache written by the LeetCode CLI."
  :type 'file)

(defcustom leetcode-org-site nil
  "LeetCode site used to build problem URLs, e.g. \"leetcode.cn\".
When nil, the value of site in ~/.leetcode/leetcode.toml is used, falling
back to \"leetcode.cn\"."
  :type '(choice (const nil) string))

(defcustom leetcode-org-results-buffer "*leetcode*"
  "Buffer name used for CLI output."
  :type 'string)

(defcustom leetcode-org-description-buffer "*leetcode problem*"
  "Buffer name used for the problem statement."
  :type 'string)

(defcustom leetcode-org-description-side 'right
  "Side used for the problem statement window."
  :type '(choice (const right) (const left)))

(defcustom leetcode-org-results-side 'bottom
  "Side used for the CLI output window."
  :type '(choice (const bottom) (const top)))

(defcustom leetcode-org-description-width 0.42
  "Width of the description side window as a fraction of the frame."
  :type 'number)

(defcustom leetcode-org-results-height 0.30
  "Height of the results side window as a fraction of the frame."
  :type 'number)

(defcustom leetcode-org-confirm-submit t
  "Whether `leetcode-org-submit' asks for confirmation."
  :type 'boolean)

(defcustom leetcode-org-record-submission t
  "Whether submission statistics are written back to the org heading."
  :type 'boolean)

(defcustom leetcode-org-record-result-line t
  "Whether a visible `#+LEETCODE_RESULT:' line is kept under the heading."
  :type 'boolean)

(defcustom leetcode-org-imenu-limit 300
  "Maximum number of problems listed per difficulty in the imenu index."
  :type 'integer)

(defvar-local leetcode-org--current-id nil
  "Problem id explicitly selected in this buffer, if any.")
(defvar-local leetcode-org--current-id-marker nil
  "Marker of the heading to which `leetcode-org--current-id' belongs.")

(defvar leetcode-org--db-handle nil
  "Cached SQLite connection to the problem cache.")

(defvar leetcode-org--imenu-cache nil
  "Cached imenu index for the problem list.")

;;; Paths and process environment

(defun leetcode-org--project-root ()
  "Return the LeetCode project root for the current buffer."
  (or (and buffer-file-name
           (locate-dominating-file buffer-file-name "helper/leetcode-org.el"))
      (and buffer-file-name (file-name-directory buffer-file-name))
      default-directory))

(defun leetcode-org--cli-path ()
  "Return the absolute path of the LeetCode CLI."
  (let* ((root (leetcode-org--project-root))
         (custom (and leetcode-org-cli
                      (if (string-match-p "/" leetcode-org-cli)
                          (expand-file-name leetcode-org-cli root)
                        (or (executable-find leetcode-org-cli)
                            (expand-file-name leetcode-org-cli root)))))
         (candidates
          (delq nil
                (list custom
                      (executable-find "leetcode")
                      (expand-file-name "/run/current-system/sw/bin/leetcode")
                      (expand-file-name "~/.nix-profile/bin/leetcode")
                      (expand-file-name "/usr/local/bin/leetcode")))))
    (or (cl-find-if #'file-executable-p candidates)
        (user-error "Cannot find the leetcode CLI (set `leetcode-org-cli')"))))

(defun leetcode-org--toml-value (key)
  "Return the string value of KEY from ~/.leetcode/leetcode.toml, if present."
  (let ((file (expand-file-name
               "leetcode.toml"
               (file-name-directory (expand-file-name leetcode-org-db)))))
    (when (file-readable-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (when (re-search-forward
               (format "^%s\\s-*=\\s-*\"\\([^\"]+\\)\"" (regexp-quote key))
               nil t)
          (match-string 1))))))

(defun leetcode-org--site-name ()
  "Return the LeetCode site domain used for problem URLs."
  (or leetcode-org-site
      (leetcode-org--toml-value "site")
      "leetcode.cn"))

(defun leetcode-org--process-environment ()
  "Return a `process-environment' for CLI calls."
  (append (list "NO_COLOR=1" "TERM=dumb" "RUST_LOG=off")
          process-environment))

(defun leetcode-org--code-dir ()
  "Return the directory where the CLI keeps solution files."
  (let* ((root (or (leetcode-org--toml-value "root") "~/.leetcode"))
         (code (or (leetcode-org--toml-value "code") "code")))
    (expand-file-name code (expand-file-name root))))

(defun leetcode-org--find-code-file (fid)
  "Return an existing solution file for FID, preferring the newest one."
  (let* ((dir (leetcode-org--code-dir))
         (prefix (concat (number-to-string fid) "."))
         (files (and (file-directory-p dir)
                     (directory-files dir t nil t))))
    (setq files
          (cl-remove-if-not
           (lambda (file)
             (and (file-regular-p file)
                  (string-prefix-p prefix (file-name-nondirectory file))
                  (not (string-suffix-p ".tests.dat" file))))
           files))
    (when files
      (car (sort files
                 (lambda (a b)
                   (let ((ta (nth 5 (file-attributes a)))
                         (tb (nth 5 (file-attributes b))))
                     (and ta tb (time-less-p tb ta)))))))))

(defun leetcode-org--probe-code-path (fid)
  "Ask the CLI for the exact solution file it will read for FID.
The CLI opens that file with `EDITOR'; setting it to /bin/echo makes the
CLI print the path, which avoids duplicating its language/suffix table."
  (let* ((default-directory (expand-file-name default-directory))
         (echo (or (executable-find "echo") "/bin/echo"))
         (cli (leetcode-org--cli-path))
         (buffer (generate-new-buffer " *leetcode edit*"))
         (process-environment
          (append (list (concat "EDITOR=" echo)
                        (concat "VISUAL=" echo))
                  (leetcode-org--process-environment)))
         (status nil)
         path)
    (unwind-protect
        (progn
          (with-current-buffer buffer (erase-buffer))
          (setq status
                (apply #'call-process cli nil buffer nil
                       (list "-d" "edit" (number-to-string fid))))
          (with-current-buffer buffer
            (let* ((lines (split-string (buffer-string) "\n" t))
                   (line (string-trim (or (car (last lines)) ""))))
              (setq path
                    (cond
                     ((and (not (string-empty-p line))
                           (file-exists-p line))
                      line)
                     ((not (string-empty-p line))
                      (car (last (split-string line nil t))))))))
          (unless (and (zerop status)
                       path
                       (stringp path)
                       (not (string-empty-p path))
                       (file-exists-p path))
            (setq path (leetcode-org--find-code-file fid)))
          path)
      (kill-buffer buffer))))

(defun leetcode-org--resolve-code-path (fid)
  "Return the CLI solution file for FID, creating it through `leetcode edit'."
  (or (leetcode-org--probe-code-path fid)
      (user-error "Could not resolve a solution file for problem %s" fid)))

(defun leetcode-org--write-code (file body)
  "Write BODY to FILE as UTF-8 Unix text."
  (let ((coding-system-for-write 'utf-8-unix))
    (with-temp-file file
      (insert body)
      (unless (string-suffix-p "\n" body)
        (insert "\n")))))

;;; Problem cache

(defun leetcode-org--db ()
  "Return an open connection to the problem cache."
  (unless (fboundp 'sqlite-open)
    (user-error "This Emacs was built without SQLite support"))
  (unless (file-readable-p leetcode-org-db)
    (user-error "Problem cache not found: %s (run `leetcode data -u')"
                leetcode-org-db))
  (unless leetcode-org--db-handle
    (setq leetcode-org--db-handle (sqlite-open leetcode-org-db)))
  leetcode-org--db-handle)

(defun leetcode-org--query (sql &optional params)
  "Run SQL with PARAMS against the problem cache."
  (if params
      (sqlite-select (leetcode-org--db) sql params)
    (sqlite-select (leetcode-org--db) sql)))

(defun leetcode-org--db-row (fid)
  "Return (FID NAME LEVEL SLUG STATUS) for problem FID, or nil."
  (when (and fid (file-readable-p leetcode-org-db))
    (condition-case err
        (car (leetcode-org--query
              "select fid, name, level, slug, status from problems where fid = ? limit 1"
              (list fid)))
      (error
       (message "LeetCode cache query failed: %s" (error-message-string err))
       nil))))

(defun leetcode-org--fid-by-slug (slug)
  "Return the problem id whose slug is SLUG, or nil."
  (when (and slug (file-readable-p leetcode-org-db))
    (condition-case nil
        (car (leetcode-org--query
              "select fid from problems where slug = ? limit 1"
              (list slug)))
      (error nil))))

(defun leetcode-org--level-name (level)
  "Human readable name for LEVEL."
  (pcase level
    (1 "Easy")
    (2 "Medium")
    (3 "Hard")
    (_ "Other")))

(defun leetcode-org-list-problems (&optional level query limit)
  "Return problems as (FID LEVEL NAME STATUS) rows.
LEVEL is 1/2/3 or nil for all.  QUERY filters by substring of the name.
LIMIT caps the number of rows."
  (let ((sql "select fid, level, name, status from problems where 1=1")
        (params nil))
    (when level
      (setq sql (concat sql " and level = ?"))
      (push level params))
    (when query
      (setq sql (concat sql " and (name like ? or slug like ?)"))
      (push (concat "%" query "%") params)
      (push (concat "%" query "%") params))
    (setq sql (concat sql " order by fid limit ?"))
    (push (or limit leetcode-org-imenu-limit) params)
    (leetcode-org--query sql (nreverse params))))

;;; Reading the org heading

(defun leetcode-org--heading-info-at-point ()
  "Return (:fid :slug :title :url :raw) for the heading at point.
Return nil when the heading has neither a problem id nor a LeetCode slug."
  (save-excursion
    (condition-case nil
        (progn
          (org-back-to-heading t)
          (let* ((raw (or (org-element-property :raw-value (org-element-at-point))
                          (org-get-heading t t t t)))
                 (title (string-trim (org-link-display-format raw)))
                 (fid-raw (or (org-entry-get nil "LEETCODE_ID")
                              (and (string-match "\\([0-9]+\\)[.、]" title)
                                   (match-string 1 title))
                              (and (string-match "\\`\\[\\([0-9]+\\)\\]" title)
                                   (match-string 1 title))
                              (and (string-match "\\`\\([0-9]+\\)\\s-" title)
                                   (match-string 1 title))
                              (and (string-match "\\`\\([0-9]+\\)\\'" title)
                                   (match-string 1 title))))
                 (fid (cond ((numberp fid-raw) fid-raw)
                            ((stringp fid-raw)
                             (string-to-number fid-raw))))
                 (slug (or (org-entry-get nil "LEETCODE_SLUG")
                           (and (string-match
                                 "leetcode\\.\\(?:cn\\|com\\)/problems/\\([^/?#]+\\)"
                                 raw)
                                (match-string 1 raw))))
                 (url (and (string-match
                            "\\[\\[\\(https?://[^]]*leetcode\\.\\(?:cn\\|com\\)[^]]*\\)\\]"
                            raw)
                           (match-string 1 raw))))
            (when (or (and fid (> fid 0)) slug)
              (list :fid (and fid (> fid 0) fid)
                    :slug slug
                    :title (if (string-empty-p title) raw title)
                    :url url
                    :raw raw))))
      (error nil))))

(defun leetcode-org--heading-info ()
  "Return problem information from the nearest heading that has one.
Subheadings such as \"思路\" or \"代碼\" inherit the problem from their parent."
  (save-excursion
    (condition-case nil
        (progn
          (org-back-to-heading t)
          (let ((info nil)
                (continue t))
            (while (and (not info) continue)
              (setq info (leetcode-org--heading-info-at-point))
              (unless info
                (setq continue (org-up-heading-safe))))
            info))
      (error nil))))

(defun leetcode-org--problem-heading-marker ()
  "Return a marker at the nearest heading that carries a problem id or slug."
  (save-excursion
    (condition-case nil
        (progn
          (org-back-to-heading t)
          (let ((found nil)
                (continue t))
            (while (and (not found) continue)
              (if (leetcode-org--heading-info-at-point)
                  (setq found (point-marker))
                (setq continue (org-up-heading-safe))))
            found))
      (error nil))))

(defun leetcode-org--remember-id (id)
  "Remember ID for the heading at point."
  (setq-local leetcode-org--current-id id)
  (setq-local leetcode-org--current-id-marker
              (save-excursion
                (condition-case nil (org-back-to-heading t) (error nil))
                (point-marker))))

(defun leetcode-org--remembered-id ()
  "Return the remembered id if point is still inside its heading's subtree."
  (when (and leetcode-org--current-id
             leetcode-org--current-id-marker
             (marker-position leetcode-org--current-id-marker))
    (save-excursion
      (condition-case nil
          (progn
            (org-back-to-heading t)
            (let ((found nil)
                  (continue t))
              (while (and (not found) continue)
                (when (= (point)
                         (marker-position leetcode-org--current-id-marker))
                  (setq found t))
                (setq continue (org-up-heading-safe)))
              (and found leetcode-org--current-id)))
        (error nil)))))

(defun leetcode-org--problem-info (&optional prompt)
  "Return a plist describing the problem at point.
Keys are :fid :slug :name :db-name :level :status :url.  When PROMPT is
non-nil and no id can be found, ask for one."
  (let* ((heading (leetcode-org--heading-info))
         (fid (plist-get heading :fid))
         (slug (plist-get heading :slug))
         (row nil))
    (when (and (not fid) slug)
      (setq fid (leetcode-org--fid-by-slug slug)))
    (unless fid
      (setq fid (or (leetcode-org--remembered-id)
                    (and prompt (read-number "Problem id: ")))))
    (when fid
      (setq row (leetcode-org--db-row fid))
      (let ((db-slug (nth 3 row)))
        (list :fid fid
              :slug (or slug db-slug)
              :name (or (plist-get heading :title)
                        (nth 1 row)
                        (format "Problem %d" fid))
              :db-name (nth 1 row)
              :level (nth 2 row)
              :status (nth 4 row)
              :url (or (plist-get heading :url)
                       (and (or slug db-slug)
                            (format "https://%s/problems/%s/"
                                    (leetcode-org--site-name)
                                    (or slug db-slug)))))))))

(defun leetcode-org-id ()
  "Report the problem id for the current heading, prompting if absent."
  (interactive)
  (let ((info (leetcode-org--problem-info t)))
    (leetcode-org--remember-id (plist-get info :fid))
    (message "LeetCode id: %s (%s)"
             (plist-get info :fid)
             (or (plist-get info :name) "unnamed"))
    (plist-get info :fid)))

;;; Reading the source block

(defun leetcode-org--src-block-at-point ()
  "Return the org element of the source block at point, or nil."
  (let ((ctx (org-element-context)))
    (when (eq (org-element-type ctx) 'src-block) ctx)))

(defun leetcode-org--first-src-block ()
  "Return the first source block in the current entry, or nil."
  (save-excursion
    (org-back-to-heading t)
    (let ((end (save-excursion (org-end-of-subtree t t) (point))))
      (when (re-search-forward "^[ \t]*#\\+begin_src\\(?:[ \t]+[^\n]*\\)?[ \t]*$"
                               end t)
        (goto-char (match-beginning 0))
        (org-element-at-point)))))

(defun leetcode-org--body ()
  "Return the body of the source block at point.
Fall back to the first block in the entry when point is not in a block."
  (let* ((ctx (or (leetcode-org--src-block-at-point)
                  (condition-case nil (leetcode-org--first-src-block) (error nil))))
         (body (and ctx (org-element-property :value ctx))))
    (unless (and body (not (string-blank-p body)))
      (user-error "No non-empty source block at point or in this entry"))
    body))

;;; Results parsing

(defun leetcode-org--clean-output (text)
  "Remove CLI log noise and ANSI escapes from TEXT."
  (let* ((text (replace-regexp-in-string "\x1b\\[[0-9;]*m" "" text))
         (lines (split-string text "\n")))
    (mapconcat #'identity
               (cl-remove-if
                (lambda (line)
                  (string-match-p "\\`\\[\\(?:INFO\\|DEBUG\\|TRACE\\)\\b" line))
                lines)
               "\n")))

(defun leetcode-org--clean-buffer ()
  "Remove CLI log lines and ANSI escapes from the current buffer."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\x1b\\[[0-9;]*m" nil t)
      (replace-match ""))
    (goto-char (point-min))
    (while (re-search-forward "^\\[\\(?:INFO\\|DEBUG\\|TRACE\\)\\b.*\n?" nil t)
      (replace-match ""))
    (goto-char (point-min))
    (while (re-search-forward "\n\n\n+" nil t)
      (replace-match "\n\n"))))

(defun leetcode-org--parse-result (text)
  "Parse a `leetcode test' or `leetcode exec' output.
Return a plist with :status, :runtime, :memory, :runtime-percentile and
:memory-percentile.  Values that cannot be found are nil."
  (let* ((text (leetcode-org--clean-output text))
         (status
          (cl-loop for s in '("Success" "Accepted" "Wrong Answer"
                              "Time Limit Exceeded" "Memory Limit Exceeded"
                              "Runtime Error" "Compile Error"
                              "Output Limit Exceeded" "Unknown Error")
                   when (string-match-p (concat "\\b" (regexp-quote s) "\\b")
                                        text)
                   return (if (equal s "Success") "Accepted" s)))
         (runtime
          (when (string-match
                 "Runtime:[ \t]*\\([0-9.]+\\)[ \t]*\\([^ \t,\n]+\\)" text)
            (format "%s %s" (match-string 1 text) (match-string 2 text))))
         (memory
          (when (string-match
                 "Memory Usage:[ \t]*\\([0-9.]+\\)[ \t]*\\([^ \t,\n]+\\)" text)
            (format "%s %s" (match-string 1 text) (match-string 2 text))))
         (runtime-percentile
          (when (string-match "faster than[ \t]*\\([0-9.]+\\)%" text)
            (match-string 1 text)))
         (memory-percentile
          (when (string-match "less than[ \t]*\\([0-9.]+\\)%" text)
            (match-string 1 text))))
    (list :status status
          :runtime runtime
          :memory memory
          :runtime-percentile runtime-percentile
          :memory-percentile memory-percentile)))

(defun leetcode-org--put-property (name value)
  "Set org property NAME to VALUE, or delete it when VALUE is nil."
  (if value
      (org-entry-put nil name value)
    (ignore-errors (org-entry-delete nil name))))

(defun leetcode-org--result-line (result)
  "Format RESULT as a visible org keyword line."
  (let ((parts (list (format-time-string "%Y-%m-%d %H:%M")
                     (or (plist-get result :status) "Unknown")))
        (runtime (plist-get result :runtime))
        (memory (plist-get result :memory))
        (runtime-pct (plist-get result :runtime-percentile))
        (memory-pct (plist-get result :memory-percentile)))
    (when runtime
      (setq parts
            (append parts
                    (list (if runtime-pct
                              (format "%s (%s%%)" runtime runtime-pct)
                            runtime)))))
    (when memory
      (setq parts
            (append parts
                    (list (if memory-pct
                              (format "%s (%s%%)" memory memory-pct)
                            memory)))))
    (concat "#+LEETCODE_RESULT: "
            (mapconcat #'identity parts " | "))))

(defun leetcode-org--write-result-line (result)
  "Insert or update the visible result line for RESULT at the current heading."
  (let ((line (leetcode-org--result-line result))
        (done nil))
    (org-back-to-heading t)
    (let ((end (save-excursion (org-end-of-subtree t t) (point))))
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "^[ \t]*#\\+LEETCODE_RESULT:.*$" end t)
          (replace-match line t t)
          (setq done t))))
    (unless done
      (org-end-of-meta-data)
      (unless (bolp) (insert "\n"))
      (insert line "\n"))))

(defun leetcode-org--record-submission (origin marker result)
  "Write submission RESULT into the heading at MARKER in ORIGIN."
  (when (and (buffer-live-p origin)
             marker
             (marker-position marker))
    (with-current-buffer origin
      (save-excursion
        (goto-char marker)
        (when (org-at-heading-p)
          (condition-case err
              (progn
                (org-entry-put nil "LEETCODE_STATUS"
                               (or (plist-get result :status) "Unknown"))
                (leetcode-org--put-property "LEETCODE_RUNTIME"
                                            (plist-get result :runtime))
                (leetcode-org--put-property "LEETCODE_MEMORY"
                                            (plist-get result :memory))
                (leetcode-org--put-property "LEETCODE_RUNTIME_PCT"
                                            (plist-get result :runtime-percentile))
                (leetcode-org--put-property "LEETCODE_MEMORY_PCT"
                                            (plist-get result :memory-percentile))
                (org-entry-put nil "LEETCODE_SUBMITTED"
                               (format-time-string "%Y-%m-%d %H:%M"))
                (when leetcode-org-record-result-line
                  (leetcode-org--write-result-line result))
                (ignore-errors (org-fold-show-entry))
                t)
            (error
             (message "Could not record submission stats: %s"
                      (error-message-string err))
             nil)))))))

;;; Windows and modes

(defun leetcode-org--display-side (buffer side size)
  "Display BUFFER in a side window on SIDE, sized by fraction SIZE."
  (let ((alist `((side . ,side)
                 (slot . 0)
                 (preserve-size . (t . t)))))
    (if (eq side 'right)
        (push (cons 'window-width
                    (max 40 (floor (* (frame-width) size))))
              alist)
      (push (cons 'window-height
                  (max 8 (floor (* (frame-height) size))))
            alist))
    (display-buffer-in-side-window buffer alist)))

(defvar leetcode-org-description-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "g") #'leetcode-org-refresh-description)
    (define-key map (kbd "o") #'leetcode-org-browse-problem)
    (define-key map (kbd "t") #'leetcode-org-test-from-description)
    (define-key map (kbd "s") #'leetcode-org-submit-from-description)
    map)
  "Keymap for `leetcode-org-description-mode'.")

(define-derived-mode leetcode-org-description-mode special-mode "LeetCode-Problem"
  "Major mode for a LeetCode problem statement."
  (setq-local truncate-lines nil)
  (visual-line-mode 1)
  (font-lock-add-keywords
   nil
   '(("^Example [0-9]+:" 0 'font-lock-keyword-face)
     ("^\\(Input\\|Output\\|Explanation\\|Constraints\\|Follow-up\\|Note\\):"
      0 'font-lock-keyword-face)))
  (font-lock-mode 1))

(defvar leetcode-org-results-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "g") #'leetcode-org-rerun)
    (define-key map (kbd "r") #'leetcode-org-record-result)
    map)
  "Keymap for `leetcode-org-results-mode'.")

(define-derived-mode leetcode-org-results-mode special-mode "LeetCode-Results"
  "Major mode for LeetCode CLI output."
  (setq-local truncate-lines nil)
  (font-lock-add-keywords
   nil
   '(("\\bAccepted\\b" 0 'success)
     ("\\bSuccess\\b" 0 'success)
     ("\\bWrong Answer\\b" 0 'error)
     ("\\bTime Limit Exceeded\\b" 0 'error)
     ("\\bMemory Limit Exceeded\\b" 0 'error)
     ("\\bRuntime Error\\b" 0 'error)
     ("\\bCompile Error\\b" 0 'error)
     ("\\bexit [1-9][0-9]*\\b" 0 'warning)
     ("^Runtime:.*" 0 'font-lock-keyword-face)
     ("^Memory Usage:.*" 0 'font-lock-keyword-face)))
  (font-lock-mode 1))

(defvar-local leetcode-org--description-info nil
  "Problem plist displayed in the description buffer.")
(defvar-local leetcode-org--description-id nil
  "Problem id currently displayed in the description buffer.")
(defvar-local leetcode-org--description-loading nil
  "Problem id currently being fetched, if any.")
(defvar-local leetcode-org--description-origin nil
  "Org buffer from which the description was requested.")
(defvar-local leetcode-org--description-point nil
  "Marker in the org buffer at the position that requested the description.")

(defvar-local leetcode-org--results-origin nil
  "Org buffer from which the last CLI command was run.")
(defvar-local leetcode-org--results-point nil
  "Marker in the org buffer at the position of the last CLI command.")
(defvar-local leetcode-org--results-command nil
  "Last CLI subcommand (\"test\" or \"exec\").")
(defvar-local leetcode-org--results-heading nil
  "Marker of the problem heading for the last CLI command.")

(defvar leetcode-org--current-process nil
  "The LeetCode CLI process currently running, if any.")
(defvar leetcode-org--run-generation 0
  "Generation counter used to ignore results from superseded CLI runs.")

;;; Problem description

(defun leetcode-org--clean-description (text)
  "Turn `leetcode pick' output into a plain statement body."
  (let ((text (string-trim (leetcode-org--clean-output text))))
    (setq text (replace-regexp-in-string
                "\\`\\[[0-9]+\\] [^\n]* is on the run\\.\\.\\.\n+" "" text))
    (string-trim text)))

(defun leetcode-org--render-description (buffer info output exit)
  "Render statement OUTPUT for INFO into BUFFER."
  (with-current-buffer buffer
    (let ((inhibit-read-only t)
          (fid (plist-get info :fid))
          (name (plist-get info :name))
          (db-name (plist-get info :db-name))
          (level (plist-get info :level))
          (status (plist-get info :status))
          (url (plist-get info :url)))
      (erase-buffer)
      (let* ((fid-string (number-to-string fid))
             (display-name
              (cond
               ((and name
                     (string-match
                      (format "\\`%s[.、]\\s-*" (regexp-quote fid-string))
                      name))
                (string-trim (substring name (match-end 0))))
               ((equal name fid-string) "")
               (t name))))
        (insert (propertize (format "LeetCode %s" fid) 'face 'bold))
        (when (and display-name (not (string-empty-p display-name)))
          (insert " · " display-name))
        (when (and db-name
                   (not (string-empty-p db-name))
                   (not (equal db-name display-name)))
          (insert (format "  (%s)" db-name))))
      (when level
        (insert "  " (propertize (format "[%s]" (leetcode-org--level-name level))
                                 'face 'font-lock-keyword-face)))
      (when (equal status "ac")
        (insert " " (propertize "[AC]" 'face 'success)))
      (insert "\n")
      (when url
        (insert (propertize url 'face 'link) "\n"))
      (insert (make-string 72 ?─) "\n\n")
      (let ((body (leetcode-org--clean-description output)))
        (if (and (zerop exit) (not (string-empty-p body)))
            (insert body "\n")
          (insert (propertize
                   (format "Failed to fetch the problem description (exit %s).\n\n%s\n"
                           exit output)
                   'face 'error))))
      (goto-char (point-min))
      (setq buffer-read-only t)
      (setq leetcode-org--description-id fid)
      (font-lock-ensure))))

(defun leetcode-org--fetch-description (info buffer)
  "Fetch the statement for INFO and render it into BUFFER."
  (let* ((fid (plist-get info :fid))
         (cli (leetcode-org--cli-path))
         (tmp (generate-new-buffer " *leetcode pick*"))
         (proc nil))
    (with-current-buffer buffer
      (setq leetcode-org--description-loading fid)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize (format "Loading problem %s...\n" fid)
                            'face 'shadow))))
    (condition-case err
        (setq proc
              (let ((process-environment (leetcode-org--process-environment))
                    (default-directory (expand-file-name default-directory)))
                (make-process
                 :name (format "leetcode-pick-%s" fid)
                 :buffer tmp
                 :command (list cli "-d" "pick" (number-to-string fid))
                 :connection-type 'pipe
                 :coding 'utf-8-unix
                 :noquery t
                 :sentinel
                 (lambda (p _event)
                   (when (memq (process-status p) '(exit signal))
                     (let* ((pbuf (process-buffer p))
                            (status (process-exit-status p))
                            (out (and (buffer-live-p pbuf)
                                      (with-current-buffer pbuf
                                        (buffer-string)))))
                       (when (buffer-live-p pbuf)
                         (kill-buffer pbuf))
                       (when (buffer-live-p buffer)
                         (with-current-buffer buffer
                           (when (equal leetcode-org--description-loading fid)
                             (setq leetcode-org--description-loading nil)
                             (leetcode-org--render-description
                              buffer info (or out "") status))))))))))
      (error
       (kill-buffer tmp)
       (with-current-buffer buffer
         (setq leetcode-org--description-loading nil)
         (leetcode-org--render-description
          buffer info (format "Cannot run %s: %s"
                              cli (error-message-string err))
          1))))
    (when proc
      (ignore-errors (process-send-eof proc))
      (message "Fetching LeetCode problem %s..." fid))))

(defun leetcode-org--fetch-description-safe (info buffer)
  "Like `leetcode-org--fetch-description', but show errors in BUFFER."
  (condition-case err
      (leetcode-org--fetch-description info buffer)
    (error
     (with-current-buffer buffer
       (setq leetcode-org--description-loading nil)
       (leetcode-org--render-description
        buffer info (error-message-string err) 1)))))

;;;###autoload
(defun leetcode-org-show-problem (&optional force)
  "Show the current problem statement in a side window.
With prefix argument FORCE, fetch it again even when it is already shown."
  (interactive "P")
  (let* ((info (leetcode-org--problem-info t))
         (fid (plist-get info :fid))
         (origin (current-buffer))
         (point-marker (point-marker))
         (buffer (get-buffer-create leetcode-org-description-buffer)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'leetcode-org-description-mode)
        (leetcode-org-description-mode))
      (setq-local leetcode-org--description-info info)
      (setq-local leetcode-org--description-origin origin)
      (setq-local leetcode-org--description-point point-marker))
    (leetcode-org--display-side buffer
                               leetcode-org-description-side
                               leetcode-org-description-width)
    (let ((shown-id (buffer-local-value 'leetcode-org--description-id buffer))
          (loading (buffer-local-value 'leetcode-org--description-loading buffer)))
      (cond
       ((equal fid loading)
        (message "Still fetching LeetCode problem %s..." fid))
       ((and (not force) (equal fid shown-id))
        (message "LeetCode problem %s is already shown" fid))
       (t
        (leetcode-org--fetch-description-safe info buffer))))))

(defun leetcode-org-refresh-description ()
  "Fetch the currently displayed problem statement again."
  (interactive)
  (let ((info leetcode-org--description-info)
        (buffer (current-buffer)))
    (unless info
      (user-error "No problem description is displayed"))
    (if leetcode-org--description-loading
        (message "Still fetching LeetCode problem %s..."
                 leetcode-org--description-loading)
      (leetcode-org--fetch-description-safe info buffer))))

(defun leetcode-org-browse-problem ()
  "Open the current problem page in the browser."
  (interactive)
  (let* ((info (or leetcode-org--description-info
                   (leetcode-org--problem-info t)))
         (url (plist-get info :url)))
    (unless url
      (user-error "No LeetCode URL for this problem"))
    (browse-url url)))

(defun leetcode-org--run-from-description (fn)
  "Run FN after returning to the org buffer that requested the description."
  (let ((origin leetcode-org--description-origin)
        (marker leetcode-org--description-point))
    (unless (buffer-live-p origin)
      (user-error "The org buffer for this problem is gone"))
    (pop-to-buffer origin)
    (goto-char (if (and marker (marker-position marker))
                   marker
                 (point-min)))
    (call-interactively fn)))

(defun leetcode-org-test-from-description ()
  "Run `leetcode-org-test' from the org buffer that requested the description."
  (interactive)
  (leetcode-org--run-from-description #'leetcode-org-test))

(defun leetcode-org-submit-from-description ()
  "Run `leetcode-org-submit' from the org buffer that requested the description."
  (interactive)
  (leetcode-org--run-from-description #'leetcode-org-submit))

;;; Running the CLI

(defun leetcode-org--finish (proc subcommand fid origin marker generation)
  "Handle the end of a CLI process PROC from GENERATION."
  (when (= generation leetcode-org--run-generation)
    (setq leetcode-org--current-process nil)
    (let* ((output-buffer (process-buffer proc))
           (status (process-exit-status proc))
           (output nil)
           (result nil))
      (when (buffer-live-p output-buffer)
        (with-current-buffer output-buffer
          (let ((inhibit-read-only t))
            (leetcode-org--clean-buffer)
            (goto-char (point-max))
            (insert (format "\n[exit %s]\n" status))
            (setq output (buffer-string)))
          (setq buffer-read-only t)
          (font-lock-ensure))
        (setq result (leetcode-org--parse-result output))
        (when (and (equal subcommand "exec")
                   leetcode-org-record-submission
                   (plist-get result :status))
          (leetcode-org--record-submission origin marker result))
        (let ((runtime (plist-get result :runtime))
              (memory (plist-get result :memory)))
          (message "LeetCode %s %s: %s%s%s"
                   subcommand fid
                   (or (plist-get result :status)
                       (if (zerop status)
                           "no result (see *leetcode*)"
                         (format "exit %s" status)))
                   (if runtime (format " · %s" runtime) "")
                   (if memory (format " · %s" memory) "")))))))

(defun leetcode-org--run (subcommand &optional problem-id)
  "Run the LeetCode CLI SUBCOMMAND on the block at point.
SUBCOMMAND is \"test\" or \"exec\".  PROBLEM-ID, when non-nil, avoids
looking the id up again (used by `leetcode-org-submit')."
  (when (process-live-p leetcode-org--current-process)
    (user-error "A LeetCode command is still running"))
  (let* ((info (unless problem-id (leetcode-org--problem-info t)))
         (fid (or problem-id (plist-get info :fid)))
         (body (leetcode-org--body))
         (origin (current-buffer))
         (point-marker (point-marker))
         (heading-marker
          (or (leetcode-org--problem-heading-marker)
              (and (leetcode-org--remembered-id)
                   leetcode-org--current-id-marker)
              (save-excursion
                (condition-case nil
                    (org-back-to-heading t)
                  (error nil))
                (point-marker))))
         (code-file (leetcode-org--resolve-code-path fid))
         (generation (cl-incf leetcode-org--run-generation))
         (buffer (get-buffer-create leetcode-org-results-buffer))
         (proc nil))
    (condition-case err
        (leetcode-org--write-code code-file body)
      (error
       (user-error "Cannot write solution file %s: %s"
                   code-file (error-message-string err))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'leetcode-org-results-mode)
        (leetcode-org-results-mode))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize
                 (format "── leetcode %s %s · %s ──\n\n"
                         subcommand fid
                         (format-time-string "%Y-%m-%d %H:%M:%S"))
                 'face 'bold)))
      (setq buffer-read-only nil)
      (setq-local leetcode-org--results-origin origin)
      (setq-local leetcode-org--results-point point-marker)
      (setq-local leetcode-org--results-command subcommand)
      (setq-local leetcode-org--results-heading heading-marker))
    (leetcode-org--display-side buffer
                                leetcode-org-results-side
                                leetcode-org-results-height)
    (condition-case err
        (setq proc
              (let ((process-environment (leetcode-org--process-environment))
                    (default-directory (expand-file-name default-directory)))
                (make-process
                 :name (format "leetcode-%s-%s" subcommand fid)
                 :buffer buffer
                 :command (list (leetcode-org--cli-path)
                                "-d" subcommand (number-to-string fid))
                 :connection-type 'pipe
                 :coding 'utf-8-unix
                 :noquery t
                 :sentinel
                 (lambda (p _event)
                   (when (memq (process-status p) '(exit signal))
                     (leetcode-org--finish p subcommand fid origin
                                         heading-marker generation))))))
      (when (process-live-p proc)
        (setq leetcode-org--current-process proc))
      (error
       (with-current-buffer buffer
         (let ((inhibit-read-only t))
           (goto-char (point-max))
           (insert (format "\n[leetcode error: %s]\n"
                           (error-message-string err))))
         (setq buffer-read-only t))
       (user-error "Cannot start LeetCode CLI: %s" (error-message-string err))))
    (message "Running leetcode %s %s..." subcommand fid)))

(defun leetcode-org-rerun ()
  "Run the last LeetCode command again from its original position."
  (interactive)
  (let ((origin leetcode-org--results-origin)
        (point-marker leetcode-org--results-point)
        (command leetcode-org--results-command))
    (unless (and (buffer-live-p origin) command)
      (user-error "Nothing to rerun"))
    (pop-to-buffer origin)
    (goto-char (if (and point-marker (marker-position point-marker))
                   point-marker
                 (point-min)))
    (if (equal command "exec")
        (call-interactively #'leetcode-org-submit)
      (leetcode-org--run command))))

;;;###autoload
(defun leetcode-org-record-result ()
  "Record the result currently shown in the results buffer.
This is useful when a submission was made before the recording code was
loaded, or when the automatic write-back was skipped."
  (interactive)
  (let ((buffer (get-buffer leetcode-org-results-buffer)))
    (unless buffer
      (user-error "No LeetCode results buffer"))
    (with-current-buffer buffer
      (unless (equal leetcode-org--results-command "exec")
        (user-error "The last LeetCode command was not a submission"))
      (unless (and (buffer-live-p leetcode-org--results-origin)
                   leetcode-org--results-heading
                   (marker-position leetcode-org--results-heading))
        (user-error "No org heading is associated with this result"))
      (let ((result (leetcode-org--parse-result (buffer-string))))
        (unless (plist-get result :status)
          (user-error "Could not parse a submission result from the buffer"))
        (if (leetcode-org--record-submission
             leetcode-org--results-origin
             leetcode-org--results-heading
             result)
            (message "Recorded: %s" (leetcode-org--result-line result))
          (user-error "Could not record the submission result"))))))

(defun leetcode-org-test ()
  "Send the current solution block to `leetcode test'."
  (interactive)
  (leetcode-org--run "test"))

;;;###autoload
(defun leetcode-org-submit ()
  "Send the current solution block to `leetcode exec' (submit)."
  (interactive)
  (let* ((info (leetcode-org--problem-info t))
         (fid (plist-get info :fid)))
    (leetcode-org--remember-id fid)
    (when (or (not leetcode-org-confirm-submit)
              (not (called-interactively-p 'any))
              (y-or-n-p (format "Submit problem %s to LeetCode? " fid)))
      (leetcode-org--run "exec" fid))))

(defun leetcode-org-quit-windows ()
  "Close the LeetCode description and result windows."
  (interactive)
  (dolist (name (list leetcode-org-description-buffer
                      leetcode-org-results-buffer))
    (let ((buffer (get-buffer name)))
      (when buffer
        (dolist (window (get-buffer-window-list buffer nil t))
          (quit-window nil window))))))

;;; Picking and inserting problems

;;;###autoload
(defun leetcode-org-insert-problem (id)
  "Insert problem ID as an org heading with an empty source block.
The statement itself is shown in the description side window, not inserted."
  (interactive "nProblem id: ")
  (let* ((row (leetcode-org--db-row id))
         (name (or (nth 1 row) (format "Problem %d" id)))
         (slug (nth 3 row)))
    (unless row
      (user-error "Problem %d is not in the cache (run `leetcode data -u')" id))
    (save-excursion
      (if (org-before-first-heading-p)
          (goto-char (point-max))
        (org-end-of-subtree t t))
      (unless (bolp) (insert "\n"))
      (insert (format "\n* %d. %s\n:PROPERTIES:\n:LEETCODE_ID: %d\n"
                      id name id))
      (when slug
        (insert (format ":LEETCODE_SLUG: %s\n" slug)))
      (insert ":END:\n\n#+BEGIN_SRC scheme\n\n#+END_SRC\n")
      (search-backward "#+BEGIN_SRC scheme")
      (forward-line 1))
    (leetcode-org--remember-id id)
    (leetcode-org-show-problem t)
    (message "Inserted problem %d" id)))

(defun leetcode-org--candidates (&optional query level)
  "Return completion candidates for QUERY as (LABEL . FID) conses.
QUERY is a substring of the name or slug; LEVEL filters 1/2/3."
  (mapcar (lambda (row)
            (let ((fid (nth 0 row)))
              (cons (format "[%d] %s" fid (nth 2 row)) fid)))
          (leetcode-org-list-problems level query)))

;;;###autoload
(defun leetcode-org-choose (&optional query)
  "Choose a cached problem and insert its heading.
QUERY narrows by substring of the name or slug."
  (interactive)
  (let* ((query (or query (read-string "Problem name (empty for all): ")))
         (candidates (leetcode-org--candidates query))
         (collection (mapcar #'car candidates)))
    (unless collection
      (user-error "No problem matching %s" query))
    (let* ((choice (completing-read "Problem: " collection nil t))
           (fid (cdr (assoc choice candidates))))
      (leetcode-org-insert-problem fid))))

;;; Imenu

(defun leetcode-org-imenu-index ()
  "Build an imenu index grouping cached problems by difficulty."
  (or leetcode-org--imenu-cache
      (setq leetcode-org--imenu-cache
            (let (index)
              (dolist (level '(3 2 1))
                (let ((entries
                       (mapcar (lambda (row)
                                 (let ((fid (nth 0 row))
                                       (name (nth 2 row))
                                       (status (nth 3 row)))
                                   (list (format "[%d] %s%s"
                                                 fid name
                                                 (if (equal status "ac") " ✓" ""))
                                         t
                                         (lambda (_name _pos id)
                                           (leetcode-org-insert-problem id))
                                         fid)))
                               (leetcode-org-list-problems level))))
                  (when entries
                    (push (cons (format "%s (%d)"
                                        (leetcode-org--level-name level)
                                        (length entries))
                                entries)
                          index))))
              (nreverse index)))))

(defun leetcode-org-refresh-index ()
  "Drop the cached problem index and reopen the cache."
  (interactive)
  (setq leetcode-org--imenu-cache nil)
  (when leetcode-org--db-handle
    (ignore-errors (sqlite-close leetcode-org--db-handle))
    (setq leetcode-org--db-handle nil))
  (message "LeetCode index cache cleared"))

;;; Minor mode

(defvar leetcode-org-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map "d" #'leetcode-org-show-problem)
    (define-key map "t" #'leetcode-org-test)
    (define-key map "s" #'leetcode-org-submit)
    (define-key map "o" #'leetcode-org-browse-problem)
    (define-key map "i" #'leetcode-org-id)
    (define-key map "p" #'leetcode-org-insert-problem)
    (define-key map "f" #'leetcode-org-choose)
    (define-key map "m" #'imenu)
    (define-key map "q" #'leetcode-org-quit-windows)
    (define-key map "r" #'leetcode-org-record-result)
    map)
  "Prefix map bound to C-c l by `leetcode-org-mode'.")

(define-minor-mode leetcode-org-mode
  "LeetCode workflow keybindings for org buffers."
  :lighter " LC"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c l") leetcode-org-command-map)
            map))

(defun leetcode-org--project-org-buffer-p (&optional buffer)
  "Return non-nil when BUFFER is an org file inside this LeetCode project."
  (with-current-buffer (or buffer (current-buffer))
    (and buffer-file-name
         (derived-mode-p 'org-mode)
         (locate-dominating-file buffer-file-name "helper/leetcode-org.el"))))

(defun leetcode-org--org-mode-setup ()
  "Set up LeetCode integration in a project org buffer."
  (when (leetcode-org--project-org-buffer-p)
    (leetcode-org-mode 1)
    (setq-local imenu-create-index-function #'leetcode-org-imenu-index)))

(defun leetcode-org-enable-existing-buffers ()
  "Enable `leetcode-org-mode' in already-open project org buffers."
  (interactive)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and (leetcode-org--project-org-buffer-p)
                 (not leetcode-org-mode))
        (leetcode-org-mode 1)
        (setq-local imenu-create-index-function #'leetcode-org-imenu-index)))))

(add-hook 'org-mode-hook #'leetcode-org--org-mode-setup)
(leetcode-org-enable-existing-buffers)

(provide 'leetcode-org)
;;; leetcode-org.el ends here
