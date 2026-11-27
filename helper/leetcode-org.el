;;; leetcode-org.el --- LeetCode workflow from org-mode -*- lexical-binding: t -*-

;;; Commentary:

;; Drives the `leetcode' CLI from org buffers. The source block body is piped to
;; helper/leetcode, which writes it to the CLI's solution file and runs `test'
;; or `exec'.

;;; Code:

(require 'subr-x)
(require 'org)
(require 'ob-core)

(defgroup leetcode-org nil
  "LeetCode workflow from org-mode."
  :group 'org)

(defcustom leetcode-org-helper "helper/leetcode"
  "Wrapper script that forwards stdin to the leetcode CLI."
  :type 'string)

(defcustom leetcode-org-cli "leetcode"
  "Override the CLI binary used by the wrapper, or nil for its default."
  :type '(choice (const nil) string))

(defcustom leetcode-org-results-buffer "*leetcode*"
  "Buffer name used for CLI output."
  :type 'string)

(defvar leetcode-org--current-id nil
  "Problem id associated with the current org buffer.")

(defun leetcode-org--helper-path ()
  "Return the wrapper script path, resolving it relative to the current file."
  (or (and (file-executable-p leetcode-org-helper) leetcode-org-helper)
      (let ((from-buffer (expand-file-name
                          "helper/leetcode"
                          (file-name-directory (buffer-file-name)))))
        (when (file-executable-p from-buffer)
          from-buffer))
      (user-error "Cannot find helper/leetcode (set `leetcode-org-helper')")))

(defun leetcode-org--id-from-heading ()
  "Extract a problem id from the current org heading, e.g. \"* 3619. ...\"."
  (save-excursion
    (org-back-to-heading t)
    (let ((title (org-get-heading t t t t)))
      (if (string-match "\\([0-9]+\\)\\." title)
          (string-to-number (match-string 1 title))
        (user-error "No problem id in heading: %s" title)))))

(defun leetcode-org-id ()
  "Return the problem id for the current heading, prompting if absent."
  (interactive)
  (setq leetcode-org--current-id
        (condition-case nil
            (leetcode-org--id-from-heading)
          (error (read-number "Problem id: "))))
  (message "LeetCode id: %s" leetcode-org--current-id)
  leetcode-org--current-id)

(defun leetcode-org--call (id subcommand body)
  "Run the wrapper for ID with SUBCOMMAND, feeding BODY on stdin."
  (let ((buffer (get-buffer-create leetcode-org-results-buffer))
        (default-directory (file-name-directory (buffer-file-name)))
        (process-environment
         (if leetcode-org-cli
             (cons (format "LEETCODE_CLI=%s" leetcode-org-cli) process-environment)
           process-environment)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "$ leetcode %s %s\n\n" subcommand id))))
    (let ((proc (make-process
                 :name "leetcode"
                 :buffer buffer
                 :command (list (leetcode-org--helper-path) (number-to-string id) subcommand)
                 :connection-type 'pipe
                 :coding 'utf-8-unix
                 :sentinel
                 (lambda (p _event)
                   (when (memq (process-status p) '(exit signal))
                     (with-current-buffer (process-buffer p)
                       (let ((inhibit-read-only t))
                         (goto-char (point-max))
                         (insert (format "\n[process %s]"
                                         (process-exit-status p))))))))))
      (process-send-string proc (concat body "\n"))
      (process-send-eof proc))
    (display-buffer buffer)
    (message "Sent %s to leetcode %s" id subcommand)))

(defun leetcode-org--body ()
  "Return the body of the org source block at point, without header args."
  (org-babel-get-src-block-info) ; ensure we are in a block
  (let* ((info (org-babel-get-src-block-info))
         (body (nth 1 info)))
    (if (stringp body) body (error "Could not read source block body"))))

;;;###autoload
(defun leetcode-org-test ()
  "Send the current solution block to `leetcode test'."
  (interactive)
  (let* ((id (or leetcode-org--current-id (leetcode-org-id)))
         (body (leetcode-org--body)))
    (leetcode-org--call id "test" body)))

;;;###autoload
(defun leetcode-org-submit ()
  "Send the current solution block to `leetcode exec'."
  (interactive)
  (let* ((id (or leetcode-org--current-id (leetcode-org-id)))
         (body (leetcode-org--body)))
    (when (or (not (called-interactively-p 'any))
              (y-or-n-p (format "Submit %s to LeetCode? " id)))
      (leetcode-org--call id "exec" body))))

(defcustom leetcode-org-db
  (expand-file-name "~/.leetcode/Problems")
  "SQLite problem cache written by the leetcode CLI."
  :type 'string)

(defcustom leetcode-org-imenu-limit 300
  "Maximum number of problems listed per level in the imenu index."
  :type 'integer)

(defun leetcode-org--db ()
  "Open the problem cache database."
  (unless (file-readable-p leetcode-org-db)
    (user-error "Problem cache not found: %s (run `leetcode data -u')" leetcode-org-db))
  (sqlite-open leetcode-org-db))

(defun leetcode-org--query (sql &optional params)
  "Run SQL with PARAMS against the problem cache."
  (if params
      (sqlite-select (leetcode-org--db) sql params)
    (sqlite-select (leetcode-org--db) sql)))

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
      (setq sql (concat sql " and name like ?"))
      (push (concat "%" query "%") params))
    (setq sql (concat sql " order by fid, id limit ?"))
    (push (or limit leetcode-org-imenu-limit) params)
    (leetcode-org--query sql (nreverse params))))

(defun leetcode-org--level-name (level)
  "Human readable name for LEVEL."
  (pcase level
    (1 "Easy")
    (2 "Medium")
    (3 "Hard")
    (_ "Other")))

(defun leetcode-org--pick-by-name (name _position &rest _args)
  "Imenu target: insert problem NAME's heading and description."
  (when (string-match "\\`\\[\\([0-9]+\\)\\] \\(.*\\)\\'" name)
    (leetcode-org-insert-problem (string-to-number (match-string 1 name)))))

(defvar leetcode-org--imenu-cache nil
  "Cached imenu index for the problem list.")

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
                                   (list (format "[%d] %s%s" fid name
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
  "Drop the cached problem index so it is rebuilt on next use."
  (interactive)
  (setq leetcode-org--imenu-cache nil)
  (message "LeetCode index cache cleared"))

(defun leetcode-org-setup-imenu ()
  "Install the LeetCode problem index when the buffer is in this project."
  (when (and buffer-file-name
             (locate-dominating-file buffer-file-name "helper/leetcode"))
    (setq-local imenu-create-index-function #'leetcode-org-imenu-index)))

(add-hook 'org-mode-hook #'leetcode-org-setup-imenu)

;;;###autoload
(defun leetcode-org-insert-problem (id)
  "Insert problem ID as a heading with its description underneath."
  (interactive "nProblem id: ")
  (let* ((default-directory (or (and (buffer-file-name)
                                     (file-name-directory (buffer-file-name)))
                                default-directory))
         (out (with-temp-buffer
                (call-process "env" nil t nil "NO_COLOR=1" "leetcode" "pick"
                              (number-to-string id))
                (buffer-string)))
         (title (if (and (string-match "^\n?\\[\\([0-9]+\\)\\] \\(.*?\\) is on the run" out)
                         (match-string 2 out))
                    (match-string 2 out)
                  (format "Problem %d" id)))
         (body (if (string-match "^.*is on the run\\.\\.\\.\n" out)
                   (string-trim (substring out (match-end 0)))
                 (string-trim out))))
    (setq leetcode-org--current-id id)
    (save-excursion
      (org-end-of-subtree t t)
      (unless (bolp) (insert "\n"))
      (insert (format "\n* %d. %s\n\n%s\n\n#+BEGIN_SRC scheme\n\n#+END_SRC\n"
                      id title body))
      (search-backward "#+BEGIN_SRC scheme")
      (forward-line 1))
    (message "Inserted problem %d" id)))

(defun leetcode-org--candidates (&optional query level)
  "Return completion candidates for QUERY as (LABEL . FID) conses.
QUERY is a substring of the name, English or Chinese; LEVEL filters 1/2/3.
Names come from `leetcode-org-list-problems', so they already prefer Chinese."
  (mapcar (lambda (row)
            (let ((fid (nth 0 row)))
              (cons (format "[%d] %s" fid (nth 2 row)) fid)))
          (leetcode-org-list-problems level query)))

(defun leetcode-org--level-candidates (level)
  "Return candidate labels for problems of difficulty LEVEL."
  (mapcar #'car (leetcode-org--candidates nil level)))

;;;###autoload
(defun leetcode-org-choose (&optional query)
  "Choose a cached problem and insert it.
QUERY narrows by substring of the name, English or Chinese; filtering happens
before the prompt, so it works regardless of completion style."
  (interactive)
  (let* ((query (or query (read-string "Problem name (empty for all): ")))
         (candidates (leetcode-org--candidates query))
         (collection (mapcar #'car candidates)))
    (unless collection
      (user-error "No problem matching %s" query))
    (let* ((choice (completing-read "Problem: " collection nil t))
           (id (cdr (assoc choice candidates))))
      (leetcode-org-insert-problem id))))

(defvar leetcode-org-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map "t" #'leetcode-org-test)
    (define-key map "s" #'leetcode-org-submit)
    (define-key map "i" #'leetcode-org-id)
    (define-key map "p" #'leetcode-org-insert-problem)
    (define-key map "f" #'leetcode-org-choose)
    (define-key map "m" #'imenu)
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
         (locate-dominating-file buffer-file-name "helper/leetcode"))))

(defun leetcode-org--maybe-enable ()
  "Turn on `leetcode-org-mode' in project org buffers."
  (when (leetcode-org--project-org-buffer-p)
    (leetcode-org-mode 1)))

(defun leetcode-org-enable-existing-buffers ()
  "Enable `leetcode-org-mode' in already-open project org buffers."
  (interactive)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and (leetcode-org--project-org-buffer-p)
                 (not leetcode-org-mode))
        (leetcode-org-mode 1)))))

(add-hook 'org-mode-hook #'leetcode-org--maybe-enable)
(leetcode-org-enable-existing-buffers)

(provide 'leetcode-org)
;;; leetcode-org.el ends here
