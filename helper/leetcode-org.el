;;; leetcode-org.el --- LeetCode workflow from org-mode -*- lexical-binding: t -*-

;;; Commentary:

;; Drives the `leetcode' CLI from an org buffer. The org source block is the
;; single source of truth: its body is piped to a small wrapper script
;; (helper/leetcode) which writes it into the CLI's solution file and then
;; runs `test' or `exec' against it.

;;; Code:

(require 'subr-x)
(require 'org)
(require 'ob-core)

(defgroup leetcode-org nil
  "LeetCode workflow from org-mode."
  :group 'org)

(defcustom leetcode-org-helper
  (expand-file-name "helper/leetcode"
                    (or (and (boundp 'leetcode-org-project-root)
                             leetcode-org-project-root)
                        default-directory))
  "Path to the wrapper script that forwards stdin to the leetcode CLI."
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

;;;###autoload
(defun leetcode-org-pick (id)
  "Show problem ID description, inserting it under the current heading."
  (interactive "nProblem id: ")
  (let* ((default-directory (file-name-directory (buffer-file-name)))
         (out (with-temp-buffer
                (call-process "env" nil t nil "NO_COLOR=1" "leetcode" "pick"
                              (number-to-string id))
                (buffer-string))))
    (setq leetcode-org--current-id id)
    (save-excursion
      (org-end-of-subtree t t)
      (insert "\n" (string-trim out) "\n"))
    (message "Inserted problem %s" id)))

(defvar leetcode-org-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c l t") #'leetcode-org-test)
    (define-key map (kbd "C-c l s") #'leetcode-org-submit)
    (define-key map (kbd "C-c l i") #'leetcode-org-id)
    (define-key map (kbd "C-c l p") #'leetcode-org-pick)
    map)
  "Keymap for LeetCode workflow commands.")

(defun leetcode-org-install-keys ()
  "Bind `leetcode-org-mode-map' in the current buffer."
  (use-local-map (make-composed-keymap leetcode-org-mode-map (current-local-map))))

(add-hook 'org-mode-hook #'leetcode-org-install-keys)

(provide 'leetcode-org)
;;; leetcode-org.el ends here
