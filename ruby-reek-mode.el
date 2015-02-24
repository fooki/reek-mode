;; Current version is heavily inspired by the flymake mode:
;; https://github.com/illusori/emacs-flymake

(defface ruby-reek-warning-face
  '((((class color) (background dark)) (:background "red"))
    (((class color) (background light)) (:background "red")))
  "Used for marking reek warnings.")

(make-variable-buffer-local
 (defvar ruby-reek-warnings (make-hash-table)
   "Stores current warnings"))


(defun ruby-reek-run-command ()
  (let ((cmd-string (format "reek --no-color -s %s" (buffer-file-name))))
    (shell-command-to-string cmd-string)))

(defun smell-code ()
  (interactive)
  (let* ((raw-results (ruby-reek-run-command))
         (warnings (ruby-reek-parse-warnings raw-results)))
    (setq ruby-reek-warnings warnings)
    (ruby-reek-update-overlays warnings)))

(defun ruby-reek-update-overlays (warnings)
  (ruby-reek-delete-overlays)
  (maphash '(lambda (linum _) (ruby-reek-add-overlay linum))
           warnings))

(defun ruby-reek-add-overlay (line-number)
  (let ((saved-point (point)))
    (goto-char (point-min))
    (forward-line (1- line-number))
    (let* ((face 'ruby-reek-warning-face)
           (beg (point-at-bol))
           (end (point-at-eol))
           (overlay (make-overlay beg end)))
      (goto-char saved-point)
      (overlay-put overlay 'face 'ruby-reek-warning-face)
      (overlay-put overlay 'priority 142)
      (overlay-put overlay 'ruby-reek-smelly t))))

(defun ruby-reek-delete-overlays ()
  (dolist (overlay (overlays-in (point-min) (point-max)))
    (when (overlay-get overlay 'ruby-reek-smelly)
      (delete-overlay overlay))))

(defun ruby-reek-update-warning-message ()
  (let ((warning (gethash (line-number-at-pos) reek-warnings)))
    (when warning
      (message warning))))

(defun ruby-reek-clear-warnings ()
  (clrhash ruby-reek-warnings))

;;;;;; Begin nasty parsing sorcery ;;;;;;

(defun ruby-reek-parse-warnings (str-warnings)

  (defun ruby-reek-chomp (str)
    "From http://emacswiki.org/emacs/ElispCookbook"
    (replace-regexp-in-string (rx (or (: bos (* (any " \t\n")))
                                      (: (* (any " \t\n")) eos)))
                              ""
                              str))

  (defun ruby-reek-remove-total-prefix (str-warnings)
    "First line of reek output is a summary, removed in this function"
    (apply 'concat (rest (split-string str-warnings "\n"))))

  (defun ruby-reek-parse-warning (str-warning)
    "Parse a single reek output warning line. Returns a list
consisting of the line number and warning text"
    (let* ((chomped-warning (ruby-reek-chomp str-warning))
           (line-num-length (string-match "[^0-9]" chomped-warning))
           (line-num (string-to-number (substring chomped-warning 0 line-num-length))))
      (list line-num
            (substring chomped-warning (1+ line-num-length)
                       (length chomped-warning)))))

  (let* ((reek-warnings (make-hash-table))
         (chomped (chomp str-warnings))
         (warnings (ruby-reek-remove-total-prefix chomped))
         (delimiter (format "%s:" (buffer-file-name))))
     (dolist (line (rest (split-string warnings delimiter)))
      (let ((parsed-line (ruby-reek-parse-warning line)))
        (puthash (first parsed-line)
                 (second parsed-line)
                 reek-warnings)))
    reek-warnings))

;;;;;; End nasty parsing sorcery ;;;;;;

(define-minor-mode ruby-reek-mode
  "reek mode for emacs"
  :keymap nil
  :lighter " reek"
  (let ((else t))
    (cond
     (ruby-reek-mode
      (cond ((not buffer-file-name) (message "requires buffer with file name"))
            (else (ruby-reek-add-hooks)
                  (ruby-reek-clear-warnings))))
     (else (ruby-reek-remove-hooks)
           (ruby-reek-clear-warnings)
           (ruby-reek-delete-overlays)))))

(defun ruby-reek-add-hooks ()
  (add-hook 'post-command-hook 'update-warning-message nil 'local)
  (add-hook 'after-save-hook 'smell-code nil t)
  (add-hook 'after-change-functions '(lambda (x y z) ((delete-overlays))) nil t)
  (add-hook 'after-change-functions '(lambda (x y z) ((clear-warnings))) nil t))

(defun ruby-reek-remove-hooks ()
  (remove-hook 'post-command-hook 'update-warning-message t)
  (remove-hook 'after-change-functions '(lambda (x y z) (delete-overlays)) t)
  (remove-hook 'after-change-functions '(lambda (x y z) (clear-warnings)) t)
  (remove-hook 'after-save-hook 'smell-code t))

(provide 'ruby-reek-mode)
