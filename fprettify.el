;;; package --- Summary
;;; Commentary:

;;; Code:
(require 'f90)

(defvar fprettify-executable "fprettify")


(defmacro fprettify-args-format (str var)
  "If `VAR' is nil then return nil, else return `STR' `VAR'."
  `(when ,var
     (if (eq ,var t)
	 (format " %s True" ,str)
       (format   " %s %d"   ,str ,var))))
(defun fprettify-args ()
  "Create args."
  (concat
   (fprettify-args-format "-i"  f90-program-indent)
   ))
(defun fprettify-command ()
  "Create command."
  (format "%s %s"
	  fprettify-executable
	  (fprettify-args)))

(defun fprettify-run ()
  "Run `fprettify' with string in `current-buffer'."
  (interactive)
  (save-excursion
    (let ((cur-buf        (current-buffer))
	  (fpe-stdout-buf (get-buffer-create "*fprettify*"))
	  (fpe-stderr-buf (get-buffer-create "*fprettify<stderr>*")))
      (shell-command (format "bash -c %s <<< '%s'"
			     (fprettify-command)
			     (buffer-substring-no-properties (point-min) (point-max)))
		     fpe-stdout-buf fpe-stderr-buf)
      (with-current-buffer fpe-stderr-buf
	(when (> (point-max) (point-min)) ;; Buffer is not empty.
	  (goto-char (point-max))
	  (backward-page)
	  (forward-char)
	  (message (buffer-substring-no-properties (point) (point-max)))
	  (delete-region (point-min) (point-max))))
      (replace-buffer-contents fpe-stdout-buf))))

(add-hook 'f90-mode-hook
	  #'(lambda()
	      (add-hook 'before-save-hook
			#'fprettify-run
			nil t)))

(provide 'fprettify)
;;; fprettify.el ends here
