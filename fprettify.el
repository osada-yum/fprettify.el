;;; package --- Summary
;;; Commentary:

;;; Code:
(require 'f90)

(defvar fprettify-executable "fprettify")

(defvar fprettify-diff "")

(defmacro args-format (str var)
  "If `VAR' is nil then return nil, else return `STR' `VAR'."
  `(when ,var
     (if (eq ,var t)
	 (format " %s True" ,str)
       (format   " %s %d"   ,str ,var))))
(defun fprettify-args ()
  "Create args."
  (concat
   (args-format "-i"  f90-program-indent)
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
	  (fpe-stdout-buf (get-buffer-create "*fprettify*")))
      (shell-command (format "%s <<< '%s'"
			     (fprettify-command)
			     (buffer-substring-no-properties (point-min) (point-max)))
		     fpe-stdout-buf fpe-stdout-buf)
      (with-current-buffer fpe-stdout-buf
	(goto-char (point-max))
	(backward-page)
	(forward-char)
	(message (buffer-substring-no-properties (point) (point-max)))
	(backward-page)
	(forward-line -1)
	(beginning-of-line)
	(delete-region (point) (point-max)))
      (replace-buffer-contents fpe-stdout-buf))))

(provide 'fprettify)
;;; fprettify.el ends here
