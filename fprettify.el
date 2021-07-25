;;; package --- Summary
;;; Commentary:

;;; Code:
(require 'f90)

(defvar fprettify-executable "fprettify")

(defvar fprettify-diff "")

(defmacro args-format (str var)
  "If `VAR' is nil then return nil, else return `STR' `VAR'."
  `(when ,var
     (if (eq var t)
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
    (let ((fpe-buf  (get-buffer-create "*fprettify*")))
      (shell-command (format "%s <<< '%s'"
			     (fprettify-command)
			     (buffer-substring-no-properties (point-min) (point-max)))
		     fpe-buf)
      (replace-buffer-contents fpe-buf))))

(provide 'fprettify)
;;; fprettify.el ends here
