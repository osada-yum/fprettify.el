;;; package --- Summary
;;; Commentary:

;;; Code:
(require 'f90)

(defvar fprettify-executable "fprettify")

(defvar fprettify-tmp-file (make-temp-file "fprettify-tmp" nil ".f90"))

(defmacro fprettify-args-format (str var)
  "If `VAR' is nil then return nil, else return `STR' `VAR'."
  `(when ,var
     (if (eq t ,var)
	 (format " %s"   ,str)
       (format " %s %s"   ,str ,var))))
(defun fprettify-args ()
  "Create args."
  (concat
   (fprettify-args-format "-s"  t)
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
      (write-region (point-min) (point-max) fprettify-tmp-file)
      (shell-command (format "%s %s"
                             (fprettify-command)
                             fprettify-tmp-file)
		     fpe-stdout-buf fpe-stderr-buf)
      (with-current-buffer fpe-stderr-buf
	(when (> (point-max) (point-min)) ;; Buffer is not empty.
	  (goto-char (point-max))
	  (backward-page)
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
