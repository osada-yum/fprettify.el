;;; package --- Summary
;;; Commentary:

;;; Code:
(require 'f90)

(defvar fprettify-executable "fprettify")

(defvar fprettify-whitespace-style 2)

(defvar fprettify-whitespace-comma      t)
(defvar fprettify-whitespace-assignment t)
(defvar fprettify-whitespace-decl       t)
(defvar fprettify-whitespace-relational t)
(defvar fprettify-whitespace-logical    t)
(defvar fprettify-whitespace-plusminus  t)
(defvar fprettify-whitespace-multdiv    t)
(defvar fprettify-whitespace-print      t)
(defvar fprettify-whitespace-type       nil)
(defvar fprettify-whitespace-intrinsics t)

(defvar fprettify-strict-indent       nil)
(defvar fprettify-enable-decl         nil)
(defvar fprettify-disable-indent      nil)
(defvar fprettify-disable-whitespace  nil)
(defvar fprettify-enable-replacements t)
(defvar fprettify-c-relations         t)
(defvar fprettify-strip-comments      t)
(defvar fprettify-disable-fypp        nil)
(defvar fprettify-disable-indent-mod  nil)

(defvar fprettify-tmp-file (make-temp-file "fprettify-tmp" nil ".f90"))

(defmacro fprettify-args-format (str var)
  "If `VAR' is nil then return nil, else return `STR' `VAR'."
  `(cond ((integerp ,var) (format " %s %s"    ,str ,var))
	 ((eq t   ,var  ) (format " %s=True"  ,str))
	 ((eq nil ,var  ) (format " %s=False" ,str))
	 (t (message "Unknown argument in macro `fprettify-args-format'."))))
(defun fprettify-args ()
  "Create args."
  (concat
   "-s"
   (fprettify-args-format "-i"  f90-program-indent)
   (fprettify-args-format "-w"  fprettify-whitespace-style)
   (fprettify-args-format "--whitespace-comma"      fprettify-whitespace-comma)
   (fprettify-args-format "--whitespace-assignment" fprettify-whitespace-assignment)
   (fprettify-args-format "--whitespace-decl"       fprettify-whitespace-decl)
   (fprettify-args-format "--whitespace-relational" fprettify-whitespace-relational)
   (fprettify-args-format "--whitespace-logical"    fprettify-whitespace-logical)
   (fprettify-args-format "--whitespace-plusminus"  fprettify-whitespace-plusminus)
   (fprettify-args-format "--whitespace-multdiv"    fprettify-whitespace-multdiv)
   (fprettify-args-format "--whitespace-print"      fprettify-whitespace-print)
   (fprettify-args-format "--whitespace-type"       fprettify-whitespace-type)
   (fprettify-args-format "--whitespace-intrinsics" fprettify-whitespace-intrinsics)
   (when fprettify-strict-indent       " --strict-indent")
   (when fprettify-enable-decl         " --enable-decl")
   (when fprettify-disable-indent      " --disable-indent")
   (when fprettify-disable-whitespace  " --disable-whitespace")
   (when fprettify-enable-replacements " --enable-replacements")
   (when fprettify-c-relations         " --c-relations")
   (when fprettify-strip-comments      " --strip-comments")
   (when fprettify-disable-fypp        " --disable-fypp")
   (when fprettify-disable-indent      " --disable-indent")
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
	(when (< (point-min) (point-max)) ;; Buffer is not empty.
	  (goto-char (point-max))
	  (backward-page)
	  (message (buffer-substring-no-properties (point) (point-max)))
	  (delete-region (point-min) (point-max))))
      (replace-buffer-contents fpe-stdout-buf))))

(add-hook 'f90-mode-hook
	  #'(lambda()
	      (add-hook 'before-save-hook #'fprettify-run nil 'make-it-local)))

(provide 'fprettify)
;;; fprettify.el ends here
