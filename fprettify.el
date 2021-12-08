;;; fprettify.el --- Interface to fprettify, auto-formatter for modern Fortran code -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'f90)
(defgroup fprettify nil
  "Format with fprettify."
  :group 'applications)

(defvar fprettify-executable "fprettify")

(defcustom fprettify-executable-path nil
  "Path to directory of executable of fprettify."
  :group 'fprettify
  :type 'directory)

(defun fprettify-executable-command ()
  "Return full-path to fprettify if `fprettify-executable-path' exists.
Otherwise return fprettify."
  (concat fprettify-executable-path fprettify-executable))

(defcustom fprettify-config-file nil
  "Path to fprettify config file.
Default: `nil'."
  :group 'fprettify
  :type 'file)

(defcustom fprettify-indent f90-program-indent
  "Relative indentation width.
Default: value of `f90-program-indent', not 3."
  :group 'fprettify
  :type 'integer)

(defcustom fprettify-whitespace-style 2
  "Select whitespace style from 5 presets.
Default: 2."
  :group 'fprettify
  :type '(choice (const :tag "minimal" 0)
                 (const :tag "0 + operators (except arithmetic)" 1)
                 (const :tag "1 + print or read, + or -" 2)
                 (const :tag "2 + * or /" 3)
                 (const :tag "3 + type % member" 4)))

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
         ((stringp  ,var) (format " %s %s"    ,str ,var))
         ((eq t     ,var) (format " %s=True"  ,str))
         ((eq nil   ,var) (format " %s=False" ,str))
         (t (message "Unknown argument in macro `fprettify-args-format'."))))
(defun fprettify-args ()
  "Create args."
  (if (and fprettify-config-file
           (file-readable-p fprettify-config-file))
      (concat "-s"
              (fprettify-args-format "-c" fprettify-config-file))
    (concat
     "-s"
     (fprettify-args-format "-i"  fprettify-indent)
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
     (when fprettify-disable-indent      " --disable-indent"))))
(defun fprettify-command ()
  "Create command."
  (format "%s %s"
          (fprettify-executable-command)
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
