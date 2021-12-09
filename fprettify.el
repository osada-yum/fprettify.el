;;; fprettify.el --- Interface to fprettify, auto-formatter for modern Fortran code -*- lexical-binding: t; -*-
;;; Commentary:

;;; Some doc strings is from `fprettify --help'.

;;; Code:
(require 'f90)
(require 'f)

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
  (if fprettify-executable-path
      (f-join fprettify-executable-path fprettify-executable)
    fprettify-executable))

(defcustom fprettify-config-file nil
  "Path to fprettify config file.
Default: nil."
  :group 'fprettify
  :type 'file)

(defcustom fprettify-indent f90-program-indent
  "Relative indentation width.
Default: value of `f90-program-indent', not 3."
  :group 'fprettify
  :type 'integer)

(defcustom fprettify-line-length 132
  "Warning if column of line exceeds this value.
Default: 132."
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

(defcustom fprettify-whitespace-comma 'none
  "Whitespace for comma (e.g. ,).
Default: None."
  :group 'fprettify
  :type '(choice (const :tag "True"  t)
                 (const :tag "False" nil)
                 (const :tag "None"  'none)))

(defcustom fprettify-whitespace-assignment 'none
  "Whitespace for assignment (e.g. =).
Default: None."
  :group 'fprettify
  :type '(choice (const :tag "True"  t)
                 (const :tag "False" nil)
                 (const :tag "None"  'none)))

(defcustom fprettify-whitespace-decl 'none
  "Whitespace for declarations.
Default: None."
  :group 'fprettify
  :type '(choice (const :tag "True"  t)
                 (const :tag "False" nil)
                 (const :tag "None"  'none)))

(defcustom fprettify-whitespace-relational 'none
  "Whitespace for relational operators (e.g. .le., < ...).
Default: None."
  :group 'fprettify
  :type '(choice (const :tag "True"  t)
                 (const :tag "False" nil)
                 (const :tag "None"  'none)))

(defcustom fprettify-whitespace-logical 'none
  "Whitespace for logical operators (e.g. .and., .or.).
Default: None."
  :group 'fprettify
  :type '(choice (const :tag "True"  t)
                 (const :tag "False" nil)
                 (const :tag "None"  'none)))

(defcustom fprettify-whitespace-plusminus 'none
  "Whitespace for plus/minus arithmetic (e.g. +, -).
Default: None."
  :group 'fprettify
  :type '(choice (const :tag "True"  t)
                 (const :tag "False" nil)
                 (const :tag "None"  'none)))

(defcustom fprettify-whitespace-multdiv 'none
  "Whitespace for multiply/divide arithmetic (e.g. *, /).
Default: None."
  :group 'fprettify
  :type '(choice (const :tag "True"  t)
                 (const :tag "False" nil)
                 (const :tag "None"  'none)))

(defcustom fprettify-whitespace-print 'none
  "Whitespace for print/read.
Default: None."
  :group 'fprettify
  :type '(choice (const :tag "True"  t)
                 (const :tag "False" nil)
                 (const :tag "None"  'none)))

(defcustom fprettify-whitespace-type 'none
  "Whitespace for select type (e.g. type % member).
Default: None."
  :group 'fprettify
  :type '(choice (const :tag "True"  t)
                 (const :tag "False" nil)
                 (const :tag "None"  'none)))

(defcustom fprettify-whitespace-intrinsics 'none
  "Whitespace for intrisics like if/write/close.
Default: None."
  :group 'fprettify
  :type '(choice (const :tag "True"  t)
                 (const :tag "False" nil)
                 (const :tag "None"  'none)))

(defcustom fprettify-strict-indent nil
  "Strictly impose indentation even for nested loops.
Default: False."
  :group 'fprettify
  :type '(choice (const :tag "True"  t)
                 (const :tag "False" nil)
                 (const :tag "None"  'none)))

(defcustom fprettify-enable-decl nil
  "Enable whitespace formatting of declarations (e.g. ::).
Default: False."
  :group 'fprettify
  :type '(choice (const :tag "True"  t)
                 (const :tag "False" nil)
                 (const :tag "None"  'none)))

(defcustom fprettify-disable-indent nil
  "Don't impose indentation.
Default: False."
  :group 'fprettify
  :type '(choice (const :tag "True"  t)
                 (const :tag "False" nil)
                 (const :tag "None"  'none)))

(defcustom fprettify-disable-whitespace nil
  "Don't impose whitespace formatting.
Default: False."
  :group 'fprettify
  :type '(choice (const :tag "True"  t)
                 (const :tag "False" nil)
                 (const :tag "None"  'none)))

(defcustom fprettify-enable-replacements nil
  "Replace relational operators (e.g. '.lt.' <--> '<').
Default: False."
  :group 'fprettify
  :type '(choice (const :tag "True"  t)
                 (const :tag "False" nil)
                 (const :tag "None"  'none)))

(defcustom fprettify-c-relations nil
  "C-style relational operators ('<', '<=', ...).
Default: False."
  :group 'fprettify
  :type '(choice (const :tag "True"  t)
                 (const :tag "False" nil)
                 (const :tag "None"  'none)))

(defcustom fprettify-strip-comments nil
  "Strip whitespaces before comments.
Default: False."
  :group 'fprettify
  :type '(choice (const :tag "True"  t)
                 (const :tag "False" nil)
                 (const :tag "None"  'none)))

(defcustom fprettify-disable-fypp nil
  "Disables the indentation of fypp preprocessor blocks.
Default: False."
  :group 'fprettify
  :type '(choice (const :tag "True"  t)
                 (const :tag "False" nil)
                 (const :tag "None"  'none)))

(defcustom fprettify-disable-indent-mod nil
  "Disables the indentation after module / program.
Default: False."
  :group 'fprettify
  :type '(choice (const :tag "True"  t)
                 (const :tag "False" nil)
                 (const :tag "None"  'none)))

(defmacro fprettify-args-format (str var)
  "If `VAR' is nil then return nil, else return `STR' `VAR'."
  `(cond ((integerp ,var) (format " %s %s"    ,str ,var))
         ((stringp  ,var) (format " %s %s"    ,str ,var))
         ((eq 'none ,var) (format " %s=None"  ,str))
         ((eq t     ,var) (format " %s=True"  ,str))
         ((eq nil   ,var) (format " %s=False" ,str))
         (t (error "Unknown argument %s %s in macro `fprettify-args-format'" ,str ,var))))

(defun fprettify-args ()
  "Create args."
  (if (and fprettify-config-file
           (file-readable-p fprettify-config-file))
      (concat "-s"
              (fprettify-args-format "-c" fprettify-config-file))
    (concat
     "-s"
     (fprettify-args-format "-i"  fprettify-indent)
     (fprettify-args-format "-l"  fprettify-line-length)
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
     (when fprettify-disable-fypp        " --disable-fypp"))))

(defun fprettify-command ()
  "Create command."
  (format "%s %s"
          (fprettify-executable-command)
          (fprettify-args)))

(defun fprettify-run ()
  "Run `fprettify' with `current-buffer' and replace contents.
If warning exists, echo message in `*fprettify<stderr>*'."
  (interactive)
  (let ((current-linum (line-number-at-pos)) ; save-excursion do not work when replace contents by shell-command-on-region?
        (fpe-stderr-buf (get-buffer-create "*fprettify<stderr>*")))
    (shell-command-on-region (point-min) (point-max)
                             (fprettify-command)
                             nil
                             t
                             fpe-stderr-buf
                             t)
    (forward-line (1- current-linum))))


(provide 'fprettify)
;;; fprettify.el ends here
