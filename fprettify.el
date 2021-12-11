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
  :type '(choice (const :tag "Default path" nil)
                 (directory :tag "Path to directory")))

(defun fprettify--executable-command ()
  "Return full-path to fprettify if `fprettify-executable-path' exists.
Otherwise return fprettify."
  (if fprettify-executable-path
      (f-join fprettify-executable-path fprettify-executable)
    fprettify-executable))

(defcustom fprettify-config-file nil
  "Path to fprettify config file.
Default: nil."
  :group 'fprettify
  :type '(choice (const :tag "Not use config file" nil)
                 (file :must-match t)))

(defcustom fprettify-indent 'f90-program-indent
  "Relative indentation width.
Default: value of `f90-program-indent', not 3."
  :group 'fprettify
  :type '(choice (variable :value f90-program-indent)
                 (integer :value 2)))

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
                 (const :tag "operators (except arithmetic) + 0"       1)
                 (const :tag "print or read, + or -         + 0 1"     2)
                 (const :tag "* or /                        + 0 1 2"   3)
                 (const :tag "type % member                 + 0 1 2 3" 4)))

(defcustom fprettify-whitespace-comma 'none
  "Whitespace for comma (e.g. ,).
Default: None."
  :group 'fprettify
  :type '(choice (const :tag "None"  none)
                 (const :tag "True"  t)
                 (const :tag "False" nil)))

(defcustom fprettify-whitespace-assignment 'none
  "Whitespace for assignment (e.g. =).
Default: None."
  :group 'fprettify
  :type '(choice (const :tag "None"  none)
                 (const :tag "True"  t)
                 (const :tag "False" nil)))

(defcustom fprettify-whitespace-decl 'none
  "Whitespace for declarations if `--enable-decl' exists.
Default: None."
  :group 'fprettify
  :type '(choice (const :tag "None"  none)
                 (const :tag "True"  t)
                 (const :tag "False" nil)))

(defcustom fprettify-whitespace-relational 'none
  "Whitespace for relational operators (e.g. .le., < ...).
Default: None."
  :group 'fprettify
  :type '(choice (const :tag "None"  none)
                 (const :tag "True"  t)
                 (const :tag "False" nil)))

(defcustom fprettify-whitespace-logical 'none
  "Whitespace for logical operators (e.g. .and., .or.).
Default: None."
  :group 'fprettify
  :type '(choice (const :tag "None"  none)
                 (const :tag "True"  t)
                 (const :tag "False" nil)))

(defcustom fprettify-whitespace-plusminus 'none
  "Whitespace for plus/minus arithmetic (e.g. +, -).
Default: None."
  :group 'fprettify
  :type '(choice (const :tag "None"  none)
                 (const :tag "True"  t)
                 (const :tag "False" nil)))

(defcustom fprettify-whitespace-multdiv 'none
  "Whitespace for multiply/divide arithmetic (e.g. *, /).
Default: None."
  :group 'fprettify
  :type '(choice (const :tag "None"  none)
                 (const :tag "True"  t)
                 (const :tag "False" nil)))

(defcustom fprettify-whitespace-print 'none
  "Whitespace for print/read.
Default: None."
  :group 'fprettify
  :type '(choice (const :tag "None"  none)
                 (const :tag "True"  t)
                 (const :tag "False" nil)))

(defcustom fprettify-whitespace-type 'none
  "Whitespace for select type (e.g. type % member).
Default: None."
  :group 'fprettify
  :type '(choice (const :tag "None"  none)
                 (const :tag "True"  t)
                 (const :tag "False" nil)))

(defcustom fprettify-whitespace-intrinsics 'none
  "Whitespace for intrisics like if/write/close.
Default: None."
  :group 'fprettify
  :type '(choice (const :tag "None"  none)
                 (const :tag "True"  t)
                 (const :tag "False" nil)))

(defcustom fprettify-strict-indent nil
  "Strictly impose indentation even for nested loops.
Default: False."
  :group 'fprettify
  :type 'boolean)

(defcustom fprettify-enable-decl nil
  "Enable whitespace formatting of declarations (e.g. ::).
Default: False."
  :group 'fprettify
  :type 'boolean)

(defcustom fprettify-disable-indent nil
  "Don't impose indentation.
Default: False."
  :group 'fprettify
  :type 'boolean)

(defcustom fprettify-disable-whitespace nil
  "Don't impose whitespace formatting.
Default: False."
  :group 'fprettify
  :type 'boolean)

(defcustom fprettify-enable-replacements nil
  "Replace relational operators (e.g. '.lt.' <--> '<').
Default: False."
  :group 'fprettify
  :type 'boolean)

(defcustom fprettify-c-relations nil
  "C-style relational operators ('<', '<=', ...).
Default: False."
  :group 'fprettify
  :type 'boolean)

(defcustom fprettify-strip-comments nil
  "Strip whitespaces before comments.
Default: False."
  :group 'fprettify
  :type 'boolean)

(defcustom fprettify-disable-fypp nil
  "Disables the indentation of fypp preprocessor blocks.
Default: False."
  :group 'fprettify
  :type 'boolean)

(defcustom fprettify-disable-indent-mod nil
  "Disables the indentation after module / program.
Default: False."
  :group 'fprettify
  :type 'boolean)

(defcustom fprettify-case-1 '0
  "Case of Case of program, module, contains, do, write and etc.
0: do nothing | 1: lowercase | 2: uppercase
Default: 0."
  :group 'fprettify
  :type '(choice (const :tag "do nothing" 0)
                 (const :tag "lowercase"  1)
                 (const :tag "uppercase"  2)))

(defcustom fprettify-case-2 '0
  "Case of intrisic module and function (e.g. iso_fortran_env, atan(...)).
0: do nothing | 1: lowercase | 2: uppercase
Default: 0."
  :group 'fprettify
  :type '(choice (const :tag "do nothing" 0)
                 (const :tag "lowercase"  1)
                 (const :tag "uppercase"  2)))

(defcustom fprettify-case-3 '0
  "Case of logical/relational operators (e.g. .or., .le.).
0: do nothing | 1: lowercase | 2: uppercase
Default: 0."
  :group 'fprettify
  :type '(choice (const :tag "do nothing" 0)
                 (const :tag "lowercase"  1)
                 (const :tag "uppercase"  2)))

(defcustom fprettify-case-4 '0
  "Case of intrinsic variables (e.g. e0, d0, int32).
0: do nothing | 1: lowercase | 2: uppercase
Default: 0."
  :group 'fprettify
  :type '(choice (const :tag "do nothing" 0)
                 (const :tag "lowercase"  1)
                 (const :tag "uppercase"  2)))

(defmacro fprettify--args-format-int (str symb-or-int)
  "Format int or symbol variable `SYMB-OR-INT' of fprettify option `STR'."
  (let ((val (gensym)))
    `(progn
       (cond ((symbolp ,symb-or-int)
              (setq ,val (symbol-value ,symb-or-int)))
             (t (setq ,val ,symb-or-int)))
       (cond ((integerp ,val) (format " %s %s" ,str ,val))
             (t (error "Unknown value of argument %s (%s) %s in macro `fprettify--args-format-int'" ,str ',symb-or-int ,val))))))

(defmacro fprettify--args-format-file (str fname)
  "Format filename variable `FNAME' of fprettify option `STR'."
  `(cond ((file-readable-p ,fname) (format " %s %s" ,str ,fname))
         (t (error "Not readable file %s of %s in macro `fprettify--args-format-file'" ,fname ',fname))))

(defmacro fprettify--args-format-enable (str var)
  "Format (t, nil, 'none) variable `VAR' of fprettify option `STR'.
If `VAR' is nil -> False t -> True, 'none -> None.."
  `(cond ((eq 'none ,var) (format " %s=None"  ,str))
         ((eq t     ,var) (format " %s=True"  ,str))
         ((eq nil   ,var) (format " %s=False" ,str))
         (t (error "Unknown value of argument %s (%s) %s in macro `fprettify--args-format-enable'" ,str ',var ,var))))

(defmacro fprettify--args-format-case (str &rest case)
  "Format int variable `CASE' of fprettify option `STR'."
  (let ((fmt (gensym))
        (lst (gensym)))
    `(let ((,fmt ,str)
           (,lst (list ,@case)))
       (when (/= 4 (length ,lst))
         (error "Length of %s must be 4 in macro `fprettify--args-format-case'" ',case))
       (dolist (var ,lst)
         (cond ((integerp var) (setq ,fmt (format " %s %d" ,fmt var)))
               (t (error "Unknown value of argument %s (%s) %s in macro `fprettify--args-format-case'" ,str ',case var))))
       ,fmt)))

(defun fprettify--args ()
  "Create args."
  (if fprettify-config-file
      (concat "-s"
              (fprettify--args-format-file "-c" fprettify-config-file))
    (concat
     "-s"
     (fprettify--args-format-int "-i"  fprettify-indent)
     (fprettify--args-format-int "-l"  fprettify-line-length)
     (fprettify--args-format-int "-w"  fprettify-whitespace-style)
     (fprettify--args-format-enable "--whitespace-comma"      fprettify-whitespace-comma)
     (fprettify--args-format-enable "--whitespace-assignment" fprettify-whitespace-assignment)
     (fprettify--args-format-enable "--whitespace-decl"       fprettify-whitespace-decl)
     (fprettify--args-format-enable "--whitespace-relational" fprettify-whitespace-relational)
     (fprettify--args-format-enable "--whitespace-logical"    fprettify-whitespace-logical)
     (fprettify--args-format-enable "--whitespace-plusminus"  fprettify-whitespace-plusminus)
     (fprettify--args-format-enable "--whitespace-multdiv"    fprettify-whitespace-multdiv)
     (fprettify--args-format-enable "--whitespace-print"      fprettify-whitespace-print)
     (fprettify--args-format-enable "--whitespace-type"       fprettify-whitespace-type)
     (fprettify--args-format-enable "--whitespace-intrinsics" fprettify-whitespace-intrinsics)
     (fprettify--args-format-case "--case" fprettify-case-1 fprettify-case-2 fprettify-case-3 fprettify-case-4)
     (when fprettify-strict-indent       " --strict-indent")
     (when fprettify-enable-decl         " --enable-decl")
     (when fprettify-disable-indent      " --disable-indent")
     (when fprettify-disable-whitespace  " --disable-whitespace")
     (when fprettify-enable-replacements " --enable-replacements")
     (when fprettify-c-relations         " --c-relations")
     (when fprettify-strip-comments      " --strip-comments")
     (when fprettify-disable-fypp        " --disable-fypp")
     (when fprettify-disable-indent-mod  " --disable-indent-mod"))))

(defun fprettify--command ()
  "Create command."
  (format "%s %s"
          (fprettify--executable-command)
          (fprettify--args)))

;;;###autoload
(defun fprettify-run ()
  "Run `fprettify' on buffer."
  (interactive)
  (fprettify-run-on-region (point-min) (point-max)))

;;;###autoload
(defun fprettify-run-on-region (start end)
  "Run `fprettify' on region and replace contents.
START: `beginning-of-line' of `region-beginning'.
END:   `beginning-of-line' of `next-line' of `region-end'.
If warning exists, echo message in `*fprettify<stderr>*'.
If error exists, echo message in `*fprettify<stderr>*', replace do not occur."
  (interactive
   (let* ((cur-line (line-number-at-pos))
          (beg-line (line-number-at-pos (region-beginning)))
          (end-line (line-number-at-pos (region-end)))
          (region-beg-line-beg-pos (1+ (- beg-line cur-line)))
          (region-end-line-beg-pos (1+ (- end-line cur-line))))
     (list (line-beginning-position region-beg-line-beg-pos)
           (line-beginning-position region-end-line-beg-pos))))
  (save-excursion
    (let ((cur-buf        (current-buffer))
          (fpe-stdout-buf (get-buffer-create "*fprettify*"))
          (fpe-stderr-buf (get-buffer-create "*fprettify<stderr>*"))
          (ext-code))
      ;; Erase contents of `fpe-stderr-buf'.
      (with-current-buffer fpe-stderr-buf
        (erase-buffer))
      (with-current-buffer fpe-stdout-buf
        (replace-buffer-contents cur-buf)
        (setq ext-code
              (shell-command-on-region start end
                                       (fprettify--command)
                                       fpe-stdout-buf
                                       t
                                       fpe-stderr-buf
                                       t))
        ;; Error handling.
        (with-current-buffer fpe-stderr-buf
          (let ((start (point-min))
                (end   (point-max)))
            ;; If error occur.
            (when (/= ext-code 0)
              (error "%s exited with code %s" (fprettify--command) ext-code))
            (when (and (< start end)
                       (search-forward "error" nil t))
              (error (buffer-substring-no-properties start end))))))
      (replace-buffer-contents fpe-stdout-buf))))

(provide 'fprettify)
;;; fprettify.el ends here
