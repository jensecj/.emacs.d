;;; contrib.el --- Extras for elisp. -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Jens Christian Jensen

;; Author: Jens Christian Jensen <jensecj@gmail.com>
;; URL:
;; Keywords:
;; Package-Requires ((emacs "28.0.50"))
;; Package-Version: 20200602
;; Version: 0.1.0


;;; Commentary:
;; For things that should be in emacs, but for some reason are not.


;;; Code:

(require 'cl-lib)

;;;; aliases

(defalias '@ 'progn)

;;;;; regular expressions

(defalias 'regexp-search-forward 're-search-forward)
(defalias 'regexp-search-backward 're-search-backward)
(defalias 'regexp-replace 'replace-regexp)
(defalias 'regexp-replace-in-string 'replace-regexp-in-string)
(defalias 'regexp-search-in-string 'string-match)
(defalias 'regexp-match-string 'match-string)
(defalias 'regexp-match-string-no-properties 'match-string-no-properties)
(defalias 'regexp-replace-match 'replace-match)
(defalias 'regexp-match-beginning 'match-beginning)
(defalias 'regexp-match-end 'match-end)

;;;;; strings
(defalias 'string-split 'split-string)
(defalias 'string-upcase 'upcase)
(defalias 'string-downcase 'downcase)
(defalias 'string-capitalize 'capitalize)

;;;;; files
(defalias 'file-rename 'rename-file)
(defalias 'file-delete 'delete-file)
(defalias 'file-copy 'copy-file)

;;;; macros

;;;; functions

;;;;; misc

(defun -mapcar (fn &rest lists)
  "Map FN to the car of each list in LISTS."
  (cond
   ((null lists) nil)
   ((null (car lists)) nil)
   ((listp lists)
    (cons
     (apply fn (-map #'car lists))
     (apply #'-mapcar fn (-map #'cdr lists))))))

(defun deleteq (ELT LIST)
  "Delete ELT from LIST, in-place."
  (setq LIST (delete ELT LIST)))

(cl-defmacro bench (&optional (times 100000) &rest body)
  "Call `benchmark-run-compiled' on BODY with TIMES iterations,
returning list suitable for Org source block evaluation.  Garbage
is collected before calling `benchmark-run-compiled' to avoid
counting existing garbage which needs collection."
  (declare (indent defun))
  `(progn
     (garbage-collect)
     (list '("Total runtime" "# of GCs" "Total GC runtime")
           'hline
           (benchmark-run-compiled ,times
             (progn
               ,@body)))))

(defun uuid ()
  "Generate a random UUID.

With `prefix-arg', insert the UUID at point in the current buffer."
  (interactive)

  (unless (executable-find "uuidgen")
    (error "generating a UUID requires the `uuidgen' executable."))

  (let ((id (replace-regexp-in-string "\n" "" (shell-command-to-string "uuidgen --random"))))
    (when current-prefix-arg
      (insert id))
    id))

(defun buffer-string-no-properties ()
  "Return the contents of the current buffer as a string, without text-properties."
  (buffer-substring-no-properties (point-min) (point-max)))

(defun file-age (filename &optional format)
  "Return the number of seconds since FILENAME was last modified.

Alternatively the return type is defined by FORMAT
Valid formats are `seconds', `minutes', `hours', and `days'."
  (let ((secs
         (float-time
          (time-subtract (current-time)
                         (nth 5 (file-attributes (file-truename filename)))))))
    (floor (case format
             ('seconds secs)
             ('minutes (/ secs 60))
             ('hours (/ secs 60 60))
             ('days (/ secs 60 60 24))
             (t secs)))))

;;;;; syntax-ppss accessors

(defun syntax-ppss-parens-depth (&optional pos)
  "Depth in parens."
  (nth 0 (syntax-ppss pos)))

(defun syntax-ppss-inner-list-beginning (&optional pos)
  "character address of start of innermost containing list; nil if none."
  (nth 1 (syntax-ppss pos)))

(defun syntax-ppss-last-sexp-beginning (&optional pos)
  "character address of start of last complete sexp terminated."
  (nth 2 (syntax-ppss pos)))

(defun syntax-ppss-inside-string (&optional pos)
  "non-nil if inside a string.
    (it is the character that will terminate the string,
     or t if the string should be terminated by a generic string delimiter.)"
  (nth 3 (syntax-ppss pos)))

(defun syntax-ppss-inside-comment (&optional pos)
  "nil if outside a comment, t if inside a non-nestable comment,
    else an integer (the current comment nesting)."
  (nth 4 (syntax-ppss pos)))

(defun syntax-ppss-after-quote (&optional pos)
  "t if following a quote character."
  (nth 5 (syntax-ppss pos)))

(defun syntax-ppss-min-paren-depth (&optional pos)
  "the minimum paren-depth encountered during this scan."
  (nth 6 (syntax-ppss pos)))

(defun syntax-ppss-comment-style (&optional pos)
  "style of comment, if any."
  (nth 7 (syntax-ppss pos)))

(defun syntax-ppss-comment-or-string-beginning (&optional pos)
  "character address of start of comment or string; nil if not in one."
  (nth 8 (syntax-ppss pos)))

(defun syntax-ppss-open-parens-positions (&optional pos)
  "List of positions of currently open parens, outermost first."
  (nth 9 (syntax-ppss pos)))

(defun syntax-ppss-two-char-construct (&optional pos)
  "When the last position scanned holds the first character of a
    (potential) two character construct, the syntax of that position,
    otherwise nil.  That construct can be a two character comment
    delimiter or an Escaped or Char-quoted character."
  (nth 10 (syntax-ppss pos)))


(provide 'contrib)
;;; contrib.el ends here
