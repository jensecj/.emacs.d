;;; contrib.el --- Extras for elisp. -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Jens Christian Jensen

;; Author: Jens Christian Jensen <jensecj@subst.net>
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
(defmacro map-elt* (coll &rest keys)
  "Access a nested element in COLL, following KEYS.

Use `setf' to change the element."
  (let ((form `(map-elt ,coll ,(pop keys))))
    (while keys
      (setq form `(map-elt ,form ,(pop keys))))
    form))

;;;; functions

;;;;; misc

(defun just-one-newline ()
  "Collapse newlines around point, so only one remains."
  (interactive)
  (save-restriction
    (save-match-data
      (skip-chars-backward "\n")
      (while (looking-at "\n\n\n")
        (save-excursion
          (delete-region (match-beginning 0) (match-end 0))
          (insert "\n\n"))))))

(defun collapse-whitespace ()
  "Collapse whitespace around point.

If point is in a region of whitespace which spans multiple lines,
whitespace is collapsed into a single newline.

If point is in a region of whitespace on a single line,
whitespace is collapsed into a single space."
  (interactive)
  (let* ((space-chars " \t\r\n")
         (start (+ (point) (skip-chars-backward space-chars)))
         (end (+ (point) (skip-chars-forward space-chars))))
    (cond
     ;; point is in a region of whitespace spanning multiple lines
     ((or (< start (line-beginning-position))
          (> end (line-end-position)))
      (delete-region start end)
      (insert "\n"))
     ;; point is in a region of whitespace on a single line
     ((not (eq start end))
      (delete-region start end)
      (insert " "))
     ;; otherwise, point is not in a region of whitespace
     )))
(advice-add #'collapse-whitespace :after #'indent-according-to-mode)
(bind-key "M-<SPC>" #'collapse-whitespace)

;; (defalias '-> #'thread-first) # TODO: benchmark against dash
;; (defalias '->> #'thread-last)
(defalias 'flatten #'flatten-list)      ;FIXME: don't remove nil elements

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
  (let* ((time-struct (nth 5 (file-attributes (file-truename filename))))
         (delta_secs (float-time (time-subtract (current-time) time-struct))))
    (cl-case format
      ('raw time-struct)
      ('date (format-time-string "%Y-%m-%d" time-struct))
      ('seconds delta_secs)
      ('minutes (floor (/ delta_secs 60)))
      ('hours (floor (/ delta_secs 60 60)))
      ('days (floor (/ delta_secs 60 60 24)))
      (t delta_secs))))

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

;;;; caching
(setq --cache-map (make-hash-table :test 'equal))

(defun --cache (fn &rest args)
  "Cache the functino FN called with ARGS."
  (let* ((key (substring-no-properties (concat (symbol-name fn) "@" (prin1-to-string args))))
         (val (gethash key --cache-map 'not-found-in-cache)))
    (if (eq val 'not-found-in-cache)
        (puthash key (apply fn args) --cache-map)
      val)))

(provide 'contrib)
;;; contrib.el ends here
