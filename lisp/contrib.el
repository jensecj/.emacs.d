;;; contrib.el --- Extras for elisp. -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Jens Christian Jensen

;; Author: Jens Christian Jensen <jensecj@gmail.com>
;; URL:
;; Keywords:
;; Package-Requires ((emacs "27.0.50"))
;; Package-Version: 20190715
;; Version: 0.1.0


;;; Commentary:
;;

;;; Code:

;;;; convenience aliases

(defalias '@ 'progn)


;;;; syntax-ppss accessors

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