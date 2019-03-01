;;; -*- lexical-binding: t -*-

(require 'f)
(require 's)
(require 'dash)
(require 'org-web-tools)
(require 'cl-lib)

(require 'today-fs)

(defun today-util-link-to-org-link (link)
  "Try to get the website title of LINK, then convert into
`org-link' format."
  (let ((title (today-util-get-website-title-from-link link)))
    (org-make-link-string link title)))

(defmacro today-util-async-lambda (args symbols &rest body)
  "Returns a lambda expression with ARGS, where each symbol in SYMBOLS is
available for use and is bound to it's value at creation.
Symbols needs to be a list of variables or functions available globally."
  (declare (indent defun))
  (let ((vars (cl-remove-if-not 'boundp symbols))
        (funcs (cl-remove-if-not 'functionp symbols)))
    `(lambda ,args
       ;; because async starts in a new emacs process, we have to load all the
       ;; functionality we need,
       (add-to-list 'load-path "/home/jens/.emacs.d/elpa/")
       (add-to-list 'load-path "/home/jens/.emacs.d/lisp/today/")

       (package-initialize)

       (require 'f)
       (require 's)
       (require 'dash)
       (require 'today)

       (let ,(mapcar (lambda (sym) (list sym (symbol-value sym))) vars)
         ,@(mapcar (lambda (sym) `(fset ',sym ,(symbol-function sym))) funcs)
         ,@body))))

;;;;;;;;;;;;;;;
;; web utils ;;
;;;;;;;;;;;;;;;

(defun today-util-get-website-title-from-link (url)
  "Get the website title from a link, requires `org-web-tools'."
  (let* ((html (org-web-tools--get-url url))
         (title (org-web-tools--html-title html))
         (title (s-replace "[" "(" title))
         (title (s-replace "]" ")" title)))
    title))

(defun today-util-get-website-lines-from-link (link)
  "Get the number of lines on the website of LINK. requires system tool `lynx'."
  (let ((lines (s-trim (shell-command-to-string (format "lynx -dump %s | wc -l" link)))))
    (if (< (length lines) 5) lines "?")))

(defun today-util-get-youtube-duration-from-link (link)
  "Get the duration of a youtube video, requires system tool `youtube-dl'."
  (letrec ((raw-duration (shell-command-to-string
                          (format "%s '%s'"
                                  "youtube-dl --get-duration"
                                  link))))
    (if (<= (length raw-duration) 10)
        (s-trim raw-duration)
      "?")))

(provide 'today-util)
