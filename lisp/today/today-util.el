;;; -*- lexical-binding: t -*-

(require 'f)
(require 's)
(require 'dash)
(require 'org-web-tools)
(require 'cl-lib)

(defun today-util-list-files ()
  "Get the list of all planning files, from newest to oldest."
  (reverse (-map #'f-base (f-directories today-directory))))

(defun today-util-to-org-link (link title)
  "Convert a link and its title into `org-link' format."
  (format "[[%s][%s]]" link title))

(defun today-util-link-to-org-link (link)
  "Try to get the website title of LINK, then convert into
`org-link' format."
  (let ((title (today-util-get-website-title-from-link link)))
    (today-util-to-org-link link title)))

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
;; Web Utils ;;
;;;;;;;;;;;;;;;

(defun today-util-get-website-title-from-link (link)
  "Get the website title from a link, requires `org-web-tools'."
  (with-current-buffer (url-retrieve-synchronously link)
    (org-web-tools--html-title (buffer-string))))

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

;;;;;;;;;;;;;;;;
;; Date Utils ;;
;;;;;;;;;;;;;;;;

(defun today-util-todays-date ()
  "Today's date."
  (format-time-string "%Y-%m-%d"))

(defun today-util-current-files-date ()
  "Get the date of the current file. See `Notes on structure' for
explanation."
  (f-base (f-no-ext (buffer-file-name))))

(defun today-util-date-add-days (date days)
  "Return the date DAYS after DATE."
  (letrec ((date-in-seconds (float-time (date-to-time (concat date " 12:00:00 EST"))))
           (seconds-in-a-day (* 60 60 24))
           (total-seconds-in-days (* days seconds-in-a-day))
           (next-date-in-seconds (+ date-in-seconds total-seconds-in-days))
           (next-date (format-time-string "%Y-%m-%d" next-date-in-seconds)))
    next-date))

(defun today-util-dates-earlier-than (date)
  "Get all available dates, of files earlier than DATE."
  (letrec ((file-dates (today-util-list-files))
           (sorting-fn (lambda (s) (string< s date)))
           (earlier-dates (-filter sorting-fn file-dates)))
    earlier-dates))

(provide 'today-util)
