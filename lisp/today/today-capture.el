;;; -*- lexical-binding: t -*-

(require 'f)
(require 's)
(require 'dash)
(require 'org-web-tools)
(require 'cl-lib)

(defun today-capture--url-to-org-link (url)
  "Try to get the website title of URL, then convert into
`org-link' format."
  (let ((title (today-capture--title-from-url url)))
    (org-make-link-string url title)))

(defmacro today-capture--async-lambda (args symbols &rest body)
  "Returns a lambda expression with ARGS, where each symbol in
SYMBOLS is available for use and is bound to it's value at
creation.  Symbols needs to be a list of variables or functions
available globally."
  (declare (indent defun))
  (let ((vars (cl-remove-if-not 'boundp symbols))
        (funcs (cl-remove-if-not 'functionp symbols)))
    `(lambda ,args
       ;; because async starts in a new emacs process, we have to load all the
       ;; functionality we need,
       (add-to-list 'load-path "/home/jens/.emacs.d/elpa/")
       (add-to-list 'load-path "/home/jens/.emacs.d/lisp/today/")
       (add-to-list 'load-path "/home/jens/.emacs.d/straight/builds/f/")

       (let ((default-directory "/home/jens/.emacs.d/straight/repos/"))
         (normal-top-level-add-subdirs-to-load-path))

       (package-initialize)
       (load "/home/jens/.emacs.d/straight/repos/f.el/f.el")

       (require 'f)
       (require 's)
       (require 'dash)
       (require 'today)

       (let ,(mapcar (lambda (sym) (list sym (symbol-value sym))) vars)
         ,@(mapcar (lambda (sym) `(fset ',sym ,(symbol-function sym))) funcs)
         ,@body))))

(defun today-capture--title-from-url (url)
  "Get the website title of URL."
  (let* ((html (org-web-tools--get-url url))
         (title (org-web-tools--html-title html))
         (title (s-replace "[" "(" title))
         (title (s-replace "]" ")" title)))
    title))

(defun today-capture--count-lines-from-url (url)
  "Get the number of lines on the website of URL.

Requires system tool `lynx'."
  (let ((lines (s-trim (shell-command-to-string (format "lynx -dump %s | wc -l" url)))))
    (if (< (length lines) 5) lines "?")))

(defun today-capture--youtube-duration-from-url (url)
  "Get the duration of a youtube video.

Requires system tool `youtube-dl'."
  (letrec ((raw-duration (shell-command-to-string
                          (format "%s '%s'"
                                  "youtube-dl --get-duration"
                                  url))))
    (if (<= (length raw-duration) 10)
        (s-trim raw-duration)
      "?")))


(defcustom today-capture-tasks
  '(read watch)
  "Tasks that can be captured by `today-capture' functions")

(defun today-capture--read-link-handler (link)
  "Handler for READ task. Expects CONTENT to be a link to some
website. Will try to extract the number of lines on the website,
and add to the front of the entry. Will also try to extract the
title of the website, and convert the link into an `org-mode'
link, using the title."
  (letrec ((lines (today-capture--count-lines-from-url link))
           (org-link (today-capture--url-to-org-link link)))
    (format "(%s lines) %s" lines org-link)))

(defun today-capture--watch-link-handler (link)
  "Handler for the WATCH task. Expects the LINK to be a source
compatible with `youtube-dl'.will try to extract the title of the
link, and create an `org-mode' link using that title, will also
extract the duration of the video."
  (letrec ((title (today-capture--title-from-url link))
           (title (replace-regexp-in-string " - YouTube$" "" title))
           (duration (today-capture--youtube-duration-from-url link))
           (org-link (org-make-link-string link title))
           (entry (format "(%s) %s" duration org-link)))
    (format "%s" entry)))

(defvar today-capture-handlers-alist
  '((read . today-capture--read-link-handler)
    (watch . today-capture--watch-link-handler))
  "List of capture tasks and their associated handlers. A handler
  recieves the capture content as a parameter.")

(defun today-capture--apply-handler (task entry)
  "If a handler exists for TASK, then return the result of
applying handler on ENTRY, otherwise return ENTRY."
  (let ((handler (assoc task today-capture-handlers-alist)))
    (if handler
        (funcall (cdr handler) entry)
      entry)))

;;;###autoload
(defun today-capture-async (task entry)
  "Captures an ENTRY with TASK, into the file for DATE, asynchronously."
  (async-start
   (today-capture--async-lambda () (task entry)
     (today-capture--apply-handler task entry))
   (today-capture--async-lambda (content) ()
     (with-current-buffer (find-file-noselect (concat today-directory today-file))
       (save-excursion
         (goto-char (point-max))
         (insert "* TODO " content)
         (save-buffer))))))

;;;###autoload
(defun today-capture (task entry)
  "Capture ENTRY with TASK into todays file."
  (today-capture-async task entry))

(defun today-capture-link-with-task-from-clipboard (task)
  "Capture ENTRY with TASK into todays file."
  (let* ((entry (substring-no-properties
                 (gui-get-selection 'CLIPBOARD 'UTF8_STRING))))
    (message "capturing from clipboard: %s" entry)
    (today-capture-async task entry)))

;;;###autoload
(defun today-capture-link-with-task (task)
  "Prompt for ENTRY, then capture with TASK into today's file."
  (letrec ((link (completing-read "link: " '())))
    (today-capture task link)))

;;;###autoload
(defun today-capture-prompt ()
  "Captures a LINK into today's file, with the selected TASK."
  (interactive)
  (letrec ((task (completing-read "task: " today-capture-tasks))
           (entry (completing-read "entry: " '())))
    (today-capture task entry)))

;;;###autoload
(defun today-capture-elfeed-at-point ()
  "Captures a TASK from selected elfeed entry."
  (interactive)
  (letrec ((entry (car (elfeed-search-selected)))
           (link (elfeed-entry-link entry))
           (title (elfeed-entry-title entry))
           (org-link (org-make-link-string link title)))
    (elfeed-untag entry 'unread)
    (elfeed-search-update-entry entry)
    (today-capture 'elfeed org-link)
    (next-line)))

(provide 'today-capture)
