;;; cleanup.el --- Simply clean a buffer. -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Jens C. Jensen

;; Author: Jens C. Jensen <jensecj@subst.net>
;; Keywords: format, reformat, clean, buffer
;; Package-Version: 20210311
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.0.50") (dash "2.18.1") (ht "2.3"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'ht)
(require 'dash)

(defvar cleanup-default-fns
  (list #'whitespace-cleanup
        #'cleanup-indent-entire-buffer)
  "List of functions to clean a buffer if no backend is added.")

(defvar cleanup-backends (ht)
  "Backends with functions to use for cleaning up buffers.")

(defun cleanup-indent-entire-buffer ()
  "Indent entire buffer."
  (indent-region (point-min) (point-max)))

(defun cleanup--add (mode fn)
  "Add MODE->FN mapping to the backends."
  (cl-assert (atom mode))
  (cl-assert (functionp fn))
  (let ((val (ht-get cleanup-backends mode '())))
    (ht-set cleanup-backends mode (-snoc val fn))))

(defun cleanup-add (modes fns)
  "Add MODES->FNS mappings to backends."
  (cond
   ((consp modes)
    (dolist (m modes) (cleanup-add m fns)))
   ((and (consp fns) (not (functionp fns)))
    (dolist (f fns) (cleanup-add modes f)))
   ((and (atom modes) (functionp fns))
    (cleanup--add modes fns))))

(defun cleanup-buffer ()
  "Apply backends for current mode, or use the default functions."
  (interactive)
  (message "cleaning buffer")
  (dolist (f (ht-get cleanup-backends major-mode cleanup-default-fns))
    (funcall f))
  (message "buffer cleaned"))

(defun cleanup--elisp-indent-entire-buffer ()
  (unless (check-parens)
    (cleanup-indent-entire-buffer)))

(cleanup-add 'emacs-lisp-mode
             (list #'whitespace-cleanup
                   #'cleanup--elisp-indent-entire-buffer))

(provide 'cleanup)
