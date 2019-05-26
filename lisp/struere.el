;;; struere.el --- Simple tool for re-structuring buffers. -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Jens Christian Jensen

;; Author: Jens Christian Jensen <jensecj@gmail.com>
;; Keywords: format, reformat, clean, buffer
;; Package-Version: 20190526
;; Version: 0.1.0

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

(defvar struere-default-fns
  (list #'whitespace-cleanup
        (lambda () (indent-region (window-start) (window-end))))
  "List of functions to cleanup a buffer if no backend is added.")

(defvar struere-backends (ht)
  "Backends with functions to use for cleaning up buffers.")

(defun struere--add (mode fn)
  "Add MODE->FN mapping to the backends."
  (assert (atom mode))
  (assert (functionp fn))
  (let ((val (ht-get struere-backends mode '())))
    (ht-set struere-backends mode (-snoc val fn))))

(defun struere-add (modes fns)
  "Add MODES->FNS mappings to backends."
  (cond
   ((consp modes)
    (dolist (m modes) (struere-add m fns)))
   ((and (consp fns) (not (functionp fns)))
    (dolist (f fns) (struere-add modes f)))
   ((and (atom modes) (functionp fns))
    (struere--add modes fns))))

(defun struere-cleanup-buffer ()
  "Apply backends for current mode, or use the default functions."
  (interactive)
  (-map-indexed (lambda (i e)
                  (message "cleanup %s: %s" i e)
                  (funcall e))
                (ht-get struere-backends major-mode struere-default-fns))

  (message "buffer cleaned"))

(provide 'struere)
