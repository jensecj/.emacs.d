;;; struere.el --- Simple tool for re-structuring buffers. -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Jens Christian Jensen

;; Author: Jens Christian Jensen <jensecj@gmail.com>
;; Keywords: reformat, clean, buffer
;; Package-Version: 20190525
;; Version: 0.1

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

(setq struere-default-cleanup
      (list #'whitespace-cleanup
            (lambda () (indent-region (point-min) (point-max)))))

(setq struere-backends (ht))

(defun struere--add (mode fn)
  "Map a single function to a mode."
  (assert (atom mode))
  (assert (functionp fn))
  (let ((val (ht-get struere-backends mode '())))
    (ht-set struere-backends mode (-snoc val fn))))

(defun struere-add (modes fns)
  "Map FNS to MODES in the struere backends."
  (cond
   ((consp modes)
    (dolist (m modes) (struere-add m fns)))
   ((and (consp fns) (not (functionp fns)))
    (dolist (f fns) (struere-add modes f)))
   ((and (atom modes) (functionp fns))
    (struere--add modes fns))))

(defun struere-cleanup-buffer ()
  "Perform a bunch of operations on the white space content of a buffer.
Including indent-buffer, which should not be called automatically
on save."
  (interactive)

  (message "cleaning buffer")

  (-map-indexed (lambda (i e)
                  (message "%s: %s" i e)
                  (funcall e))
                (ht-get struere-backends major-mode struere-default-cleanup))

  (message "cleaned up"))

(provide 'struere)
