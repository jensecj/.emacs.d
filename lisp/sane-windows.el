;;; sane-windows.el --- Simple window management for emacs

;; Copyright (C) 2018 Jens Christian Jensen

;; Author: Jens Christian Jensen <jensecj@gmail.com>
;; Keywords: sane-windows
;; Package-Version: 20190203
;; Version: 0.1
;; Package-Requires: ((emacs "25.1") (dash "2.14.1") (s "1.12.0"))

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

;; Provides a simple way to resize, move, and cycle windows.

;;; Code:

(defun sw/toggle-window-split ()
  "Toggle window splitting between horizontal and vertical."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))
    (message "You can only toggle split of two windows!")))

(defun sw/rotate-windows ()
  "Rotate windows in current window configuration."
  (interactive)
  (cond ((not (> (count-windows)1))
         (message "You can't rotate a single window!"))
        (t
         (let ((i 1)
               (numWindows (count-windows)))
           (while  (< i numWindows)
             (let* ((w1 (elt (window-list) i))
                    (w2 (elt (window-list) (+ (% i numWindows) 1)))

                    (b1 (window-buffer w1))
                    (b2 (window-buffer w2))

                    (s1 (window-start w1))
                    (s2 (window-start w2)))
               (set-window-buffer w1  b2)
               (set-window-buffer w2 b1)
               (set-window-start w1 s2)
               (set-window-start w2 s1)
               (setq i (1+ i))))))))

(defun sw/xor (b1 b2)
  "Exclusive OR."
  (or (and b1 b2)
      (and (not b1) (not b2))))

(defun sw/move-border-left-or-right (arg dir)
  "General function covering sw/move-border-left and sw/move-border-right.
   If DIR is t, then move left, otherwise move right."
  (interactive)
  (if (null arg) (setq arg 3))
  (let ((left-edge (nth 0 (window-edges))))
    (if (sw/xor (= left-edge 0) dir)
        (shrink-window arg t)
      (enlarge-window arg t))))

(defun sw/move-border-up-or-down (arg dir)
  "General function covering sw/move-border-up and sw/move-border-down.
   If DIR is t, then move up, otherwise move down."
  (interactive)
  (if (null arg) (setq arg 3))
  (let ((top-edge (nth 1 (window-edges))))
    (if (sw/xor (= top-edge 0) dir)
        (shrink-window arg nil)
      (enlarge-window arg nil))))

(defun sw/move-border-left (arg)
  "Move the border between windows to the left."
  (interactive "P")
  (sw/move-border-left-or-right arg t))

(defun sw/move-border-right (arg)
  "Move the border between windows to the right."
  (interactive "P")
  (sw/move-border-left-or-right arg nil))

(defun sw/move-border-up (arg)
  "Move the border between windows up."
  (interactive "P")
  (sw/move-border-up-or-down arg t))

(defun sw/move-border-down (arg)
  "Move the border between windows down."
  (interactive "P")
  (sw/move-border-up-or-down arg nil))


(provide 'sane-windows)
;;; sane-windows.el ends here
