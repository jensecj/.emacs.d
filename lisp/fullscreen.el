;;; fullscreen.el --- Toggling fullscreen state of buffers

;; Copyright (C) 2018 Jens Christian Jensen

;; Author: Jens Christian Jensen <jensecj@gmail.com>
;; Keywords: fullscreen
;; Package-Version: 20180511
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

;; Provides a simple way to toggle fullscreen of a single buffer, and go back to
;; the previous window configuration afterwards.

;;; Code:

(defvar fullscreen-alist
  nil
  "List of window-configurations stored before calling ´fullscreen´.")

(defun fullscreen ()
  "Make the current buffer fill the entire window."
  (interactive)
  (message "entered fullscreen")
  (add-to-list 'fullscreen-alist (window-state-get))
  (delete-other-windows))

(defun fullscreen-quit ()
  "Return to the previous window configuration."
  (interactive)
  (message "exited fullscreen")
  (window-state-put (pop fullscreen-alist)))

(defun fullscreen-toggle ()
  "Toggle fullscreen state of a single buffer."
  (interactive)
  (if fullscreen-alist
      (fullscreen-quit)
    (fullscreen)))

(provide 'fullscreen)
