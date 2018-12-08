;;; highlight-bookmarks.el --- Toggling highlight-bookmarks state of buffers

;; Copyright (C) 2018 Jens Christian Jensen

;; Author: Jens Christian Jensen <jensecj@gmail.com>
;; Keywords: highlight-bookmarks
;; Package-Version: 20181208
;; Version: 0.1
;; Package-Requires: ((emacs "25.1") (ov "1.0.6") (dash "2.14.1") (f "0.20.0"))

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

(require 'ov)

(defcustom highlight-bookmarks-bookmark-face
  '(:background "#2B2B2B")
  "Face used to highlight the line of a bookmark.")

(defun highlight-bookmarks-in-this-buffer ()
  (interactive)

  ;; clear previous bookmark highlights, to avoid double-drawing
  (ov-clear 'highlight-bookmarks-highlight)

  ;; grab bookmarks from `bookmark-alist', and create an overlay for all
  ;; bookmarks that belong to the current buffer
  (let* ((bm-list (-map #'(lambda (e)
                            (cons (map-elt e 'filename)
                                  (map-elt e 'position)))
                        bookmark-alist))
         (buffer-bms (-filter #'(lambda (e)
                                  (f-same?
                                   (car e)
                                   (buffer-file-name (current-buffer))))
                              bm-list)))
    (if buffer-bms
        (-map #'(lambda (b)
                  (ov-set (ov-line (cdr b))
                          'face highlight-bookmarks-bookmark-face
                          'highlight-bookmarks-highlight t))
              buffer-bms))))

(provide 'highlight-bookmarks)
;;; highlight-bookmarks.el ends here
