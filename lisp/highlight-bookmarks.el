;;; highlight-bookmarks.el --- Toggling highlight-bookmarks state of buffers. -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Jens Christian Jensen

;; Author: Jens Christian Jensen <jensecj@subst.net>
;; Keywords: highlighting, bookmarks
;; Package-Version: 20200626
;; Version: 0.3
;; Package-Requires: ((emacs "25.1") (ov "1.0.6") (dash "2.18.1") (f "0.20.0"))

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

;; Highlight bookmarks in the current buffer using overlays.

;; a configuration might look like this:
;; (add-hook 'find-file-hook #'highlight-bookmarks-in-this-buffer)
;; (add-hook 'after-save-hook #'highlight-bookmarks-in-this-buffer)
;; (advice-add #'bookmark-jump :after #'highlight-bookmarks-in-this-buffer)
;; (advice-add #'bookmark-set :after #'highlight-bookmarks-in-this-buffer)
;; (advice-add #'bookmark-delete :after #'highlight-bookmarks-in-this-buffer)

;;; Code:

(require 'ov)
(require 'bookmark)
(require 'dash)
(require 'f)

(defcustom highlight-bookmarks-bookmark-face
  '(:background "#2B2B2B")
  "Face used to highlight the line of a bookmark."
  :group 'highlight-bookmarks)

(defun hb--all-bookmarks ()
  "Returns all files containing bookmarks, and the bookmarks positions."
  (-map #'(lambda (e)
            (let ((param-list (cdr e)))
              (cons (f-expand (map-elt param-list 'filename))
                    (map-elt param-list 'position))))
        bookmark-alist))

(defun hb--current-buffers-bookmarks ()
  "Returns the positions of bookmarks in the current buffer."
  (interactive)
  (let ((current-file (buffer-file-name)))
    (-select #'(lambda (o)
                 (string= current-file (car o)))
             (hb--all-bookmarks))))

;; TODO: make into minor-mode, use jit-lock
;;;###autoload
(defun highlight-bookmarks-in-this-buffer (&rest _args)
  (interactive)

  ;; clear previous bookmark highlights, to avoid double-drawing
  (ov-clear 'highlight-bookmarks-highlight)

  ;; grab bookmarks from `bookmark-alist', and create an overlay for all
  ;; bookmarks that belong to the current buffer
  (-map #'(lambda (b)
            (ov-set (ov-line (cdr b))
                    'face highlight-bookmarks-bookmark-face
                    'highlight-bookmarks-highlight t))
        (hb--current-buffers-bookmarks)))

(provide 'highlight-bookmarks)
;;; highlight-bookmarks.el ends here
