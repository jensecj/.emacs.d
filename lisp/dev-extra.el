;;; dev-extra.el --- Convenience functions for lisp maintainers. -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Jens Christian Jensen

;; Author: Jens Christian Jensen <jensecj@gmail.com>
;; Keywords: elisp, packages
;; Package-Version: 20200505
;; Version: 0.2.0
;; Package-Requires: ((emacs "28.0.0.5"))

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

(require 'lisp-mnt)
(require 's)
(require 'dash)

(defun dev--get-header (header)
  "Return content of HEADER if it exists."
  (save-excursion
    (lm-header header)))

(defun dev--set-header (header new)
  "Set HEADER's contents to NEW."
  (save-excursion
    (when (lm-header header)
      (kill-line)
      (insert new))))

(defun dev--edit-header (header)
  "Edit HEADER interactively."
  ;; TODO: create new header if it does not exist?
  (when-let* ((contents (dev--get-header header))
              (new (completing-read (format "%s: " header) nil nil nil contents)))
    (dev--set-header header new)))

(defun dev--crack-version (ver)
  "Convert a version string into a list of numbers."
  (->> ver
       (s-split (regexp-quote "."))
       (-map #'string-to-number)))

(defun dev--make-version (ver loc)
  "Bump the LOC part of version VER."
  (when-let ((ver (dev--crack-version ver)))
    ;; make sure the version is in three parts (major.minor.patch)
    (while (< (length ver) 3)
      (setq ver (-snoc ver 0)))

    (cond
     ((equal loc 'patch) (format "%s.%s.%s" (nth 0 ver) (nth 1 ver) (1+ (nth 2 ver))))
     ((equal loc 'minor) (format "%s.%s.%s" (nth 0 ver) (1+ (nth 1 ver)) 0))
     ((equal loc 'major) (format "%s.%s.%s" (1+ (nth 0 ver)) 0 0)))))

(defun dev-edit-version ()
  "Edit the Version header interactively."
  (interactive)
  (dev--edit-header "version"))

(defun dev-bump-patch ()
  "Bump the patch part of the Version header."
  (interactive)
  (when-let ((ver (dev--get-header "version")))
    (dev--set-header "version" (dev--make-version ver 'patch))))

(defun dev-bump-minor ()
  "Bump the minor part of the Version header."
  (interactive)
  (when-let ((ver (dev--get-header "version")))
    (dev--set-header "version" (dev--make-version ver 'minor))))

(defun dev-bump-major ()
  "Bump the major part of the Version header."
  (interactive)
  (when-let ((ver (dev--get-header "version")))
    (dev--set-header "version" (dev--make-version ver 'major))))

(defun dev-update-package-version ()
  "Update the Package-Version header to the current date in compact format."
  (interactive)
  (when (dev--get-header "package-version")
    (dev--set-header "package-version" (format-time-string "%Y%m%d"))))

(defun dev-edit-package-version ()
  "Edit the Package-Version header."
  (interactive)
  (dev--edit-header "package-version"))

(defun dev-update-copyright ()
  "Update the Copyright header to the current year."
  (interactive)
  (let ((copyright-regex (rx bol ";; Copyright (C) " (group (1+ digit) (optional "-" (1+ digit))))))
    (save-excursion
      (ignore-errors
        (goto-char (point-min))
        (regexp-search-forward copyright-regex)
        (replace-match (format-time-string "%Y") nil nil nil 1)))))

(defun dev-edit-url ()
  "Edit the URL header."
  (interactive)
  (dev--edit-header "url"))

(defun dev-edit-keywords ()
  "Edit the Keywords header."
  (interactive)
  (dev--edit-header "keywords"))

(defun dev-edit-requires ()
  "Edit the Package-Requires header."
  (interactive)
  (dev--edit-header "package-requires"))

(defun dev-fixup ()
  "Update common headers which go out-of-date."
  (interactive)
  (dev-update-package-version)
  (dev-update-copyright))


(provide 'dev-extra)
