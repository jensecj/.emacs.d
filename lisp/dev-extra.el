;;; dev-extra.el --- Convenience functions for lisp maintainers. -*- lexical-binding: t; -*-

;; Copyright (C) 2018, 2019 Jens Christian Jensen

;; Author: Jens Christian Jensen <jensecj@gmail.com>
;; Keywords: library, maintenance
;; Package-Version: 20190610
;; Version: 0.1.1

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
(require 'hydra)

(defun dev--header-exists (header)
  "Return content of header if it exists."
  (save-excursion
    (goto-char (point-min))
    (lm-header header)))

(defun dev--crack-version (ver)
  "Convert a version string into a list of numbers."
  (->> ver
       (s-split (regexp-quote "."))
       (-map #'string-to-number)))

(defun dev--bump-version (ver loc)
  "Bump the LOC part of version VER."
  (when-let* ((ver (dev--crack-version ver)))
    ;; make sure the version is in three parts (major.minor.patch)
    (while (< (length ver) 3)
      (setq ver (-snoc ver 0)))

    (cond
     ((equal loc 'patch) (format "%s.%s.%s" (nth 0 ver) (nth 1 ver) (1+ (nth 2 ver))))
     ((equal loc 'minor) (format "%s.%s.%s" (nth 0 ver) (1+ (nth 1 ver)) 0))
     ((equal loc 'major) (format "%s.%s.%s" (1+ (nth 0 ver)) 0 0)))))

(defun dev--update-version (ver)
  "Update the package version to VER."
  (save-excursion
    (lm-header "version")
    (kill-line)
    (insert ver)))

(defun dev-bump-patch ()
  "Bump the patch part of the version header."
  (interactive)
  (when-let ((ver (dev--header-exists "version")))
    (dev--update-version (dev--bump-version ver 'patch))))

(defun dev-bump-minor ()
  "Bump the minor part of the version header."
  (interactive)
  (when-let ((ver (dev--header-exists "version")))
    (dev--update-version (dev--bump-version ver 'minor))))

(defun dev-bump-major ()
  "Bump the major part of the version header."
  (interactive)
  (when-let ((ver (dev--header-exists "version")))
    (dev--update-version (dev--bump-version ver 'major))))

(defhydra dev-hydra ()
  "Hydra for bumping package version"
  ("u" #'dev-update-package-version "update package-version")
  ("p" #'dev-bump-patch "bump patch")
  ("m" #'dev-bump-minor "bump minor")
  ("M" #'dev-bump-major "bump major"))

(defun dev-update-package-version ()
  "Update the package-version to current date in compact format."
  (interactive)
  (when (dev--header-exists "package-version")
    (save-excursion
      (lm-header "package-version")
      (kill-line)
      (insert (format-time-string "%Y%m%d")))))

(defun dev-insert-mode-header ()
  ""
  (interactive)
  (goto-char (point-min))
  (insert "-*- mode: org -*-"))

(defun dev-new-package ()
  "Insert standard library header at the top of the current buffer."
  (interactive)
  (goto-char (point-min))
  (insert
   (format
    ";;; %s --- Some description. -*- lexical-binding: t; -*-

;; Copyright (C) %s %s

;; Author: %s <%s>
;; URL:
;; Keywords:
;; Package-Requires ((%s))
;; Package-Version: %s
;; Version: 0.1.0


;;; Commentary:
;;

;;; Code:


(provide '%s)
;;; %s ends here
"
    (buffer-name)
    (format-time-string "%Y")
    (user-full-name)
    (user-full-name)
    user-mail-address
    (format "emacs \"%s\"" emacs-version)
    (format-time-string "%Y%m%d")
    (f-base (buffer-name))
    (buffer-name))))

(provide 'dev-extra)
