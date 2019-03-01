;;; -*- lexical-binding: t -*-

(require 'f)

(defun today-fs-file-from-date (date)
  "Returns the path to the file corresponding to DATE."
  (f-join today-directory date (concat date ".org")))

(defun today-fs-visit-date-file (date)
  "Visit the file for DATE, create it if it does not exist."
  (let ((date-file (today-fs-file-from-date date)))
    (find-file date-file)))

(provide 'today-fs)
