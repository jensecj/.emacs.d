;;; -*- lexical-binding: t -*-

(require 'f)

(defun today-fs-file-from-date (date)
  "Returns the path to the file corresponding to DATE."
  (f-join today-directory date (concat date ".org")))

(defun today-fs-create-planning-file (filepath)
  "Creates a planning file with FILEPATH."
  ;; create the planning directory if it does not exist
  (unless (f-exists? today-directory)
    (f-mkdir today-directory))

  (let ((dir (f-dirname filepath)))
    ;; create directory of FILEPATH if it does not exist
    (unless (f-exists? dir)
      (f-mkdir dir)))

  ;; finally, create the file of FILEPATH
  (unless (f-exists? filepath)
    (f-touch filepath)))

(defun today-fs-buffer-from-date (date)
  "Get the buffer for DATEs file, if the buffer does not exist,
then visit the corresponding file, if the file does not exist,
then create it."
  (let ((file (today-fs-file-from-date date)))
    (today-fs-create-planning-file file)
    (find-file-noselect file)))

(defun today-fs-visit-date-file (date)
  "Visit the file for DATE, create it if it does not exist."
  (switch-to-buffer (today-fs-buffer-from-date date)))

(provide 'today-fs)
