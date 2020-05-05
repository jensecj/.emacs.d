;;; contrib.el --- Extras for elisp. -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Jens Christian Jensen

;; Author: Jens Christian Jensen <jensecj@gmail.com>
;; URL:
;; Keywords:
;; Package-Requires ((emacs "28.0.50"))
;; Package-Version: 20200502
;; Version: 0.1.0


;;; Commentary:
;; For things that should be in emacs, but for some reason are not.


;;; Code:

;;;; convenience aliases

(defalias '@ 'progn)

;;;; macros


;;;; functions

(defun -mapcar (fn &rest lists)
  "Map FN to the car of each list in LISTS."
  (cond
   ((null lists) nil)
   ((null (car lists)) nil)
   ((listp lists)
    (cons
     (apply fn (-map #'car lists))
     (apply #'-mapcar fn (-map #'cdr lists))))))

(cl-defmacro bench (&optional (times 100000) &rest body)
  "Call `benchmark-run-compiled' on BODY with TIMES iterations,
returning list suitable for Org source block evaluation.  Garbage
is collected before calling `benchmark-run-compiled' to avoid
counting existing garbage which needs collection."
  (declare (indent defun))
  `(progn
     (garbage-collect)
     (list '("Total runtime" "# of GCs" "Total GC runtime")
           'hline
           (benchmark-run-compiled ,times
             (progn
               ,@body)))))


(provide 'contrib)
;;; contrib.el ends here
