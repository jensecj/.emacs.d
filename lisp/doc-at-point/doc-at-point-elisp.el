(require 'help)
(require 'help-fns)
(require 's)

(require 'doc-at-point-core)

(defun doc-at-point-elisp--format-documentation (str)
  "Cleanup elisp doc-strings."
  (->> str
       (s-replace "For more information check the manuals.\n\n" "")
       (s-trim)))

(defmacro doc-at-point-elisp--capture-to-string (&rest body)
  "Capture output written to `standard-output' by help functions
and others, and return as string."
  `(with-temp-buffer
     (let ((standard-output (current-buffer))
           (help-xref-following t))
       (setq major-mode 'help-mode)
       (progn ,@body)
       (doc-at-point-elisp--format-documentation (buffer-string)))))

(defun doc-at-point-elisp--describe-function (symbol)
  "Return documentation for elisp function."
  (doc-at-point-elisp--capture-to-string
   (princ symbol)
   (princ " is ")
   (describe-function-1 symbol)))

(defun doc-at-point-elisp--describe-variable (symbol)
  "Return documentation for elisp variable."
  (doc-at-point-elisp--capture-to-string
   (let ((file-name  (find-lisp-object-file-name symbol 'defvar)))
     (princ symbol)
     (princ " is a variable")
     (when file-name
       (princ " defined in `")
       (princ (if (eq file-name 'C-source)
                  "C source code"
                (file-name-nondirectory file-name)))
       (princ "'."))
     (princ "\n\n")
     (princ (or (documentation-property symbol 'variable-documentation t)
                "no documentation.")))))

(defun doc-at-point-elisp--describe-face (sym)
  "Return documentation for elisp face."
  (doc-at-point-elisp--capture-to-string
   (let ((file-name  (find-lisp-object-file-name symbol 'defface)))
     (princ symbol)
     (princ " is a face ")
     (when file-name
       (princ " defined in `")
       (princ (if (eq file-name 'C-source)
                  "C source code"
                (file-name-nondirectory file-name)))
       (princ "'."))
     (princ "\n\n")
     (princ (or (documentation-property symbol 'face-documentation t)
                "no documentation.")))))

(defun doc-at-point-elisp--describe-group (symbol)
  "Return documentation for elisp group."
  (doc-at-point-elisp--capture-to-string
   (let ((doc (documentation-property symbol 'group-documentation t)))
     (when doc
       (princ symbol)
       (princ " is a group.\n\n")
       (princ doc)))))

(defun doc-at-point-elisp (symbol)
  "Return documentation for elisp symbol."
  (if (stringp symbol)
      (setq symbol (intern-soft symbol)))

  (ignore-errors
    (cond
     ((fboundp symbol) (doc-at-point-elisp--describe-function symbol))
     ((boundp symbol) (doc-at-point-elisp--describe-variable symbol))
     ((facep symbol) (doc-at-point-elisp--describe-face symbol))
     (t (doc-at-point-elisp--describe-group symbol)))))

(defun doc-at-point-elisp-cached (symbol)
  "Return documentation for elisp symbol. if `symbol' is nil,
return documentation for last symbol that was looked up."

  (if (not (boundp 'doc-at-point-elisp-symbol-cache))
      (make-local-variable 'doc-at-point-elisp-symbol-cache))

  (if symbol
      (doc-at-point-elisp symbol)
    (doc-at-point-elisp doc-at-point-elisp-symbol-cache)))

;; register the default elisp handler for doc-at-point
(doc-at-point-register
 :mode 'emacs-lisp-mode
 :symbol-fn #'symbol-at-point
 :doc-fn #'doc-at-point-elisp)

(provide 'doc-at-point-elisp)