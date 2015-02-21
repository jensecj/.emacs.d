;;; botcode-mode.el --- mode for editing botcode assembler code

;;; Commentary:

;;; Code:

(defgroup botcode nil
  "Mode for editing botcode assembler code."
  :group 'languages)

(defcustom botcode-comment-char ?\#
  "The 'comment-start' character assumed by botcode mode."
  :type 'character
  :group 'botcode)

(defvar botcode-mode-syntax-table nil
  "Syntax table used while in botcode mode.")

(defvar botcode-mode-abbrev-table nil
  "Abbrev table used while in botcode mode.")
(define-abbrev-table 'botcode-mode-abbrev-table ())

(defvar botcode-mode-map nil
  "Keymap for botcode mode.")

(if botcode-mode-map
    nil
  (setq botcode-mode-map (make-sparse-keymap))
  (define-key botcode-mode-map "\C-i" 'botcode-indent)
  (define-key botcode-mode-map "\C-c\C-c" 'botcode-compile)
  (define-key botcode-mode-map "\C-c\C-r" 'botcode-run)
  )

(defconst botcode-font-lock-keywords
  `(
    (":\\([a-zA-Z_0-9]\\)*" . font-lock-keyword-face) ; label definitions
    ("\\(pop\\|push\\|inc\\|dec\\|in\\|out\\|zero\\)[ ]" . font-lock-function-name-face) ; functions
    ("\\(ax\\|bx\\|cx\\|pc\\)[ ,\n]" . font-lock-constant-face) ; registers
    ("[0-9]+" . font-lock-builtin-face) ; number literals
    ("\\([a-z_0-9]\\)*" . font-lock-keyword-face) ; labels in use
    ))

(defvar botcode-code-level-empty-comment-pattern nil)
(defvar botcode-flush-left-empty-comment-pattern nil)
(defvar botcode-inline-empty-comment-pattern nil)

;;;###autoload
(defun botcode-mode ()
  "Major mode for BotCode."
  (interactive)
  (kill-all-local-variables)
  (setq mode-name "BotCode")
  (setq major-mode 'botcode-mode)
  (setq local-abbrev-table botcode-mode-abbrev-table)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(botcode-font-lock-keywords))
  (make-local-variable 'botcode-mode-syntax-table)
  (setq botcode-mode-syntax-table (make-syntax-table))
  (set-syntax-table botcode-mode-syntax-table)
  (setq to-column nil)
  (setq indent-line-function 'botcode-indent)

  (run-hooks 'botcode-mode-set-comment-hook)

  (set (make-local-variable 'compile-command)
       (format "java Compiler %s" (buffer-file-name)))

  ;; Make our own local child of botcode-mode-map so we can define our own comment character.
  (use-local-map (nconc (make-sparse-keymap) botcode-mode-map))

  (modify-syntax-entry	botcode-comment-char "<" botcode-mode-syntax-table)
  (modify-syntax-entry	?\n ">" botcode-mode-syntax-table)

  (make-local-variable 'comment-end)
  (setq comment-end "")

  (let ((cs (regexp-quote (char-to-string botcode-comment-char))))
    (make-local-variable 'comment-start)
    (setq comment-start (concat (char-to-string botcode-comment-char) " "))
    (make-local-variable 'comment-start-skip)
    (setq comment-start-skip (concat cs "+[ \t]*"))
    (setq botcode-inline-empty-comment-pattern (concat "^.+" cs "+ *$"))
    (setq botcode-code-level-empty-comment-pattern (concat "^[\t ]+" cs cs " *$"))
    (setq botcode-flush-left-empty-comment-pattern (concat "^" cs cs cs " *$"))
    )

  (run-hooks 'botcode-mode-hook))

(defun botcode-indent ()
  "Indent all text to the left."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (looking-at "[ \t]+")
        (delete-horizontal-space))
    )
  )

(defun botcode-compile ()
  (interactive)
  (let
      ((default-directory
         (file-name-directory
          (directory-file-name
           (file-name-directory
            (buffer-file-name))))))
    (call-interactively 'compile)))

(defun botcode-run ()
  (interactive)
  (let
      ((default-directory
         (file-name-directory
          (directory-file-name
           (file-name-directory
            (buffer-file-name))))))
    (start-process
     "Assembly Robo Wars"
     nil
     "java"
     "Gui"
     (file-name-directory (buffer-file-name)))))

(provide 'botcode-mode)

;;; botcode-mode.el ends here
