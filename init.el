;;; -*- lexical-binding: t -*-
;; use lexical binding for initialization code
(setq-default lexical-binding t)

(defun msg-info (txt) (message "# %s" txt))
(defun msg-warning (txt) (message "! %s" txt))
(defun msg-success (txt) (message "@ %s" txt))

(msg-info "Started initializing emacs!")

;; Turn off excess interface early in startup to avoid momentary display
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; directories for things related to emacs
(defconst my-emacs-dir user-emacs-directory)
(defconst my-emacs-elpa-dir (concat my-emacs-dir "elpa"))
(defconst my-emacs-lisp-dir (concat my-emacs-dir "lisp/"))
(defconst my-emacs-modes-dir (concat my-emacs-dir "modes/"))
(defconst my-emacs-temp-dir (concat my-emacs-dir ".temp/"))
(defconst my-emacs-data-dir (concat my-emacs-dir "data/")) ;; config and cache files
(defconst my-emacs-backup-dir (concat my-emacs-data-dir "backups/")) ;; backups, auto saves, etc.

;; create them if they don't exist
(unless (file-exists-p my-emacs-lisp-dir)
  (make-directory my-emacs-lisp-dir))
(unless (file-exists-p my-emacs-modes-dir)
  (make-directory my-emacs-modes-dir))
(unless (file-exists-p my-emacs-temp-dir)
  (make-directory my-emacs-temp-dir))
(unless (file-exists-p my-emacs-data-dir)
  (make-directory my-emacs-data-dir))
(unless (file-exists-p my-emacs-backup-dir)
  (make-directory my-emacs-backup-dir))

;; add the lispy directories to the load-path
(add-to-list 'load-path my-emacs-lisp-dir)
(add-to-list 'load-path my-emacs-modes-dir)
(add-to-list 'load-path my-emacs-elpa-dir)

;; setup package archives
(setq-default package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                                 ("melpa-stable" . "https://stable.melpa.org/packages/")
                                 ("melpa" . "https://melpa.org/packages/")
                                 ("org" . "https://orgmode.org/elpa/")))

;; install use-package if we don't already have it
(unless (package-installed-p 'use-package)
  (msg-warning "use-package is not installed. installing...")
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; make use-package tell us what its doing
(use-package use-package
  :config
  (setq use-package-verbose t)
  (setq use-package-enable-imenu-support t)
  (setq use-package-compute-statistics t))

;;; download, setup, and load straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; some libraries that are frequently used
(use-package dash ;; functional things, -map, -fold, etc
  :straight t
  :commands (-some -remove))
(use-package dash-functional :straight t :demand t)

(use-package s ;; string manipulations
  :straight t
  :commands (s-trim s-prefix? s-replace))

(use-package f ;; handling the file-system
  :straight t
  :ensure t
  :commands (f-exists? f-glob f-no-ext))

;; Use =Source Code Pro= font if it is available. When launching emacs as a
;; daemon, fonts are not loaded until we actually produce a frame, so the
;; font list will be empty, focus-in-hook is run when a frame is created,
;; whether by a user or a daemon, the first frame created will not have
;; the setup, as it is created before this is run, still looking into
;; this.
(defun jens/init-fonts ()
  "Setup fonts, then remove self from `focus-in-hook' so we only run once."
  (let ((my-font "Source Code Pro Semibold 10"))
    (if (find-font (font-spec :name my-font))
        (progn
          (add-to-list 'default-frame-alist `'(font . ,my-font))
          (set-frame-font my-font))
      (msg-warning (format "could not find font: %s" my-font)))))

(add-hook 'server-after-make-frame-hook 'jens/init-fonts)

;;;;;;;;;;;;;;;;;;;;;;
;; temp files, etc. ;;
;;;;;;;;;;;;;;;;;;;;;;

;; try to place all the extra files emacs creates into suitable folders

;; Save temp files in the =.temp= folder
(setq temporary-file-directory my-emacs-temp-dir)
(setq bookmark-default-file (concat my-emacs-data-dir "bookmarks"))
;; the session persistent cache, used by unicode-fonts, etc.
(setq pcache-directory (concat my-emacs-data-dir "pcache/"))

;; Save backup and auto-save files in the =data= folder.
(setq backup-directory-alist `((".*" . ,my-emacs-backup-dir)))
(setq auto-save-file-name-transforms `((".*" ,my-emacs-backup-dir t)))
(setq auto-save-list-file-prefix my-emacs-backup-dir)

;; Keep emacs custom settings in a separate file, and load it if it exists.
(setq custom-file (concat my-emacs-dir "custom.el"))
(if (file-exists-p custom-file)
    (load custom-file))

;;;;;;;;;;;;;;
;; settings ;;
;;;;;;;;;;;;;;

;; location of emacs source files
(setq source-directory "/home/jens/.aur/emacs-git-src/")

;; Hide the splash screen
(setq inhibit-startup-message t)

;; set the paranoia level to medium, warns if connections are insecure
(setq network-security-level 'medium)

;; enable useful disabled features
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; load newer files, even if they have (outdated) byte-compiled counterparts
(setq load-prefer-newer t)

;; Don't blink the cursor
(blink-cursor-mode -1)

;; Highlight current line, with a sane color, and transparent foreground
;; (so it does not mess with syntax highlighting)
(global-hl-line-mode 1)
(set-face-background 'hl-line "gray30")
(set-face-foreground 'highlight nil)
(set-face-underline 'highlight nil)

;; Allow pasting selection outside of Emacs
(setq select-enable-clipboard t)

;; Show keystrokes in progress
(setq echo-keystrokes 0.1)

;; Move files to trash when deleting
(setq delete-by-moving-to-trash t)

;; don't use shift to mark things
(setq shift-select-mode nil)

;; Transparently open compressed files
(auto-compression-mode t)

;; Enable syntax highlighting for older Emacsen that have it off
(global-font-lock-mode t)

;; Answering just 'y' or 'n' will do
(defalias 'yes-or-no-p 'y-or-n-p)

;; use UTF-8
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Make backups of files, even when they're in version control, and set
;; how many backups we want to save for each file.
(setq make-backup-files t
      vc-make-backup-files t
      version-control t
      delete-old-versions t
      kept-old-versions 9
      kept-new-versions 9
      auto-save-default t)

;; Show active region
(transient-mark-mode 1)

;; Remove text in active region if inserting text
(delete-selection-mode 1)

;; display line and column numbers in mode-line
(setq line-number-mode t)
(setq column-number-mode t)

;; make the mode-line not look weird
(set-face-attribute 'mode-line nil :box nil)
(set-face-attribute 'mode-line-inactive nil :box nil)

;; Lines should be 80 characters wide, not 72
(setq-default fill-column 80)

;; Undo/redo window configuration with C-c <left>/<right>
(winner-mode 1)

;; use spaces instead of tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
;; this messes with less things when indenting,
;; tabs are converted to spaces automatically
(setq-default indent-line-function 'insert-tab)

;; Show me empty lines after buffer end
(setq-default indicate-empty-lines t)

;; Don't automatically break lines
(setq-default truncate-lines t)

;; Allow recursive mini buffers
(setq enable-recursive-minibuffers t)

;; show everything that's happening when evaluating somethingx
(setq eval-expression-print-level nil)

;; End files in newlines
(setq require-final-newline nil)

;; Save before compiling, dont ask
(setq compilation-ask-about-save nil)

;; save more things in the kill ring
(setq kill-ring-max 500)

;; keep a lot more undo history (expressed in bytes)
(setq undo-limit (* 500 1000))
(setq undo-strong-limit (* 800 1000))

(setq message-log-max 10000)

(setq auto-window-vscroll nil)
;; if moving the poijt more than 10 lines away,
;; center point in the middle of the window, otherwise be conservative.
(setq scroll-conservatively 10)
;; only scroll the current line when moving outside window-bounds
(setq auto-hscroll-mode 'current-line)

(setq initial-scratch-message "")

;; used to be transpose-words, used for new keybindings
(global-unset-key (kbd "M-t"))

;; don't show trailing whitespace by default
(setq-default show-trailing-whitespace nil)
(defun jens/show-trailing-whitespace ()
  "Show trailing whitespace in buffer."
  (interactive)
  (setq show-trailing-whitespace t))

(add-hook 'text-mode-hook 'jens/show-trailing-whitespace)
(add-hook 'prog-mode-hook 'jens/show-trailing-whitespace)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; authentication and security ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; gpg and auth
(use-package epa-file
  :demand t
  :commands epa-file-enable
  :config
  (setq epg-pinentry-mode 'loopback)
  (epa-file-enable))

;; enable gpg pinentry through the minibuffer
(use-package pinentry
  :ensure t
  :demand t
  :commands (pinentry-start pinentry-stop)
  :config
  (setenv "GPG_AGENT_INFO" nil)

  (defun jens/pinentry-reset ()
    "Reset the `pinentry' service."
    (interactive)
    (pinentry-stop)
    (pinentry-start))

  (jens/pinentry-reset)

  (setq gpg-reset-timer (run-with-timer 0 (* 60 45) #'jens/pinentry-reset))
  ;; (cancel-timer gpg-reset-timer)
  )

(setq auth-sources
      '("~/vault/authinfo.gpg" "~/.netrc"))

(defun jens/kill-idle-gpg-buffers ()
  "Kill .gpg buffers after they have not been used for 60 seconds."
  (interactive)
  (let ((buffers-killed 0))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (string-match ".*\.gpg$" (buffer-name buffer))
          (let ((current-time (second (current-time)))
                (last-displayed-time (second buffer-display-time)))
            (when (> (- current-time last-displayed-time) 60)
              (message "killing %s" (buffer-name buffer))
              (when (buffer-modified-p buffer)
                (save-buffer))
              (kill-buffer buffer)
              (incf buffers-killed))))))
    (unless (zerop buffers-killed)
      (message "%s .gpg buffers have been saved and killed" buffers-killed))))

(setq jens/kill-idle-gpg-buffers-timer (run-with-idle-timer 60 t 'jens/kill-idle-gpg-buffers))

;;;;;;;;;;;;;;;;;;;;
;; editing defuns ;;
;;;;;;;;;;;;;;;;;;;;

(defun jens/new-scratch-buffer ()
  "Return a newly created scratch buffer."
  (let ((n 0)
        bufname)
    (while (progn
             (setq bufname
                   (concat "*scratch"
                           (if (= n 0) "" (format "-%s" (int-to-string n)))
                           "*"))
             (setq n (1+ n))
             (get-buffer bufname)))
    (get-buffer-create bufname)))

(defun jens/create-scratch-buffer nil
  "Create a new scratch buffer to work in.  (named *scratch* - *scratch<n>*)."
  (interactive)
  (switch-to-buffer (jens/new-scratch-buffer))
  (funcall initial-major-mode))

(defun jens/clean-view ()
  "Create a scratch buffer, and makes it the only buffer visible."
  (interactive)
  (jens/create-scratch-buffer)
  (delete-other-windows))

(defun jens/clone-buffer ()
  "Open a clone of the current buffer."
  (interactive)
  (let ((newbuf (jens/new-scratch-buffer))
        (content (buffer-string))
        (oldpoint (point)))
    (with-current-buffer newbuf
      (insert content))
    (switch-to-buffer newbuf)
    (goto-char oldpoint)))

(defun jens/cleanup-buffer ()
  "Perform a bunch of operations on the white space content of a buffer.
   Including indent-buffer, which should not be called automatically on save."
  (interactive)
  (indent-region (point-min) (point-max))
  (whitespace-cleanup)
  (message "cleaned up"))

(defun jens/sudo-reopen ()
  "Re-open current buffer file with superuser permissions."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (when file-name
      (find-alternate-file (concat "/sudo::" file-name)))))

(defun jens/open-line-below ()
  "Inserts a line below the current line, indents it, and moves
   the the beginning of that line."
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))

(defun jens/open-line-above ()
  "Inserts a line above the current line, indents it, and moves
  the the beginning of that line."
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))

(defun jens/smart-beginning-of-line ()
  "Move point to the beginning of line or beginning of text."
  (interactive)
  (let ((pt (point)))
    (beginning-of-line-text)
    (when (eq pt (point))
      (beginning-of-line))))

(defun jens/kill-to-beginning-of-line ()
  "Kill from <point> to the beginning of the current line."
  (interactive)
  (kill-region (save-excursion (beginning-of-line) (point))
               (point)))

(defun jens/save-region-or-current-line (_arg)
  "If a region is active then it is saved to the `kill-ring',
otherwise the current line is saved."
  (interactive "P")
  (save-mark-and-excursion
    (if (region-active-p)
        (kill-ring-save (region-beginning) (region-end))
      (kill-ring-save (line-beginning-position) (+ 1 (line-end-position))))))

(defun jens/kill-region-or-current-line (arg)
  "If a region is active then it is killed, otherwise the current line is killed."
  (interactive "P")
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (save-excursion
      (kill-whole-line arg))))

(defun jens/join-region ()
  "Join all lines in a region into a single line."
  (interactive)
  (save-excursion
    (let ((beg (region-beginning))
          (end (copy-marker (region-end))))
      (goto-char beg)
      (while (< (point) end)
        (progn
          (join-line 1)
          (end-of-line))))))

(defun jens/join-region-or-line ()
  "If region is active, join all lines in region to a single
   line. Otherwise join the line below the current line, with the
   current line, placing it after."
  (interactive)
  (if (region-active-p)
      (jens/join-region)
    (join-line -1)))

(defun jens/wrap-region (b e text-begin text-end)
  "Surrounds region with given text."
  (interactive "r\nsStart text: \nsEnd text: ")
  (if (use-region-p)
      (save-restriction
        (narrow-to-region b e)
        (goto-char (point-max))
        (insert text-end)
        (goto-char (point-min))
        (insert text-begin))
    (message "wrap-region: Error! invalid region!")))

(defun jens/comment-uncomment-region-or-line ()
  "If region is active, comment or uncomment it (based on what it currently is),
otherwise comment or uncomment the current line."
  (interactive)
  (if (region-active-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (line-beginning-position) (line-end-position))))

;;;;;;;;;;;;;;;;;
;; file defuns ;;
;;;;;;;;;;;;;;;;;

(defun jens/byte-compile-this-file ()
  "Byte compile the current buffer."
  (interactive)
  (if (f-exists? (concat (f-no-ext (buffer-file-name)) ".elc"))
      (byte-recompile-file (buffer-file-name))
    (byte-compile-file (buffer-file-name))))

(defun jens/get-buffer-file-name+ext ()
  "Get the file name and extension of the file belonging to the current buffer."
  (file-name-nondirectory buffer-file-name))

(defun jens/get-buffer-file-name ()
  "Get the file name of the file belonging to the current buffer."
  (file-name-sans-extension (jens/get-buffer-file-name+ext)))

(defun jens/get-buffer-file-directory ()
  "Get the directory of the file belonging to the current buffer."
  (file-name-directory (buffer-file-name)))

(defun jens/file-age (file)
  "Returns the number of seconds since FILE was last modified."
  (float-time
   (time-subtract (current-time)
                  (nth 5 (file-attributes (file-truename file))))))

(defun jens/rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(defun jens/delete-current-buffer-file ()
  "Remove the file of the current buffer and kill the buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer)))
    (if (not (and filename (file-exists-p filename)))
        (message "no such file exists")
      (when (yes-or-no-p (format "really delete '%s'?" filename))
        (delete-file filename)
        (kill-buffer buffer)
        (message "deleted '%s'" filename)))))

(defun jens/touch-buffer-file ()
  "Touches the current buffer, marking it as dirty."
  (interactive)
  (insert " ")
  (backward-delete-char 1)
  (save-buffer))

;;;;;;;;;;;;;;;;;
;; lisp defuns ;;
;;;;;;;;;;;;;;;;;

;; easy 'commenting out' of sexps
(defmacro comment (&rest _args))

;; easy interactive lambda forms
(defmacro xi (lam)
  `(lambda ()
     (interactive)
     ,lam))

(defun jens/one-shot-keybinding (key command)
  "Set a keybinding that disappear once you press a key that is not in
the overlay-map"
  (set-transient-map
   (let ((map (make-sparse-keymap)))
     (define-key map (kbd key) command)
     map) t))
;; example
;; (jens/one-shot-keybinding "a" (xi (previous-line)))

(defun jens/one-shot-keymap (key-command-pairs)
  "Set a keybinding that disappear once you press a key that is not in
the overlay-map"
  (set-transient-map
   (let ((map (make-sparse-keymap)))
     (dolist (kvp key-command-pairs)
       (let ((key (car kvp))
             (cmd (cdr kvp)))
         (define-key map (kbd key) cmd)))
     map) t))
;; example:
;; (does not work yet, use explicit interactive lambda form)
;; (jens/one-shot-keymap
;;  '(("a" . (xi (message "a")))
;;    ("b" . (xi (message "b")))
;;    ("c" . (xi (message "c")))
;;    ("d" . (xi (message "d")))))

(defun jens/try-require (feature)
  "Try to require FEATURE, if an exception is thrown, log it."
  (condition-case ex
      (progn
        (msg-info (format "= Requiring \"%s\" " (symbol-name feature)))
        (require feature))
    ('error (msg-warning (format "@ Error requiring \"%s\": %s" (symbol-name feature) ex)))))

(defun jens/eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defun jens/save-to-file (data filename)
  "Save lisp object to a file."
  (with-temp-file filename
    (prin1 data (current-buffer))))

(defun jens/load-from-file (filename)
  "Load lisp object from file."
  (with-temp-buffer
    (insert-file-contents filename)
    (cl-assert (eq (point) (point-min)))
    (read (current-buffer))))

;;;;;;;;;;;;;;;;;
;; misc defuns ;;
;;;;;;;;;;;;;;;;;

(defun jens/is-online-p ()
  "Return a non-nil value if we have a network connection."
  (if (and (functionp 'network-interface-list)
           (network-interface-list))
      (-some (lambda (iface) (unless (equal "lo" (car iface))
                               (member 'up (first (last (network-interface-info
                                                         (car iface)))))))
             (network-interface-list))
    t))

(defun jens/insert-todays-date ()
  "Insert the current date at point."
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

(defun jens/processor-count ()
  "Return the number of logical processors of the system."
  (when (file-exists-p "/proc/cpuinfo")
    (with-temp-buffer
      (insert-file-contents "/proc/cpuinfo")
      (how-many "^processor[[:space:]]+:"))))

(defun jens/goto-msg-buffer ()
  "View the *Messages* buffer, return to previous buffer when done."
  (interactive)
  (view-buffer "*Messages*"))

;;;;;;;;;;;;;;
;; packages ;;
;;;;;;;;;;;;;;

;; We are going to use the bind-key (`:bind') and diminish (`:diminish')
;; extentions of `use-package', so we need to have those packages.
(use-package bind-key :ensure t)
(use-package diminish :ensure t :commands diminish)
(use-package delight :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; some built-in packages ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package simple
  :defines auto-fill-mode
  :diminish auto-fill-mode
  :commands (region-active-p
             use-region-p
             upcase-dwim
             downcase-dwim
             capitalize-dwim
             deactivate-mark
             current-kill)
  :hook (org-mode . auto-fill-mode))

(use-package abbrev
  :demand t
  :diminish abbrev-mode
  :hook (org-mode . abbrev-mode)
  :commands read-abbrev-file
  :config
  (setq abbrev-file-name (concat my-emacs-data-dir "abbreviations"))
  (read-abbrev-file))

;; Easily navigate silly cased words
(use-package subword
  :diminish subword-mode
  :commands global-subword-mode
  :config (global-subword-mode 1))

;; give buffers unique names
(use-package uniquify
  :config (setq uniquify-buffer-name-style 'forward))

(use-package tramp
  :defer t
  :config (setq tramp-persistency-file-name (concat my-emacs-data-dir "tramp")))

;; Save point position between sessions
(use-package saveplace
  :config
  (setq-default save-place t)
  (setq save-place-file (concat my-emacs-data-dir "saveplaces")))

;; Persist some vars across sessions
(use-package savehist
  :defer 2
  :commands savehist-mode
  :config
  (setq savehist-file (concat my-emacs-data-dir "savehist"))
  (setq savehist-autosave-interval 60) ;; save every minute
  (setq savehist-additional-variables '(kill-ring
                                        search-ring
                                        regexp-search-ring))
  ;; just keep all history
  (setq history-length t)
  (setq history-delete-duplicates t)
  (savehist-mode 1))

;; Save a list of recently visited files.
(use-package recentf
  :commands recentf-mode
  :config
  (setq recentf-save-file (recentf-expand-file-name (concat my-emacs-data-dir "recentf")))
  (setq recentf-exclude '(".emacs.d/elpa/" ".emacs.d/data/" "COMMIT_EDITMSG"))
  (setq recentf-max-saved-items 500) ;; just 20 is too few
  (setq recentf-auto-cleanup 300) ;; cleanup every 5 mins.
  ;; save recentf file every 30s, but don't bother us about it
  (setq recentf-auto-save-timer
        (run-with-idle-timer 30 t '(lambda ()
                                     (shut-up (recentf-save-list)))))

  (defun jens/recentf-cleanup (orig-fun &rest args)
    "Silence `recentf-auto-cleanup'."
    (shut-up (apply orig-fun args)))
  (advice-add 'recentf-cleanup :around #'jens/recentf-cleanup)

  (recentf-mode 1))

(use-package autorevert
  :diminish auto-revert-mode
  :commands global-auto-revert-mode
  :config
  ;; Also auto refresh dired, but be quiet about it
  (setq global-auto-revert-non-file-buffers t)
  (setq auto-revert-verbose nil)

  ;; Auto refresh buffers
  (global-auto-revert-mode 1))

;; Semantic analysis in supported modes (cpp, java, etc.)
(use-package semantic
  :disabled
  :defer t
  ;; :hook ((emacs-lisp-mode python-mode c++-mode java-mode) . semantic-mode)
  :config
  ;; persist the semantic parse database
  (setq semanticdb-default-save-directory (concat my-emacs-data-dir "semantic/"))
  (unless (file-exists-p semanticdb-default-save-directory)
    (make-directory semanticdb-default-save-directory))

  ;; save parsing results into a persistent database
  (global-semanticdb-minor-mode -1)
  ;; re-parse files on idle
  (global-semantic-idle-scheduler-mode -1)
  (semantic-mode -1))

(use-package display-line-numbers
  :defer t
  :commands display-line-numbers-mode
  :bind ("M-g M-g" . jens/goto-line-with-feedback)
  :config
  (defun jens/goto-line-with-feedback ()
    "Show line numbers temporarily, while prompting for the line number input"
    (interactive)
    (unwind-protect
        (progn
          (display-line-numbers-mode 1)
          (call-interactively 'goto-line))
      (display-line-numbers-mode -1))))

(use-package elisp-mode
  :delight
  (emacs-lisp-mode "Elisp" :major))

(use-package eldoc
  :diminish
  :config
  (setq eldoc-idle-delay 0)

  (defface eldoc-highlight-&s-face '((t ())) "")

  (defun jens/eldoc-highlight-&s (doc)
    "Highlight &keywords in elisp eldoc arglists."
    (condition-case nil
        (with-temp-buffer
          (insert doc)
          (goto-char (point-min))

          (while (re-search-forward "&optional\\|&rest\\|&key" nil 't)
            (set-text-properties (match-beginning 0) (match-end 0) (list 'face 'eldoc-highlight-&s-face)))

          (buffer-string))
      (error doc)))

  (advice-add #'elisp-eldoc-documentation-function :filter-return #'jens/eldoc-highlight-&s)

  (global-eldoc-mode +1)
  :custom-face
  (eldoc-highlight-function-argument ((t (:inherit font-lock-warning-face))))
  (eldoc-highlight-&s-face ((t (:inherit font-lock-preprocessor-face)))))

(use-package dired+
  :straight (dired+ :type git :host github :repo "emacsmirror/dired-plus")
  :after dired
  :demand t
  :commands (toggle-diredp-find-file-reuse-dir
             diredp-up-directory-reuse-dir-buffer)
  :bind
  (:map dired-mode-map
        ("<backspace>" . diredp-up-directory-reuse-dir-buffer)
        ("C-<down>" . nil)
        ("C-<up>" . nil))
  :config
  (toggle-diredp-find-file-reuse-dir +1)
  :custom-face
  (diredp-dir-priv ((t (:foreground "#8CD0D3"))))
  (diredp-file-name ((t (:foreground "#DCDCCC"))))
  (diredp-dir-name ((t (:foreground "#8CD0D3")))))

(use-package dired
  :defer t
  :commands dired
  :bind
  (("C-x C-d" . (lambda () (interactive) (dired default-directory)))
   :map dired-mode-map
   ("C-." . dired-omit-mode))
  :init
  :config
  ;; pull in extra functionality for dired
  (load-library "dired-x")
  (load-library "dired-aux")

  (setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))
  (setq dired-listing-switches "-agholXN")
  (setq-default dired-create-destination-dirs 'always)

  (setq ibuffer-formats
        '((mark modified read-only " "
                (name 60 -1 :left) " "
                (filename-and-process 70 -1))
          (mark " " (name 16 -1) " " filename)))

  (defun jens/dired-sort ()
    "Sort dired listings with directories first."
    (save-excursion
      (let (buffer-read-only)
        (forward-line 2) ;; beyond dir. header
        (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max)))
      (set-buffer-modified-p nil)))

  (advice-add 'dired-readin :after #'jens/dired-sort))

;; use firefox as the default browser
(use-package browse-url
  :defer t
  :config (setq browse-url-firefox-program "firefox"))

(use-package fringe
  :commands fringe-mode
  :config
  (fringe-mode '(5 . 0)))

;; I want to use the new version of org-mode from upstream.
;; remove the built-in org-mode from the load path, so it does not get loaded
(setq load-path (-remove (lambda (x) (string-match-p "org$" x)) load-path))
;; remove org-mode from the built-ins list, because we're using upstream
(setq package--builtins (assq-delete-all 'org package--builtins))

;; install upstream org-mode
(use-package org
  :ensure org-plus-contrib
  :pin org
  :defer t
  :commands (org-indent-region
             org-indent-line
             org-babel-do-load-languages
             org-context)
  :defines jens/load-org-agenda-files
  :bind
  (("C-x g " . org-agenda)
   :map org-mode-map
   ([(tab)] . nil)
   ;; unbind things that are used for other things
   ("C-a" . nil)
   ("<S-up>" . nil)
   ("<S-down>" . nil)
   ("<S-left>" . nil)
   ("<S-right>" . nil)
   ("<M-S-right>" . nil)
   ("<M-S-left>" . nil)
   ("<M-S-up>" . nil)
   ("<M-S-down>" . nil)
   ("<C-S-up>" . nil)
   ("<C-S-down>" . nil)
   ("C-c n" . jens/org-indent))
  :config
  (defun jens/org-indent ()
    (interactive)
    (if (region-active-p)
        (org-indent-region (region-beginning) (region-end)))
    (org-indent-line)
    (message "indented"))

  (defun jens/copy-org-url-at-point ()
    "Grab URL from org-link at point."
    (interactive)
    (let* ((link-info (assoc :link (org-context)))
           (text (when link-info
                   (buffer-substring-no-properties (cadr link-info) (caddr link-info)))))
      (if (not text)
          (error "Not in org link")
        (with-temp-buffer
          (string-match "\\[\\[.*\\]\\[" text)
          (insert (substring-no-properties text (+ (match-beginning 0) 2) (- (match-end 0) 2)))
          (clipboard-kill-ring-save (point-min) (point-max))))))

  ;; (defun jens/load-org-agenda-files ()
  ;;   (interactive)
  ;;   (setq org-agenda-files
  ;;         (append '("")
  ;;                 (f-glob "**/*.org" "~/vault/org/planning"))))

  ;; (advice-add 'org-agenda :before #'jens/load-org-agenda-files)

  ;; (org-babel-do-load-languages 'org-babel-load-languages
  ;;                              '((emacs-lisp . t)
  ;;                                (gnuplot . t)))

  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)
  ;; keep #+BEGIN_SRC blocks aligned with their contents
  (setq org-edit-src-content-indentation 0)
  ;; dont indent things
  (setq org-adapt-indentation nil)
  ;; syntax highlight org-mode code blocks when exporting as pdf
  ;; (setq-default org-latex-listings 'minted
  ;;               org-latex-packages-alist '(("" "minted"))
  ;;               org-latex-pdf-process
  ;;               '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
  ;;                 "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
  ;; try to get non-fuzzy latex fragments
  (plist-put org-format-latex-options :scale 1.6)
  ;; (setq org-latex-create-formula-image-program 'dvisvgm) ;; obsolete as of org 9.0
  (setq-default org-preview-latex-default-process 'dvisvgm)
  ;; use some noise in scheduling org-drills
  ;; (setq-default org-drill-add-random-noise-to-intervals-p t)
  )

(use-package ob-async
  :disabled
  :ensure t
  :defer t)

(use-package ob-clojure
  :disabled
  :requires cider
  :config
  (setq org-babel-clojure-backend 'cider))

(use-package ox-pandoc
  :ensure t
  :defer t)

(use-package org-ref
  :ensure t
  :defer t)

;;;;;;;;;;;;;;;;;
;; major modes ;;
;;;;;;;;;;;;;;;;;

;; built-ins
(use-package octave :mode ("\\.m\\'" . octave-mode))

(use-package sh-script
  :mode (("\\.sh\\'" . shell-script-mode)
         ("\\.zsh\\'" . shell-script-mode)
         ("\\.zshrc\\'" . shell-script-mode)
         ("\\.zshenv\\'" . shell-script-mode)
         ("\\.zprofile\\'" . shell-script-mode)
         ("\\.PKGBUILD\\'" . shell-script-mode)))

(use-package scheme
  :defer t
  :mode ("\\.scm\\'" . scheme-mode)
  :config (setq-default scheme-program-name "csi -:c"))

(use-package python
  :defer t
  :bind
  (:map python-mode-map
        ("C-c C-c" . nil)
        ("M-," . nil)
        ("M-." . nil)
        ("M--" . nil)))

;; homemade
(use-package botcode-mode
  :mode "\\.bot\\'")

;; from repos
(use-package cmake-mode :ensure t :mode "\\CmakeLists.txt\\'")
(use-package dockerfile-mode :ensure t :mode "\\Dockerfile\\'")
(use-package gitconfig-mode :ensure t :mode "\\.gitconfig\\'")
(use-package gitignore-mode :ensure t :mode "\\.gitignore\\'")
(use-package haskell-mode :ensure t :mode "\\.hs\\'")
(use-package lua-mode :ensure t :mode "\\.lua\\'")
(use-package markdown-mode :ensure t :mode ("\\.md\\'" "\\.card\\'"))
(use-package scss-mode :ensure t :mode "\\.scss\\'")

(use-package c++-mode
  :bind
  (:map c++-mode-map
        ("C-c n" . clang-format-buffer)
        ("C-c C-c" . compile)
        ("M-," . nil) ("M-." . nil) ("M--" . nil))
  :config
  (set (make-local-variable 'compile-command)
       (format "clang++ -std=c++17 -stdlib=libstdc++ %s -o %s"
               (jens/get-buffer-file-name+ext) (jens/get-buffer-file-name)))
  (setq c++-include-files
        '("/usr/include"
          "/usr/include/c++/7.3.0"
          "/usr/include/c++/7.3.0/backward"
          "/usr/include/c++/7.3.0/x86_64-unknown-linux-gnu"
          "/usr/lib/gcc/x86_64-unknown-linux-gnu/7.3.0/include"
          "/usr/lib/gcc/x86_64-unknown-linux-gnu/7.3.0/include-fixed"
          "/usr/lib/clang/5.0.1/include")))

(use-package rust-mode
  :ensure t
  :bind (:map rust-mode-map
              ("C-c n" . rust-format-buffer)
              ("M-," . nil) ("M-." . nil) ("M--" . nil))
  :mode "\\.rs\\'")

(use-package clojure-mode
  :ensure t
  :defer t
  :after (company-mode cider clj-refactor)
  :functions jens/company-clojure-quickhelp-at-point
  :commands (cider-create-doc-buffer
             cider-try-symbol-at-point)
  :bind
  (:map clojure-mode-map
        ("C-+" . jens/company-clojure-quickhelp-at-point)
        ("M-," . nil) ("M-." . nil) ("M--" . nil))
  :config
  (auto-complete-mode -1)
  (company-mode +1)
  (add-to-list 'company-backends 'company-capf)

  ;; clojure emacs power settings
  (cider-mode +1)
  (clj-refactor-mode +1)

  ;; (setq cider-cljs-lein-repl
  ;;       "(do (require 'figwheel-sidecar.repl-api)
  ;;        (figwheel-sidecar.repl-api/start-figwheel!)
  ;;        (figwheel-sidecar.repl-api/cljs-repl))")

  (defun jens/company-clojure-quickhelp-at-point ()
    (interactive)
    (cider-try-symbol-at-point "symbol to show doc for" #'cider-create-doc-buffer)
    (popup-tip (with-current-buffer "*cider-doc*"
                 (buffer-substring-no-properties (point-min) (point-max))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; extensions to major modes ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package cider
  :ensure t
  :defer t
  :hook (clojure-mode . cider-mode)
  :config
  (setq cider-repl-use-pretty-printing t
        cider-prompt-for-symbol nil
        cider-pprint-fn 'pprint
        cider-repl-pop-to-buffer-on-connect nil
        cider-default-cljs-repl nil
        cider-check-cljs-repl-requirements nil))

(use-package clj-refactor
  :ensure t
  :defer t
  :hook (clojure-mode . clj-refactor-mode)
  :config
  ;; dont warn on refactor evals
  (setq cljr-warn-on-eval nil))

(use-package geiser
  :ensure t
  :defer t
  :hook (scheme-mode . geiser-mode)
  :config
  (setq geiser-active-implementations '(chicken)))

(use-package racer
  :ensure t
  :defer t
  :after rust-mode
  :bind
  (:map racer-mode-map
        ("M-," . nil) ("M-." . nil) ("M--" . nil))
  :hook ((rust-mode . racer-mode)
         (racer-mode . eldoc-mode)))

(use-package elpy
  :ensure t
  :defer t
  :delight " elpy "
  :commands (elpy-goto-definition)
  :hook (python-mode . elpy-mode)
  :bind
  (:map elpy-mode-map
        ("<C-up>" . nil)
        ("<C-down>" . nil)
        ("C-c C-c" . nil)
        ("M-," . nil) ("M-." . nil) ("M--" . nil))
  :custom
  (elpy-modules
   '(elpy-module-sane-defaults
     elpy-module-company
     elpy-module-eldoc
     elpy-module-pyvenv)))

(use-package ggtags
  :ensure t
  :defer t
  :delight " ggtags "
  :hook (c++-mode . ggtags-mode)
  :bind
  (:map ggtags-mode-map
        ("M-," . nil) ("M-." . nil) ("M--" . nil))
  :custom-face
  (ggtags-highlight ((t ()))))

(use-package rtags :ensure t :defer t)
(use-package clang-format :ensure t :defer t)

;;;;;;;;;;;;;;;;;;;;;
;; auto completion ;;
;;;;;;;;;;;;;;;;;;;;;

;; COMPANY-MODE
(use-package company
  :ensure t
  :defer t
  :diminish company-mode
  :hook (emacs-lisp-mode . company-mode)
  :bind
  (("M-<tab>" . company-complete)
   :map company-active-map
   ("C-+" . jens/company-menu-at-selection-quickhelp))
  :config
  (setq company-search-regexp-function 'company-search-flex-regexp)
  (setq company-require-match nil)
  (setq company-show-numbers 't)
  (add-to-list 'company-backends 'company-capf)

  (defun jens/company-menu-at-selection-quickhelp ()
    (interactive)
    (let ((candidate (nth company-selection company-candidates)))
      (popup-tip (doc-at-point-elisp (intern candidate))
                 :margin-left 1 :margin-right 1))))

(use-package company-flx
  :ensure t
  :hook (company-mode . company-flx-mode))

(use-package company-quickhelp
  :ensure t
  :hook (company-mode . company-quickhelp-mode)
  :config
  (setq company-quickhelp-use-propertized-text 't)
  (setq company-quickhelp-delay 0.2))


;; AUTO-COMPLETE-MODE
(use-package ac-rtags :ensure t :defer t)
;; auto-complete source for c/c++ header files
(use-package ac-c-headers :disabled :ensure t :defer t)
;; auto-complete source for clang
(use-package ac-clang :disabled :ensure t :defer t)

;; auto-complete source for octave
(use-package ac-octave :disabled :ensure t :defer t)
;; auto-complete source for auctex
(use-package auto-complete-auctex :disabled :ensure t :defer t)
;; auto-completion source for scheme
(use-package scheme-complete :ensure t :defer t)

(use-package auto-complete
  :disabled
  :ensure t
  :diminish auto-complete-mode
  :commands (global-auto-complete-mode
             ac-symbol-documentation
             company-indent-or-complete-common)
  :functions (jens/ac-quick-help-at-point
              jens/ac-c++-mode-setup
              jens/ac-elisp-mode-setup)
  :bind
  (("C-+" . jens/ac-quick-help-at-point)
   ("M-<tab>" . auto-complete))
  :config
  (jens/try-require 'auto-complete-config)
  (setq ac-auto-start 't) ;; auto start completing
  ;; (setq ac-show-menu 't) ;; show the menu instantly
  (setq ac-use-menu-map 't)
  (setq ac-show-menu-immediately-on-auto-complete t) ;; show the autocompletion menu instantly
  (setq ac-delay 0.1) ;; show completion menu quickly
  (setq ac-use-quick-help 't) ;; use the help
  (setq ac-quick-help-delay 0.1) ;; show help quickly
  (setq ac-quick-help-prefer-pos-tip nil) ;; use popup-tips
  (setq ac-use-comphist t)
  (setq ac-comphist-file (concat my-emacs-data-dir "ac-history")) ;; move the history file
  (setq ac-ignore-case t)
  (setq-default ac-sources
                '(ac-source-imenu
                  ac-source-words-in-same-mode-buffers))

  (setq -quickhelp-at-point-cache ())
  (defun jens/ac-quick-help-at-point ()
    (interactive)
    (let* ((position (point))
           (string-under-cursor
            (buffer-substring-no-properties
             (progn (skip-syntax-backward "w_") (point))
             (progn (skip-syntax-forward "w_") (point)))))
      (goto-char position)

      (if (not (string= "" string-under-cursor))
          (setq -quickhelp-at-point-cache string-under-cursor))

      (popup-tip (ac-symbol-documentation (intern -quickhelp-at-point-cache))
                 :margin-left 1 :margin-right 1)))

  (defun jens/ac-c++-mode-setup ()
    (setq-default achead:include-directories c++-include-files)

    (jens/try-require 'ac-rtags)
    (add-to-list 'ac-sources 'ac-source-rtags)

    ;; (jens/try-require 'ac-clang)
    ;; (add-to-list 'ac-sources 'ac-source-clang)
    ;; (setq ac-clang-flags (mapcar (lambda (item)(concat "-I" item)) c++-include-files))
    ;; (ac-clang-activate-after-modify)

    ;; (jens/try-require 'ac-c-headers)
    ;; (add-to-list 'ac-sources 'ac-source-c-headers)
    ;; (add-to-list 'ac-sources 'ac-source-c-header-symbols t)

    ;; (add-to-list 'ac-sources 'ac-source-semantic)
    )

  (defun jens/ac-elisp-mode-setup ()
    (add-to-list 'ac-sources 'ac-source-functions) ;; elisp functions
    (add-to-list 'ac-sources 'ac-source-features) ;; elisp features
    (add-to-list 'ac-sources 'ac-source-symbols) ;; elisp symbols
    (add-to-list 'ac-sources 'ac-source-variables)) ;; elisp variables
  (add-hook 'emacs-lisp-mode-hook 'jens/ac-elisp-mode-setup)

  (global-auto-complete-mode -1)
  :custom-face
  (ac-candidate-face ((t (:foreground "#F0DFAF" :background "#313131"))))
  (ac-selection-face ((t (:foreground "#FEFEFE" :background "#3E3E3E")))))

;;;;;;;;;;;;;;;;;;;
;; misc packages ;;
;;;;;;;;;;;;;;;;;;;

(use-package flx :ensure t) ;; fuzzy searching for ivy, etc.
(use-package fzf :ensure t) ;; fuzzy file finder
(use-package rg :ensure t :commands (rg-read-pattern rg-project-root rg-default-alias rg-run)) ;; ripgrep in emacs
(use-package flycheck :disabled :ensure t :defer t)
(use-package git-timemachine :ensure t :defer t)
(use-package yasnippet :ensure t :defer t)
(use-package loccur :straight t)
(use-package paradox :ensure t :defer t) ;; improvements on package.el
(use-package org-ql :straight (org-ql :type git :host github :repo "alphapapa/org-ql") :defer t)
(use-package dumb-jump :ensure t :defer t)
(use-package counsel-tramp :ensure t :defer t)
(use-package centered-cursor-mode :ensure t :defer t)
(use-package with-editor :ensure t) ;; run commands in `emacsclient'
(use-package gist :ensure t :defer t) ;; work with github gists
(use-package rainbow-mode :ensure t :defer t :diminish) ;; highlight color-strings (hex, etc.)
(use-package ov :ensure t) ;; easy overlays
(use-package popup :ensure t)
(use-package shut-up :ensure t)

(use-package org-web-tools
  :ensure t
  :defer t
  :after s
  :config
  (require 'subr-x)

  (defun jens/website-title-from-url (url)
    "Get the website title from a url."
    (let* ((html (org-web-tools--get-url url))
           (title (org-web-tools--html-title html))
           (title (s-replace "[" "(" title))
           (title (s-replace "]" ")" title)))
      title))

  (defun jens/youtube-duration-from-url (url)
    "Get the duration of a youtube video, requires system tool `youtube-dl'."
    (let ((raw-duration (shell-command-to-string
                         (format "%s '%s'"
                                 "youtube-dl --get-duration"
                                 url))))
      (if (<= (length raw-duration) 10)
          (s-trim raw-duration)
        "?")))

  (defun jens/youtube-url-to-org-link (url)
    "Return an org-link from a youtube url, including video title
and duration."
    (let* ((title (jens/website-title-from-url url))
           (duration (jens/youtube-duration-from-url url))
           (title (replace-regexp-in-string " - YouTube$" "" title))
           (title (format "(%s) %s" duration title))
           (org-link (org-make-link-string url title)))
      org-link))

  (defun jens/youtube-url-to-org-link-at-point ()
    "Convert youtube-url-at-point to an org-link, including video
title and duration."
    (interactive)
    (save-excursion
      (if-let ((url (thing-at-point 'url)))
          (let ((bounds (bounds-of-thing-at-point 'url))
                (org-link (jens/youtube-url-to-org-link url)))
            (delete-region (car bounds) (cdr bounds))
            (goto-char (car bounds))
            (insert org-link))
        (error "no url at point")))))

(use-package help-fns+
  :init
  (let ((help-fns-plus-file (concat my-emacs-elpa-dir "/help-fns+.el"))
        (url "https://raw.githubusercontent.com/emacsmirror/emacswiki.org/master/help-fns%2B.el"))
    (unless (file-exists-p help-fns-plus-file)
      (url-copy-file url help-fns-plus-file))))

(use-package hydra
  :ensure t
  :commands (hydra-default-pre
             hydra-keyboard-quit
             hydra-show-hint
             hydra--call-interactively-remap-maybe
             hydra-set-transient-map)
  :bind (("C-x <left>" . buffer-carousel-hydra/body)
         (("C-x <right>" . buffer-carousel-hydra/body)))
  :config
  (defhydra buffer-carousel-hydra ()
    "Move betweet buffers."
    ("<left>" #'previous-buffer "previous")
    ("<right>" #'next-buffer "next")))

(use-package pdf-tools
  :straight t
  :defer t
  :commands pdf-tools-install
  :hook (doc-view-mode . pdf-tools-install)
  :bind
  ;; need to use plain isearch, pdf-tools hooks into it to handle searching
  (:map pdf-view-mode-map ("C-s" . isearch-forward))
  :config
  (pdf-tools-install))

(use-package amx
  :ensure t
  :commands amx-mode
  :config
  (setq amx-save-file (concat my-emacs-data-dir "amx-items"))
  (amx-mode))

(use-package smartparens
  :ensure t
  :demand t
  :diminish smartparens-mode
  :commands (show-smartparens-global-mode
             smartparens-global-mode)
  :bind (("M-<up>" .  sp-backward-barf-sexp)
         ("M-<down>" . sp-forward-barf-sexp)
         ("M-<left>" . sp-backward-slurp-sexp)
         ("M-<right>" . sp-forward-slurp-sexp)
         ("M-k" . sp-kill-sexp)
         ("M-S-K" . sp-copy-sexp)
         ("C-S-a" . sp-beginning-of-sexp)
         ("C-S-e" . sp-end-of-sexp)
         ("S-<next>" . sp-split-sexp)
         ("S-<prior>" . sp-join-sexp))
  :config
  (setq sp-show-pair-from-inside 't)
  (jens/try-require 'smartparens-config)
  (smartparens-global-mode +1)
  (show-smartparens-global-mode +1))

(use-package smart-jump
  :straight t
  :demand t
  :commands (smart-jump-register
             smart-jump-simple-find-references)
  :functions (jens/smart-jump-find-references-with-rg
              smart-jump-refs-search-rg
              jens/select-rg-window)
  :bind
  (("M-." . smart-jump-go)
   ("M-," . smart-jump-back)
   ("M--" . smart-jump-references))
  :config
  (defun jens/smart-jump-find-references-with-rg ()
    "Use `rg' to find references."
    (interactive)
    (if (fboundp 'rg)
        (progn
          (if (not (fboundp 'smart-jump-refs-search-rg))
              (rg-define-search smart-jump-refs-search-rg
                "Search for references for QUERY in all files in
                the current project."
                :dir project
                :files current))

          (smart-jump-refs-search-rg (cond ((use-region-p)
                                            (buffer-substring-no-properties (region-beginning)
                                                                            (region-end)))
                                           ((symbol-at-point)
                                            (substring-no-properties
                                             (symbol-name (symbol-at-point)))))))
      (message "Install `rg' to use `smart-jump-simple-find-references-with-rg'.")))

  (defun jens/select-rg-window nil
    (select-window (get-buffer-window (get-buffer "*rg*"))))

  (setq smart-jump-find-references-fallback-function #'jens/smart-jump-find-references-with-rg)

  (smart-jump-register :modes '(clojure-mode rust-mode))

  (smart-jump-register
   :modes '(emacs-lisp-mode lisp-interaction-mode)
   :jump-fn #'xref-find-definitions
   :pop-fn #'xref-pop-marker-stack
   :refs-fn #'xref-find-references
   :should-jump t
   :async 500
   :heuristic 'error)

  (smart-jump-register
   :modes 'python-mode
   :jump-fn #'elpy-goto-definition
   :pop-fn #'xref-pop-marker-stack
   :refs-fn #'smart-jump-simple-find-references
   :should-jump (lambda () (bound-and-true-p elpy-mode))
   :heuristic #'jens/select-rg-window)

  (smart-jump-register
   :modes 'c++-mode
   :jump-fn 'dumb-jump-go
   :pop-fn 'pop-tag-mark
   :refs-fn #'smart-jump-simple-find-references
   :should-jump t
   :heuristic #'jens/select-rg-window
   :order 3)

  (smart-jump-register
   :modes 'c++-mode
   :jump-fn 'ggtags-find-tag-dwim
   :pop-fn 'ggtags-prev-mark
   :refs-fn 'ggtags-find-reference
   :should-jump  (lambda () (bound-and-true-p ggtags-mode))
   :heuristic 'point
   :async 500
   :order 2)

  (smart-jump-register
   :modes 'c++-mode
   :jump-fn #'rtags-find-symbol-at-point
   :pop-fn #'rtags-location-stack-back
   :refs-fn #'rtags-find-all-references-at-point
   :should-jump (lambda ()
                  (and
                   (fboundp 'rtags-executable-find)
                   (fboundp 'rtags-is-indexed)
                   (rtags-executable-find "rc")
                   (rtags-is-indexed)))
   :heuristic 'point
   :async 500
   :order 1))

(use-package paxedit
  :ensure t
  :defer t
  :delight " paxedit "
  :hook ((lisp-mode . paxedit-mode)
         (lisp-interaction-mode . paxedit-mode)
         (emacs-lisp-mode . paxedit-mode)
         (clojure-mode . paxedit-mode)
         (scheme-mode . paxedit-mode))
  :commands (paxedit-transpose-forward
             paxedit-transpose-backward)
  :bind (("M-t" . paxedit-transpose-hydra/body)
         ("M-k" . paxedit-kill)
         ("M-K" . paxedit-copy)
         ("M-<prior>" . paxedit-backward-up)
         ("M-<next>" . paxedit-backward-end))
  :config
  (defhydra paxedit-transpose-hydra ()
    "Transpose things"
    ("f" #'paxedit-transpose-forward "forward")
    ("b" #'paxedit-transpose-backward "backward")))

(use-package diff-hl
  :ensure t
  :demand t
  :diminish diff-hl-mode
  :commands (global-diff-hl-mode
             diff-hl-mode
             diff-hl-next-hunk
             diff-hl-previous-hunk)
  :functions (jens/diff-hl-hydra/body jens/diff-hl-refresh)
  :bind ("C-c C-v" . jens/diff-hl-hydra/body)
  :hook
  ((magit-post-refresh . diff-hl-magit-post-refresh)
   (prog-mode . diff-hl-mode)
   (org-mode . diff-hl-mode)
   (dired-mode . diff-hl-dired-mode))
  :config
  (defun jens/diff-hl-refresh ()
    "Refresh diff-hl-mode."
    (interactive)
    (diff-hl-mode nil)
    (diff-hl-mode +1))

  (defhydra jens/diff-hl-hydra ()
    "Move to changed VC hunks."
    ("n" #'diff-hl-next-hunk "next")
    ("p" #'diff-hl-previous-hunk "previous"))

  (global-diff-hl-mode +1))

(use-package hl-todo
  :ensure t
  :commands global-hl-todo-mode
  :config
  (global-hl-todo-mode +1))

(use-package hl-fill-column
  :ensure t
  :custom-face
  (hl-fill-column-face ((t (:background "#4d0000")))))

(use-package helpful
  :straight t
  :demand t
  :commands (helpful-key
             helpful-callable
             helpful-variable
             helpful-symbol
             helpful-key
             helpful-mode)
  :bind
  (:map help-map
        ("M-a" . helpful-at-point))
  :config
  (defalias #'describe-key #'helpful-key)
  (defalias #'describe-function #'helpful-callable)
  (defalias #'describe-variable #'helpful-variable)
  (defalias #'describe-symbol #'helpful-symbol)
  (defalias #'describe-key #'helpful-key)
  (defalias #'describe-mode #'helpful-mode))

(use-package iedit
  :straight t
  :bind ("C-;" . iedit-mode))

(use-package multiple-cursors
  :ensure t
  :bind
  (("C-d" . mc/mark-next-like-this)
   ("C-S-d" . mc/mark-previous-like-this)
   ("C-M-a" . mc/mark-all-like-this)
   ("C-M-m" . mc-hydra/body))
  :init
  (setq mc/list-file (concat my-emacs-data-dir "mc-lists"))
  :config
  (defhydra mc-hydra ()
    "
^Next^                ^Previous^
^^^^^^^^----------------------------------------------------
_n_: Mark next        _p_: Mark previous
_N_: Skip to next     _P_: Skip to previous
_M-n_: Unmark next    _M-p_: Unmark previous
"
    ("n" #'mc/mark-next-like-this)
    ("N" #'mc/skip-to-next-like-this)
    ("M-n" #'mc/unmark-next-like-this)
    ("p" #'mc/mark-previous-like-this)
    ("P" #'mc/skip-to-previous-like-this)
    ("M-p" #'mc/unmark-previous-like-this)))

(use-package browse-kill-ring
  :ensure t
  :defer t
  :bind ("C-x C-y" . browse-kill-ring)
  :config (setq browse-kill-ring-quit-action 'save-and-restore))

(use-package highlight-defined
  :ensure t
  :hook (emacs-lisp-mode . highlight-defined-mode)
  :custom-face
  (highlight-defined-function-name-face ((t (:foreground "#BDE0F3"))))
  (highlight-defined-builtin-function-name-face ((t (:foreground "#BFBFBF")))))

(use-package avy
  :ensure t
  :defer t
  :bind
  (("C-ø" . avy-goto-char)
   ("C-'" . avy-goto-line))
  :config
  (setq avy-background 't)
  :custom-face
  (avy-background-face ((t (:background "#2B2B2B"))))
  (avy-lead-face ((t (:background "#2B2B2B"))))
  (avy-lead-face-0 ((t (:background "#2B2B2B"))))
  (avy-lead-face-1 ((t (:background "#2B2B2B"))))
  (avy-lead-face-2 ((t (:background "#2B2B2B")))))

(use-package avy-zap
  :ensure t
  :defer t
  :bind ("C-å" . avy-zap-to-char))

(use-package ace-mc
  :ensure t
  :bind ("C-M-d" . ace-mc-add-multiple-cursors))

(use-package expand-region
  :ensure t
  :defer t
  :bind
  (("M-e" . er/expand-region)
   ("C-M-e" . er/contract-region)))

(use-package change-inner
  :ensure t
  :defer t
  :bind
  (("M-i" . copy-inner)
   ("M-o" . copy-outer)
   ("M-I" . change-inner)
   ("M-O" . change-outer)))

(use-package move-text
  :ensure t
  :defer t
  :bind
  (("C-S-<up>" . move-text-up)
   ("C-S-<down>" . move-text-down)))

(use-package visual-regexp-steroids
  :ensure t
  :defer t
  :bind
  (("C-c r" . vr/replace)
   ("C-c q" . vr/query-replace)))

(use-package fullframe
  :ensure t
  :commands fullframe/maybe-restore-configuration
  :config
  (fullframe magit-status magit-mode-quit-window))

(use-package magit
  :ensure t
  :defer t
  :bind
  (("C-x m" . magit-status)
   :map magit-file-mode-map
   ("C-x g" . nil)
   :map magit-mode-map
   ("C-c C-a" . magit-commit-amend)
   ("<tab>" . magit-section-cycle))
  :config
  (setq magit-auto-revert-mode nil)
  (setq magit-log-arguments '("--graph" "--color" "--decorate" "-n256"))
  (setq magit-merge-arguments '("--no-ff"))
  (setq magit-section-visibility-indicator '("…", t)))

(use-package magithub
  :ensure t
  :after magit
  :config
  (magithub-feature-autoinject t))

(use-package magit-todos
  :ensure t
  :commands magit-todos-mode
  :config
  (magit-todos-mode))

(use-package undo-tree
  :ensure t
  :defer t
  :diminish undo-tree-mode
  :commands global-undo-tree-mode
  :bind
  (("C-x u" . undo-tree-visualize)
   ("C-_" . undo-tree-undo)
   ("M-_" . undo-tree-redo))
  :config
  (setq undo-tree-visualizer-diff t)
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist `(("." . ,my-emacs-temp-dir)))
  (global-undo-tree-mode))

(use-package goto-chg
  :ensure t
  :defer t
  :bind ("M-ø" . goto-last-change))

(use-package beginend
  :ensure t
  :defer t
  :diminish beginend-global-mode
  :commands beginend-global-mode
  :bind (("M-<" . beginning-of-buffer)
         ("M->" . end-of-buffer))
  :config
  ;; diminish all the beginend modes
  (mapc (lambda (s) (diminish (cdr s))) beginend-modes)
  (beginend-global-mode))

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :commands (which-key-mode
             which-key-setup-side-window-right)
  :config
  (which-key-setup-side-window-right)
  (setq which-key-max-description-length 40)
  (which-key-mode))

(use-package wgrep
  :ensure t
  :defer t
  :after grep
  :bind
  (("C-S-g" . rgrep)
   :map grep-mode-map
   ("C-x C-q" . wgrep-change-to-wgrep-mode)
   ("C-x C-k" . wgrep-abort-changes)
   ("C-c C-c" . wgrep-finish-edit))
  :config
  (setq wgrep-auto-save-buffer t))

(use-package auctex
  :ensure t
  :defer t
  :hook (LaTeX-mode-hook . reftex-mode)
  :defines (TeX-view-program-selection
            TeX-view-program-list)
  :functions (TeX-PDF-mode TeX-source-correlate-mode)
  :config
  (setq-default TeX-PDF-mode t) ;; default to pdf
  (setq-default TeX-global-PDF-mode t) ;; default to pdf
  (setq-default TeX-parse-self t) ;; parse on load
  (setq-default TeX-auto-save t) ;; parse on save
  (setq-default TeX-save-query nil) ;; save before compiling
  (setq-default TeX-master nil) ;; try to figure out which file is the master
  (setq-default reftex-plug-into-AUCTeX t) ;; make reftex and auctex work together
  (setq-default doc-view-resolution 300)

  ;; (setq TeX-view-program-selection (quote ((output-pdf "zathura") (output-dvi "xdvi"))))
  (TeX-source-correlate-mode)        ; activate forward/reverse search
  (TeX-PDF-mode)
  ;; (add-to-list 'TeX-view-program-list
  ;;              '("Zathura" "zathura " (mode-io-correlate "--synctex-forward %n:1:%b") " %o"))
  ;; (add-to-list 'TeX-view-program-selection
  ;;              '(output-pdf "Zathura"))

  (add-to-list
   'TeX-view-program-list
   '("Zathura"
     ("zathura "
      (mode-io-correlate " --synctex-forward %n:0:%b -x \"emacsclient +%{line} %{input}\" ")
      " %o")
     "zathura"))

  (add-to-list
   'TeX-view-program-selection
   '(output-pdf "Zathura")))

(use-package slime
  :defer t
  :ensure t
  :functions qlot-slime
  :commands slime-start
  :config
  (setq inferior-lisp-program "sbcl")
  (setq slime-contribs '(slime-fancy))

  (defun qlot-slime (directory)
    "Start Common Lisp REPL using project-local libraries via
`roswell' and `qlot'."
    (interactive (list (read-directory-name "Project directory: ")))
    (slime-start :program "~/.roswell/bin/qlot"
                 :program-args '("exec" "ros" "-S" "." "run")
                 :directory directory
                 :name 'qlot
                 :env (list (concat "PATH="
                                    (mapconcat 'identity exec-path ":"))
                            (concat "QUICKLISP_HOME="
                                    (file-name-as-directory directory) "quicklisp/")))))

(use-package elfeed
  :ensure t
  :defer t
  :after today
  :commands (elfeed elfeed-search-selected)
  :functions jens/elfeed-copy-link-at-point
  :bind (:map elfeed-search-mode-map
              ("t" . today-capture-elfeed-at-point)
              ("c" . jens/elfeed-copy-link-at-point))
  :config
  (setq-default elfeed-search-filter "@1-month-ago +unread ")

  (defface youtube-elfeed-face '((t :foreground "#E0CF9F"))
    "face for youtube.com entries"
    :group 'elfeed-faces)
  (push '(youtube youtube-elfeed-face) elfeed-search-face-alist)
  (defface reddit-elfeed-face '((t :foreground "#9FC59F"))
    "face for reddit.com entries"
    :group 'elfeed-faces)
  (push '(reddit reddit-elfeed-face) elfeed-search-face-alist)
  (defface blog-elfeed-face '((t :foreground "#DCA3A3"))
    "face for blog entries"
    :group 'elfeed-faces)
  (push '(blog blog-elfeed-face) elfeed-search-face-alist)
  (defface emacs-elfeed-face '((t :foreground "#94BFF3"))
    "face for emacs entries"
    :group 'elfeed-faces)
  (push '(emacs emacs-elfeed-face) elfeed-search-face-alist)
  (defface aggregate-elfeed-face '((t :foreground "#948FF3"))
    "face for aggregate entries"
    :group 'elfeed-faces)
  (push '(aggregate aggregate-elfeed-face) elfeed-search-face-alist)

  (setq elfeed-feeds (jens/load-from-file (concat my-emacs-dir "elfeeds.el")))

  (defun jens/elfeed-copy-link-at-point ()
    "Copy the link of the elfeed entry at point to the clipboard."
    (interactive)
    (letrec ((entry (car (elfeed-search-selected)))
             (link (elfeed-entry-link entry)))
      (with-temp-buffer
        (insert link)
        (clipboard-kill-ring-save (point-min) (point-max))
        (message (format "copied %s to clipboard" link))))))

(use-package auth-source-pass
  :commands (auth-source-pass-enable auth-source-pass-get)
  :config
  (auth-source-pass-enable)
  ;; using authinfo.gpg
  ;; (letrec ((auth (auth-source-search :host "freenode"))
  ;;          (user (plist-get (car auth) :user))
  ;;          (secret (plist-get (car auth) :secret)))
  ;;   (message (format "user: %s, secret: %s" user (funcall secret))))

  ;; using password-store
  ;; (auth-source-pass-get 'secret "irc/freenode/nickserv")
  )

(use-package erc-hl-nicks
  :ensure t
  :defer t
  :commands erc-hl-nicks-enable)

(use-package erc
  :defer t
  :after auth-source-pass
  :functions ercgo
  :commands erc-tls
  :config
  (setq erc-rename-buffers t
        erc-interpret-mirc-color t
        erc-prompt ">"
        erc-insert-timestamp-function 'erc-insert-timestamp-left
        erc-lurker-hide-list '("JOIN" "PART" "QUIT"))

  (setq erc-user-full-name "Jens Christian Jensen")
  (erc-hl-nicks-enable)

  (setq erc-autojoin-channels-alist '(("freenode.net" "#emacs" "##java")))

  (defun ercgo ()
    (interactive)
    (erc-tls :server "irc.freenode.net"
             :port 6697
             :nick "jensecj"
             :password (auth-source-pass-get 'secret "irc/freenode/jensecj"))))

(use-package fontify-face
  :ensure t
  :defer t
  :hook (emacs-lisp-mode . fontify-face-mode))

(use-package unicode-fonts
  :disabled
  :ensure t
  :defer t
  :config (unicode-fonts-setup))

(use-package exec-path-from-shell
  :defer 3
  :ensure t
  :demand t
  :commands (exec-path-from-shell-copy-env
             exec-path-from-shell-initialize)
  :config
  (exec-path-from-shell-initialize)
  ;; try to grab the ssh-agent if it is running
  (exec-path-from-shell-copy-env "SSH_AGENT_PID")
  (exec-path-from-shell-copy-env "SSH_AUTH_SOCK"))

(use-package multi-term
  :ensure t
  :demand t
  :functions (jens/multi-term
              jens/multi-term-save-term
              jens/multi-term-unsave-term
              jens/multi-term-restore-terms
              jens/multi-term-list-saves)
  :defines (multi-term-save-file
            multi-term-saved-terms)
  :commands (multi-term-get-buffer
             multi-term-internal)
  :bind ("C-z" . jens/multi-term)
  :config
  (setq multi-term-program "/bin/zsh")
  ;; (setq term-bind-key-alist '()) ;; clear the binds list, defaulting to emacs binds
  (setq term-buffer-maximum-size 10000)

  (defun jens/term-paste (&optional string)
    "Paste a string to the process of the current buffer, fixes paste for
    multi-term mode."
    (interactive)
    (process-send-string
     (get-buffer-process (current-buffer))
     (if string string (current-kill 0))))
  (define-key term-raw-map (kbd "C-y") 'jens/term-paste)
  ;; (add-to-list 'term-bind-key-alist '("<C-left>" . term-send-backward-word))
  ;; (add-to-list 'term-bind-key-alist '("<C-right>" . term-send-forward-word))
  ;; (add-to-list 'term-bind-key-alist '("<C-backspace>" . (lambda () (interactive) (term-send-raw-string "\C-h")))) ;; backwards-kill-word
  ;; (add-to-list 'term-bind-key-alist '("<C-del>" . (lambda () (interactive) (term-send-raw-string "\e[3;5~")))) ;; forwards-kill-word


  ;; Sets up the ability to store a multi-term using =jens/multi-term-save-term=, all
  ;; terminals saved this was will be reopened when starting a new session.

  ;; It does not restart programs, just starts the terminals in the folders they were
  ;; in when saved.
  (defvar multi-term-saved-terms '()
    "List of saved terminals")
  (defvar multi-term-save-file (concat my-emacs-data-dir "multi-terms")
    "File on disk used to store the list of saved terminals")

  (defun jens/multi-term (&optional open-term-in-background)
    "Create new term buffer."
    (interactive)
    (let ((term-buffer)
          (buffer-new-name (concat "*" default-directory "*")))
      ;; Set buffer.
      (setq term-buffer (multi-term-get-buffer current-prefix-arg))
      (setq multi-term-buffer-list (nconc multi-term-buffer-list (list term-buffer)))
      (set-buffer term-buffer)
      ;; Internal handle for `multi-term' buffer.
      (multi-term-internal)
      ;; Switch buffer
      (if (not open-term-in-background)
          (switch-to-buffer term-buffer))
      (rename-buffer buffer-new-name)))

  (defun jens/multi-term-save-term ()
    "Pick an open terminal and save it"
    (interactive)
    (if (null multi-term-buffer-list)
        (error "Error: No open terminals"))
    (let ((buf (get-buffer (completing-read "Select term:" (mapcar 'buffer-name multi-term-buffer-list)))))
      (with-current-buffer buf
        (if (member default-directory multi-term-saved-terms)
            (error "That term is already saved"))
        (add-to-list 'multi-term-saved-terms default-directory)))
    (jens/save-to-file multi-term-saved-terms multi-term-save-file))

  (defun jens/multi-term-unsave-term ()
    "Pick a saved terminal to remove from the saved list"
    (interactive)
    (let ((trm (completing-read "Select term:" multi-term-saved-terms)))
      (setq multi-term-saved-terms (delete trm multi-term-saved-terms)))
    (jens/save-to-file multi-term-saved-terms multi-term-save-file))

  (defun jens/multi-term-restore-terms ()
    "Restores all terminals from the saved list"
    (interactive)
    (setq multi-term-saved-terms (jens/load-from-file multi-term-save-file))
    (ignore-errors
      (dolist (trm multi-term-saved-terms)
        (if (f-exists? trm)
            (let ((default-directory trm))
              (jens/multi-term t))
          (message (format "multi-term-restore error, path does not exist: %s" trm))))))

  (defun jens/multi-term-list-saves ()
    "List all saved terminals"
    (interactive)
    (completing-read "All saved terms:" (jens/load-from-file multi-term-save-file)))

  ;; restore all saved terminals at startup
  (jens/multi-term-restore-terms))

(use-package ivy
  :ensure t
  :demand t
  :diminish ivy-mode
  :commands (ivy-read
             ivy-mode
             ivy-pop-view-action
             ivy-default-view-name
             ivy--get-window)
  :bind
  (("M-p p" . ivy-push-view)
   ("M-p k" . ivy-pop-view)
   ("M-p b" . ivy-switch-view)
   ("C-x C-b" . ivy-switch-buffer)
   :map ivy-minibuffer-map
   ("C-d" . (lambda () (interactive) (ivy-quit-and-run (dired ivy--directory))))
   ("C-S-<return>" . ivy-immediate-done))
  :config
  (setq ivy-height 15)
  (setq ivy-count-format "")
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)

  (setq ivy-re-builders-alist '((t . ivy--regex-plus)))

  ;; Adds functionality to persist ivy-views across sessions.
  ;; You could simple add =ivy-views= to =savehist-additional-variables=, but I
  ;; decided to do it this way, so it saves the view straight to disk when added, and
  ;; I was modifying =ivy-push-view= anyway, to handle overwriting a view, and
  ;; changing the default name for views.
  (defvar ivy-save-file (concat my-emacs-data-dir "ivy-views")
    "The file on disk used to save ivy-views")

  (defun jens/ivy-save-views ()
    "Save ivy-views to disk"
    (interactive)
    (jens/save-to-file ivy-views ivy-save-file))

  (defun jens/ivy-load-views ()
    "Load ivy-views from disk"
    (interactive)
    (setq ivy-views (jens/load-from-file ivy-save-file)))

  ;; use an empty string as the default view name, instead of buffers
  (defun jens/ivy-empty-default-view-name ()
    "Default name for a new view, used in push-view prompt."
    '"{} ")

  (defun jens/ivy-views-find (view)
    "Find a view from its name"
    (dolist (v ivy-views)
      (if (string= view (car v))
          (return v))))

  (defun jens/ivy-push-view ()
    "Push the current window tree on `ivy-views'.
Currently, the split configuration (i.e. horizonal or vertical)
and point positions are saved, but the split positions aren't.
Use `ivy-pop-view' to delete any item from `ivy-views'."
    (interactive)
    (let* ((view (cl-labels
                     ((ft (tr)
                          (if (consp tr)
                              (if (eq (car tr) t)
                                  (cons 'vert
                                        (mapcar #'ft (cddr tr)))
                                (cons 'horz
                                      (mapcar #'ft (cddr tr))))
                            (with-current-buffer (window-buffer tr)
                              (cond ((buffer-file-name)
                                     (list 'file (buffer-file-name) (point)))
                                    ((eq major-mode 'dired-mode)
                                     (list 'file default-directory (point)))
                                    (t
                                     (list 'buffer (buffer-name) (point))))))))
                   (ft (car (window-tree)))))
           (view-name (ivy-read "Name view: " ivy-views
                                :initial-input (ivy-default-view-name))))
      (when view-name
        ;; pop the view if it already exists, so we replace it
        (ivy-pop-view-action (jens/ivy-views-find view-name))
        (push (list view-name view) ivy-views))))

  (advice-add 'ivy-push-view :override #'jens/ivy-push-view)

  ;; replace the default view-name
  (advice-add 'ivy-default-view-name :override #'jens/ivy-empty-default-view-name)
  ;; (advice-remove 'ivy-default-view-name #'jens/ivy-empty-default-view-name)

  ;; save ivy-views when pushing/popping views
  (advice-add 'ivy-push-view :after #'jens/ivy-save-views)
  (advice-add 'ivy-pop-view :after #'jens/ivy-save-views)

  (ivy-mode)
  (jens/ivy-load-views)
  :custom-face
  (ivy-current-match ((t (:foreground nil :background "#4f4f4f" :box t)))))

(use-package ivy-rich
  :ensure t
  :commands ivy-rich-mode
  :config
  (defun ivy-rich-bookmark-context-string (candidate)
    (let ((front (cdr (assoc 'front-context-string (cdr (assoc candidate bookmark-alist))))))
      (s-replace "\n" ""  (concat  "" front))))

  (add-to-list 'ivy-rich--display-transformers-list
               '(:columns
                 ((ivy-rich-candidate (:width 30 :face font-lock-builtin-face))
                  (ivy-rich-bookmark-context-string (:width 20 :face font-lock-string-face))
                  (ivy-rich-bookmark-filename (:face font-lock-doc-face)))))
  (add-to-list 'ivy-rich--display-transformers-list 'counsel-bookmark)

  (ivy-rich-mode -1)
  (ivy-rich-mode 1))

(use-package counsel
  :ensure t
  :after (ivy fzf)
  :defer 1
  :diminish counsel-mode
  :functions jens/counsel-read-file-name
  :commands (counsel-mode counsel--find-file-matcher)
  :bind
  (("C-S-s" . counsel-rg)
   ("C-x f" . counsel-recentf)
   ("C-x C-f" . counsel-find-file)
   ("C-x C-S-f" . counsel-fzf)
   ("C-x C-i" . counsel-imenu)
   ("M-æ" . counsel-mark-ring)
   ("M-x" . counsel-M-x)
   ("M-b" . counsel-bookmark)
   ("<f1> l" . counsel-find-library))
  :config
  (setq
   counsel-grep-base-command
   "rg -i -M 120 --no-heading --line-number --color never '%s' %s")

  (defun jens/counsel-read-file-name (prompt &optional initial-input)
    "Query for a file path using counsel and ivy"
    (interactive)
    (ivy-read prompt 'read-file-name-internal
              :matcher #'counsel--find-file-matcher
              :initial-input initial-input
              :action
              (lambda (x)
                (with-ivy-window
                  (if (and counsel-find-file-speedup-remote
                           (file-remote-p ivy--directory))
                      (let ((find-file-hook nil))
                        (expand-file-name x ivy--directory))
                    (expand-file-name x ivy--directory))))
              :preselect (when counsel-find-file-at-point
                           (require 'ffap)
                           (let ((f (ffap-guesser)))
                             (when f (expand-file-name f))))
              :require-match 'confirm-after-completion
              :history 'file-name-history
              :keymap counsel-find-file-map
              :caller 'counsel-read-find-name))

  (counsel-mode))

(use-package swiper
  :bind ("C-s" . jens/swiper)
  :config
  (defun jens/swiper ()
    "If region is active, use the contents of the region as the
initial search query."
    (interactive)
    (if (region-active-p)
        (let ((query (buffer-substring-no-properties (region-beginning) (region-end))))
          (deactivate-mark)
          (swiper query))
      (call-interactively #'swiper))))

(use-package bookmark+
  :straight (bookmark+ :type git :host github :repo "emacsmirror/bookmark-plus"))

(use-package projectile
  :ensure t
  :defer t
  :diminish projectile-mode
  :config
  (setq projectile-known-projects-file (concat my-emacs-data-dir "projectile-bookmarks"))
  (setq projectile-cache-file (concat my-emacs-data-dir "projectile.cache")))

(use-package counsel-projectile
  :ensure t
  :defer t
  :after (counsel projectile)
  :commands counsel-projectile-mode
  :config (counsel-projectile-mode))

(use-package keyfreq
  :commands (keyfreq-mode keyfreq-autosave-mode)
  :config
  (keyfreq-mode +1)
  (keyfreq-autosave-mode +1))

(use-package flyspell
  :ensure t
  :defer t
  :functions enable-spellchecking
  :commands (flyspell-mode
             flyspell-prog-mode
             flyspell-buffer)
  :config
  (ispell-change-dictionary "english")

  (defun enable-spellchecking ()
    (interactive)
    (ispell-change-dictionary "english")
    (flyspell-prog-mode)
    (flyspell-buffer)))

(use-package so-long
  :straight (so-long :type git :repo "https://git.savannah.gnu.org/git/so-long.git/")
  :demand t
  :commands so-long-enable
  :config
  (so-long-enable))

(use-package zenburn-theme
  :ensure t
  :demand t
  :config
  (load-theme 'zenburn t)
  :custom-face
  (popup-tip-face ((t (:background "#cbcbbb" :foreground "#2b2b2b")))))

;;;;;;;;;;;;;;;;;;;;;;
;; homemade things ;;
;;;;;;;;;;;;;;;;;;;;;;

(use-package sane-windows
  :straight (sane-windows :type git :repo "git@github.com:jensecj/sane-windows.el.git")
  :demand t
  :bind (("C-x 0" . nil)
         ("C-x 1" . nil)
         ("C-x o" . delete-other-windows)
         ("C-x p" . delete-window)
         ("M-C-<tab>" . sw/toggle-window-split)
         ("M-S-<iso-lefttab>" . sw/rotate-windows)
         ("M-S-<left>" . sw/move-border-left)
         ("M-S-<right>" . sw/move-border-right)
         ("M-S-<up>" . sw/move-border-up)
         ("M-S-<down>" . sw/move-border-down)))

(use-package fullscreen
  :bind ("M-f" . fullscreen-toggle))

(use-package etmux
  :straight (etmux :repo "git@github.com:jensecj/etmux.el.git")
  :demand t
  :commands (etmux-send-command))

(use-package highlight-bookmarks
  :demand t
  :commands highlight-bookmarks-in-this-buffer
  :config
  (add-hook 'find-file-hook #'highlight-bookmarks-in-this-buffer)
  (add-hook 'after-save-hook #'highlight-bookmarks-in-this-buffer)
  (advice-add #'bookmark-set :after #'highlight-bookmarks-in-this-buffer)
  (advice-add #'bookmark-delete :after #'highlight-bookmarks-in-this-buffer))

(use-package today
  :load-path "lisp/today/"
  :defer t
  :commands (today
             today-list
             today-move-to-date
             today-move-to-tomorrow
             today-capture--with-task
             today-capture-elfeed-at-point
             today-hydra/body)
  :bind
  (("C-x t" . today-hydra/body))
  :config
  (setq today-directory "~/vault/org/planner/"))

(use-package doc-at-point
  :load-path "lisp/doc-at-point/"
  :defer t
  :bind
  (("C-+" . doc-at-point))
  :commands (doc-at-point doc-at-point-setup-defaults)
  :config
  (require 'help-fns+)
  (doc-at-point-setup-defaults))

(use-package lowkey-mode-line
  :straight
  (lowkey-mode-line
   :repo "git@github.com:jensecj/lowkey-mode-line.git")
  :demand t
  :commands lowkey-mode-line-enable
  :config
  (lowkey-mode-line-enable)
  :custom-face
  (lml-buffer-face ((t (:background "grey20"))))
  (lml-buffer-face-inactive ((t (:background "grey20"))))
  (lml-position-face ((t (:background "grey25"))))
  (lml-position-face-inactive ((t (:background "grey20"))))
  (lml-major-mode-face ((t (:background "grey30"))))
  (lml-major-mode-face-inactive ((t (:background "grey20"))))
  (lml-minor-modes-face ((t (:background "grey30"))))
  (lml-minor-modes-face-inactive ((t (:background "grey20"))))
  (lml-filler-face ((t (:background "grey30"))))
  (lml-filler-face-inactive ((t (:background "grey20"))))
  (lml-vc-face ((t (:background "grey20"))))
  (lml-vc-face-inactive ((t (:background "grey20")))))

;;;;;;;;;;;;;;;;;;;;;;;
;; advice and hooks ;;
;;;;;;;;;;;;;;;;;;;;;;;

;; When popping the mark, continue popping until the cursor actually
;; moves. also, if the last command was a copy - skip past all the
;; expand-region cruft.
(defun jens/pop-to-mark-command (orig-fun &rest args)
  "Call ORIG-FUN until the cursor moves. Try popping up to 10
  times."
  (let ((p (point)))
    (dotimes (_ 10)
      (when (= p (point))
        (apply orig-fun args)))))
(advice-add 'pop-to-mark-command :around #'jens/pop-to-mark-command)

;; allows us to type 'C-u C-SPC C-SPC...' instead of having to re-type 'C-u'
;; every time.
(setq set-mark-command-repeat-pop t)

;; Create nonexistent directories when saving a file
(add-hook 'before-save-hook
          (lambda ()
            (when buffer-file-name
              (let ((dir (file-name-directory buffer-file-name)))
                (when (not (file-exists-p dir))
                  (make-directory dir t))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; keybindings for built-in things ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; handle special keys
(define-key key-translation-map [S-dead-circumflex] "^")
(define-key key-translation-map [dead-tilde] "~")
(define-key key-translation-map [S-dead-grave] "´")
(define-key key-translation-map [dead-acute] "`")
(define-key key-translation-map [dead-diaeresis] "¨")

;; Insert tilde with a single keystroke
(global-set-key (kbd "<menu>") (lambda () (interactive) (insert "~")))

;; Easily mark the entire buffer
(global-set-key (kbd "C-x a") 'mark-whole-buffer)

;; Quit emacs, mnemonic is C-x REALLY QUIT
(global-set-key (kbd "C-x r q") 'save-buffers-kill-terminal)
;; Kill emacs, mnemonic is C-x REALLY KILL
(global-set-key (kbd "C-x r k") 'save-buffers-kill-emacs)

;; don't close emacs
(global-set-key (kbd "C-x C-c") nil)

;; Rebind help to F1
(define-key key-translation-map [?\C-h] [?\C-?])
(global-set-key (kbd "<f1>") 'help-command)
(define-key help-map (kbd "M-f") #'find-function)
(define-key help-map (kbd "M-v") #'find-variable)
(define-key help-map (kbd "b") #'describe-bindings)

;; Evaluate the current buffer/region
(global-set-key (kbd "C-c C-k") 'eval-buffer)
(global-set-key (kbd "C-c k") 'eval-region)

;; Casing words/regions
(global-set-key (kbd "M-u") #'upcase-dwim)
(global-set-key (kbd "M-l") #'downcase-dwim)
(global-set-key (kbd "M-c") #'capitalize-dwim)

;; Scroll the buffer without moving the point (unless we over-move)
(global-set-key
 (kbd "C-<up>")
 (lambda ()
   (interactive)
   (scroll-down 5)))

(global-set-key
 (kbd "C-<down>")
 (lambda ()
   (interactive)
   (scroll-up 5)))

;; dont use the mouse
(global-set-key (kbd "<down-mouse-1>") nil)
(global-set-key (kbd "<down-mouse-2>") nil)
(global-set-key (kbd "<down-mouse-3>") nil)
(global-set-key (kbd "C-<down-mouse-1>") nil)
(global-set-key (kbd "C-<down-mouse-2>") nil)
(global-set-key (kbd "C-<down-mouse-3>") nil)
(global-set-key (kbd "S-<down-mouse-1>") nil)
(global-set-key (kbd "S-<down-mouse-2>") nil)
(global-set-key (kbd "S-<down-mouse-3>") nil)
(global-set-key (kbd "M-<down-mouse-1>") nil)
(global-set-key (kbd "M-<down-mouse-2>") nil)
(global-set-key (kbd "M-<down-mouse-3>") nil)

;; Disable suspend-frame
(global-set-key (kbd "C-x C-z") nil)

;; Make Home and End to to the top and bottom of the buffer, we have C-a/e
(global-set-key (kbd "<home>") 'beginning-of-buffer)
(global-set-key (kbd "<end>") 'end-of-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; keybindings for defuns ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Better C-a
(global-set-key (kbd "C-a") 'jens/smart-beginning-of-line)

;; Join lines (pull the below line up to this one)
(global-set-key (kbd "M-j") 'jens/join-region-or-line)

;; Comment/uncomment block
(global-set-key (kbd "C-c c") 'jens/comment-uncomment-region-or-line)

;; Fix spaces / tabs
(global-set-key (kbd "C-c n") 'jens/cleanup-buffer)

;; Enable backwards killing of lines
(global-set-key (kbd "C-S-k") 'jens/kill-to-beginning-of-line)

(global-set-key (kbd "C-x b") 'ibuffer)

;; Move windows with S-<arrow>
(windmove-default-keybindings 'shift)

;; Force save a file, mnemonic is C-x TOUCH
(global-set-key (kbd "C-x C-t") 'jens/touch-buffer-file)

;; Copy current line / region
(global-set-key (kbd "M-w") 'jens/save-region-or-current-line)
(global-set-key (kbd "C-w") 'jens/kill-region-or-current-line)

;; Completion that uses many different methods to find options.
;; (global-set-key (kbd "C-.") 'hippie-expand-no-case-fold)
;; (global-set-key (kbd "C-:") 'hippie-expand-lines)
;; (global-set-key (kbd "C-,") 'completion-at-point)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; experimantal things ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(load (concat my-emacs-dir "experimental.el"))

(msg-success (format "Emacs initialized in %s, with %s garbage collections." (emacs-init-time) gcs-done))

(provide 'init)
;;; init.el ends here
