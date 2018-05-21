;;; -*- lexical-binding: t -*-
;; use lexical binding for initialization code
(setq-default lexical-binding t)

;; setup some things that other packages may depend on
(setq user-full-name "Jens Christian Jensen")

(defun text-red (txt) (format "\e[1m\e[31m%s\e[0m" txt))
(defun text-green (txt) (format "\e[1m\e[32m%s\e[0m" txt))
(defun text-yellow (txt) (format "\e[1m\e[33m%s\e[0m" txt))
(defun text-blue (txt) (format "\e[1m\e[34m%s\e[0m" txt))
(defun text-magenta (txt) (format "\e[1m\e[35m%s\e[0m" txt))
(defun text-cyan (txt) (format "\e[1m\e[36m%s\e[0m" txt))

(defun msg-info (txt) (message (text-yellow txt)))
(defun msg-warning (txt) (message (text-red txt)))
(defun msg-success (txt) (message (text-green txt)))

(msg-info "Started initializing emacs!")

;; Turn off excess interface early in startup to avoid momentary display
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; directories for things related to emacs
(defconst my-emacs-dir user-emacs-directory)
(defconst my-emacs-lisp-dir (concat my-emacs-dir "lisp/"))
(defconst my-emacs-modes-dir (concat my-emacs-dir "modes/"))

(defconst my-emacs-temp-dir (concat my-emacs-dir ".temp/"))
;; config and cache files
(defconst my-emacs-data-dir (concat my-emacs-dir "data/"))
;; backups and auto saves
(defconst my-emacs-backup-dir (concat my-emacs-data-dir "backups/"))

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

;; setup package archives
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")))

(defconst melpa-archive (concat my-emacs-dir "elpa/archives/melpa"))

;; install use-package if we don't already have it
(unless (package-installed-p 'use-package)
  (msg-warning "use-package is not installed. installing...")
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; make use-package tell us what its doing
(setq use-package-verbose t
      use-package-enable-imenu-support t)

;; some libraries that are frequently used
(use-package dash ;; functional things, -map, -fold, etc
  :ensure t
  :commands (-some -remove))
(use-package s ;; string manipulations
  :ensure t
  :commands (s-trim s-prefix?))
(use-package f ;; handling the file-system
  :ensure t
  :commands (f-exists? f-glob))

;; Use =Source Code Pro= font if it is available. When launching emacs as a
;; daemon, fonts are not loaded until we actually produce a frame, so the
;; font list will be empty, focus-in-hook is run when a frame is created,
;; whether by a user or a daemon, the first frame created will not have
;; the setup, as it is created before this is run, still looking into
;; this.
(defun jens/init-font-setup ()
  "Setup fonts, then remove self from `focus-in-hook' so we only run once."
  (when (find-font (font-spec :name "Source Code Pro Semibold 10"))
    (add-to-list 'default-frame-alist '(font . "Source Code Pro Semibold 10"))
    (set-frame-font "Source Code Pro Semibold 10"))
  (remove-hook 'server-after-make-frame-hook 'jens/init-font-setup))

(add-hook 'server-after-make-frame-hook 'jens/init-font-setup)

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
(setq require-final-newline 't)

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

(setq initial-scratch-message "")

;; used to be transpose-words, used for new keybindings
(global-unset-key (kbd "M-t"))

;; show trailing whitespace by default
(setq-default show-trailing-whitespace nil)
(defun jens/show-trailing-whitespace ()
  (interactive)
  (setq show-trailing-whitespace t))
(add-hook 'text-mode-hook 'jens/show-trailing-whitespace)
(add-hook 'prog-mode-hook 'jens/show-trailing-whitespace)

;; gpg and auth
(use-package epa-file
  :demand t
  :commands epa-file-enable
  :config
  (epa-file-enable)
  (setq epa-pinentry-mode 'loopback))

;; enable gpg pinentry through the minibuffer
(use-package pinentry
  :ensure t
  :demand t
  :commands pinentry-start
  :config
  (pinentry-start)
  (setenv "GPG_AGENT_INFO" nil))

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

(defun jens/create-scratch-buffer nil
  "create a new scratch buffer to work in. (could be *scratch* - *scratchX*)"
  (interactive)
  (let ((n 0)
        bufname)
    (while (progn
             (setq bufname (concat "*scratch"
                                   (if (= n 0) "" (int-to-string n))
                                   "*"))
             (setq n (1+ n))
             (get-buffer bufname)))
    (switch-to-buffer (get-buffer-create bufname))
    (funcall initial-major-mode)))

(defun jens/clean-view ()
  "Creates a scratch buffer, and makes it the only buffer visible."
  (interactive)
  (jens/create-scratch-buffer)
  (delete-other-windows))

(defun jens/cleanup-buffer ()
  "Perform a bunch of operations on the white space content of a buffer.
   Including indent-buffer, which should not be called automatically on save."
  (interactive)
  (indent-region (point-min) (point-max))
  (whitespace-cleanup)
  (message "cleaned up"))

(defun jens/open-line-below ()
  "Inserts a line below the current line, indents it, and moves the the
  beginning of that line."
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))

(defun jens/open-line-above ()
  "Inserts a line above the current line, indents it, and moves the the
  beginning of that line."
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))

(defun jens/smart-line-beginning ()
  "Move point to the beginning of line or beginning of text"
  (interactive)
  (let ((pt (point)))
    (beginning-of-line-text)
    (when (eq pt (point))
      (beginning-of-line))))

(defun jens/kill-to-beginning-of-line ()
  "Kills from <point> to the beginning of the current line."
  (interactive)
  (kill-region (save-excursion (beginning-of-line) (point))
               (point)))

(defun jens/save-region-or-current-line (arg)
  "If a region is active then it is saved to the kill-ring, otherwise the current
line is saved."
  (interactive "P")
  (if (region-active-p)
      (kill-ring-save (region-beginning) (region-end))
    (kill-ring-save (line-beginning-position) (+ 1 (line-end-position)))))

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
  "If region is active, join all lines in region to a single line. Otherwise join
the line below the current line, with the current line, placing it after."
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

(defun jens/byte-recompile-this-file ()
  (interactive)
  (byte-recompile-file (buffer-file-name)))

(defun jens/get-buffer-file-name+ext ()
  "Get the file name and extension of the file belonging to the current buffer."
  (file-name-nondirectory buffer-file-name))

(defun jens/get-buffer-file-name ()
  "Get the file name of the file belonging to the current buffer."
  (file-name-sans-extension (jens/get-buffer-file-name+ext)))

(defun jens/get-buffer-file-directory ()
  "Get the directory of the file belonging to the current buffer"
  (file-name-directory (buffer-file-name)))

(defun jens/file-age (file)
  "Returns the number of seconds since the file was last modified."
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
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer)))
    (if (not (and filename (file-exists-p filename)))
        (message "no such file exists")
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

(defun jens/touch-buffer-file ()
  "Touches the current buffer, marking it as dirty."
  (interactive)
  (insert " ")
  (backward-delete-char 1)
  (save-buffer))

;;;;;;;;;;;;;;;;;
;; lisp defuns ;;
;;;;;;;;;;;;;;;;;

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
  "Tries to require FEATURE, if an exception is thrown, log it."
  (condition-case ex
      (progn
        (msg-info (format "@ Loading \"%s\" " (symbol-name feature)))
        (require feature))
    ('error (msg-warning (format "@ Error loading \"%s\": %s" (symbol-name feature) ex)))))

(defun jens/eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defmacro jens/with-supressed-message (&rest body)
  "Saves the current message in the minibuffer, executes body, then
restores the message."
  (let ((saved-message-symbol (make-symbol "saved-message")))
    `(let ((,saved-message-symbol (current-message)))
       (progn ,@body)
       (message ,saved-message-symbol))))

(defun jens/save-to-file (data filename)
  "Save lisp object to a file"
  (with-temp-file filename
    (prin1 data (current-buffer))))

(defun jens/load-from-file (filename)
  "Load lisp object from file"
  (with-temp-buffer
    (insert-file-contents filename)
    (cl-assert (eq (point) (point-min)))
    (read (current-buffer))))

;;;;;;;;;;;;;;;;;;;
;; window defuns ;;
;;;;;;;;;;;;;;;;;;;

(defun jens/toggle-window-split ()
  "Toggle window splitting between horizontal and vertical"
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))
    (message "You can only toggle split of two windows!")))

(defun jens/rotate-windows ()
  "Rotate your windows"
  (interactive)
  (cond ((not (> (count-windows)1))
         (message "You can't rotate a single window!"))
        (t
         (let ((i 1)
               (numWindows (count-windows)))
           (while  (< i numWindows)
             (let* ((w1 (elt (window-list) i))
                    (w2 (elt (window-list) (+ (% i numWindows) 1)))

                    (b1 (window-buffer w1))
                    (b2 (window-buffer w2))

                    (s1 (window-start w1))
                    (s2 (window-start w2)))
               (set-window-buffer w1  b2)
               (set-window-buffer w2 b1)
               (set-window-start w1 s2)
               (set-window-start w2 s1)
               (setq i (1+ i))))))))

;; intuitive window resizing
(defun xor (b1 b2)
  (or (and b1 b2)
      (and (not b1) (not b2))))

(defun jens/move-border-left-or-right (arg dir)
  "General function covering jens/move-border-left and jens/move-border-right.
   If DIR is t, then move left, otherwise move right."
  (interactive)
  (if (null arg) (setq arg 3))
  (let ((left-edge (nth 0 (window-edges))))
    (if (xor (= left-edge 0) dir)
        (shrink-window arg t)
      (enlarge-window arg t))))

(defun jens/move-border-up-or-down (arg dir)
  "General function covering jens/move-border-up and jens/move-border-down.
   If DIR is t, then move up, otherwise move down."
  (interactive)
  (if (null arg) (setq arg 3))
  (let ((top-edge (nth 1 (window-edges))))
    (if (xor (= top-edge 0) dir)
        (shrink-window arg nil)
      (enlarge-window arg nil))))

(defun jens/move-border-left (arg)
  (interactive "P")
  (jens/move-border-left-or-right arg t))

(defun jens/move-border-right (arg)
  (interactive "P")
  (jens/move-border-left-or-right arg nil))

(defun jens/move-border-up (arg)
  (interactive "P")
  (jens/move-border-up-or-down arg t))

(defun jens/move-border-down (arg)
  (interactive "P")
  (jens/move-border-up-or-down arg nil))

;;;;;;;;;;;;;;;;;
;; misc defuns ;;
;;;;;;;;;;;;;;;;;

(defun jens/is-online-p ()
  "Returns a non-nil value if we have a network connection."
  (if (and (functionp 'network-interface-list)
           (network-interface-list))
      (-some (lambda (iface) (unless (equal "lo" (car iface))
                               (member 'up (first (last (network-interface-info
                                                         (car iface)))))))
             (network-interface-list))
    t))

(defun jens/insert-todays-date ()
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

;;;;;;;;;;;;;;
;; packages ;;
;;;;;;;;;;;;;;

;; We are going to use the bind-key (=:bind=) and diminish (=:diminish=)
;; functionalities, so we need to have those packages.
(use-package bind-key :ensure t)
(use-package diminish :ensure t :commands diminish)

;; some built-in packages

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
                                     (jens/with-supressed-message (recentf-save-list)))))
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

(use-package hideshow
  :defer t
  :bind (("C-c @ h" . hs-hide-block)
         ("C-c @ s" . hs-show-block)))

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

(use-package dired
  :defer t
  :commands dired
  :bind
  (("C-x C-d" . (lambda () (interactive) (dired default-directory)))
   :map dired-mode-map
   ("C-." . dired-omit-mode)
   ("<backspace>" . diredp-up-directory-reuse-dir-buffer))
  :config
  ;; pull in extra functionality for dired
  (require 'dired-x)
  (require 'dired+)

  (setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))
  (setq dired-listing-switches "-agholXN")
  (setq dired-create-destination-dirs 'always)
  (toggle-diredp-find-file-reuse-dir 1)

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
             org-babel-do-load-languages)
  :defines jens/load-org-agenda-files
  :bind
  (("C-x g " . org-agenda)
   :map org-mode-map
   ([(tab)] . company-indent-or-complete-common)
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

  (defun jens/load-org-agenda-files ()
    (interactive)
    (setq org-agenda-files
          (append '("")
                  (f-glob "**/*.org" "~/vault/org/planning"))))

  (advice-add 'org-agenda :before #'jens/load-org-agenda-files)

  (org-babel-do-load-languages 'org-babel-load-languages
                               '((emacs-lisp . t)
                                 (gnuplot . t)))

  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)
  ;; keep #+BEGIN_SRC blocks aligned with their contents
  (setq org-edit-src-content-indentation 0)
  ;; dont indent things
  (setq org-adapt-indentation nil)
  ;; syntax highlight org-mode code blocks when exporting as pdf
  (setq-default org-latex-listings 'minted
                org-latex-packages-alist '(("" "minted"))
                org-latex-pdf-process
                '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
                  "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
  ;; try to get non-fuzzy latex fragments
  (plist-put org-format-latex-options :scale 1.6)
  ;; (setq org-latex-create-formula-image-program 'dvisvgm) ;; obsolete as of org 9.0
  (setq-default org-preview-latex-default-process 'dvisvgm)

  ;; use some noise in scheduling org-drills
  (setq-default org-drill-add-random-noise-to-intervals-p t))

(use-package ob-async
  :ensure t
  :defer t)

(use-package ob-clojure
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
(use-package shell-script-mode
  :mode ("\\.sh\\'" "\\.zsh\\'" "\\zshrc\\'" "\\PKGBUILD\\'"))
(use-package octave-mode
  :mode "\\.m\\'")
(use-package scheme-mode
  :defer t
  :mode "\\.scm\\'"
  :config (setq-default scheme-program-name "csi -:c"))

;; homemade
(use-package botcode-mode
  :mode "\\.bot\\'")

;; from repos
(use-package cmake-mode
  :ensure t
  :mode "\\CmakeLists.txt\\'")
(use-package dockerfile-mode
  :ensure t
  :mode "\\Dockerfile\\'")
(use-package gitconfig-mode
  :ensure t
  :mode "\\.gitconfig\\'")
(use-package gitignore-mode
  :ensure t
  :mode "\\.gitignore\\'")
(use-package haskell-mode
  :ensure t
  :mode "\\.hs\\'")
(use-package lua-mode
  :ensure t
  :mode "\\.lua\\'")
(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" "\\.card\\'"))
(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'")
(use-package scss-mode
  :ensure t
  :mode "\\.scss\\'")
(use-package tuareg
  :ensure t
  :mode "\\.ocaml\\'")
(use-package yaml-mode
  :ensure t
  :mode "\\.yml\\'")
(use-package clojure-mode
  :ensure t
  :defer t
  :after (company-mode cider cljr-refactor)
  :functions (jens/clojure-mode-setup
              jens/company-clojure-quickhelp-at-point)
  :commands (cider-create-doc-buffer
             cider-try-symbol-at-point)
  :config
  (defun jens/clojure-mode-setup ()
    (interactive)
    ;; use company for clojure
    (auto-complete-mode -1)
    (company-mode +1)
    (add-to-list 'company-backends 'company-capf)
    (define-key clojure-mode-map (kbd "<tab>") #'company-indent-or-complete-common)
    (define-key clojure-mode-map (kbd "C-+") #'jens/company-clojure-quickhelp-at-point)

    ;; clojure emacs power settings
    (cider-mode +1)
    (clj-refactor-mode +1)

    ;; (setq cider-cljs-lein-repl
    ;;       "(do (require 'figwheel-sidecar.repl-api)
    ;;        (figwheel-sidecar.repl-api/start-figwheel!)
    ;;        (figwheel-sidecar.repl-api/cljs-repl))")

    (defun jens/company-clojure-quickhelp-at-point ()
      (interactive)
      (let* ((position (point)))
        (goto-char position)
        (cider-try-symbol-at-point "symbol to show doc for" #'cider-create-doc-buffer)
        (popup-tip (with-current-buffer "*cider-doc*"
                     (buffer-substring-no-properties (point-min) (point-max)))))))
  (add-hook 'clojure-mode-hook 'jens/clojure-mode-setup))

;; extensions to major modes:
(use-package cider
  :ensure t
  :defer t
  :config
  (setq cider-repl-use-pretty-printing t
        cider-prompt-for-symbol nil
        cider-pprint-fn 'pprint
        cider-repl-pop-to-buffer-on-connect nil
        cider-default-cljs-repl nil
        cider-check-cljs-repl-requirements nil)
  (advice-add 'cider-jack-in :before #'direnv-update-environment))

(use-package clj-refactor
  :ensure t
  :defer t
  :config
  ;; dont warn on refactor evals
  (setq cljr-warn-on-eval nil))

(use-package minimap
  :disabled t
  :ensure t
  :config
  (setq minimap-update-delay 0)
  (setq minimap-width-fraction 0.1)
  (setq minimap-minimum-width 30)
  (setq minimap-window-location 'right)
  (setq minimap-hide-fringes t)
  (setq minimap-recreate-window nil)
  :custom-face
  (minimap-font-face  ((t (:family "Source Code Pro" :height 15))))
  (minimap-active-region-background ((t (:background "gray20")))))

;; fancy modeline replacement
(use-package powerline
  :ensure t
  :demand t
  :config
  ;; Make the mode-line flat
  (set-face-attribute 'mode-line nil :box nil)
  (set-face-attribute 'mode-line-inactive nil :box nil)

  ;; Group colors
  (defface face-light '((t (:background "grey35" :inherit mode-line))) "" :group 'powerline)
  (defface face-dark '((t (:background "grey30" :inherit mode-line))) "" :group 'powerline)
  (defface face-darker '((t (:background "grey25" :inherit mode-line))) "" :group 'powerline)
  (defface face-darkest '((t (:background "grey20" :inherit mode-line))) "" :group 'powerline)

  ;; Setup the powerline theme
  (setq-default
   mode-line-format
   '("%e"
     (:eval
      (let* (
             (active (powerline-selected-window-active))
             (mode-line (if active 'mode-line 'mode-line-inactive))

             (face-light 'face-light)
             (face-dark 'face-dark)
             (face-darker 'face-darker)
             (face-darkest 'face-darkest)

             (powerline-default-separator 'bar)

             (seperator-> (intern (format "powerline-%s-%s"
                                          powerline-default-separator
                                          (car powerline-default-separator-dir))))

             (separator-< (intern (format "powerline-%s-%s"
                                          powerline-default-separator
                                          (cdr powerline-default-separator-dir))))

             (lhs (list
                   (powerline-buffer-id face-darkest 'l)
                   (powerline-raw " " face-darkest)

                   (funcall seperator-> face-darkest face-darker)

                   (powerline-raw " %4l (%p)" face-darker 'r)
                   (powerline-raw ":" face-darker 'l)
                   (powerline-raw "%3c " face-darker 'r)

                   (funcall seperator-> face-darker face-dark)

                   (powerline-major-mode face-dark 'l)
                   (powerline-process face-dark)
                   (powerline-minor-modes face-dark 'l)
                   (powerline-narrow face-dark 'l)

                   (powerline-raw " " face-dark)

                   (funcall seperator-> face-dark face-light)))

             (rhs (list
                   (funcall separator-< face-light face-darkest)
                   (powerline-vc face-darkest))))
        (concat
         (powerline-render lhs)
         (powerline-fill face-light (powerline-width rhs))
         (powerline-render rhs)))))))

;;;;;;;;;;;;;;;;;;;;;
;; auto completion ;;
;;;;;;;;;;;;;;;;;;;;;

;; COMPANY-MODE
(use-package company
  :ensure t
  :defer t
  :config
  (company-quickhelp-mode nil))

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
  :ensure t
  :demand t
  :diminish auto-complete-mode
  :commands (global-auto-complete-mode
             ac-symbol-documentation
             company-indent-or-complete-common)
  :functions (jens/ac-quick-help-at-point
              jens/ac-c++-mode-setup
              jens/ac-elisp-mode-setup)
  :bind
  (("C-+" . jens/ac-quick-help-at-point)
   ("C-<tab>" . auto-complete))
  :config
  (require 'auto-complete-config)
  (setq ac-auto-start t) ;; auto start completing
  (setq ac-show-menu t) ;; show the menu instantly
  (setq ac-show-menu-immediately-on-auto-complete t) ;; show the autocompletion menu instantly
  (setq ac-delay 0.1) ;; show completion menu quickly
  (setq ac-use-quick-help t) ;; use the help
  (setq ac-quick-help-delay 0.1) ;; show help quickly
  (setq ac-quick-help-prefer-pos-tip nil) ;; use popup-tips
  (setq ac-use-comphist t)
  (setq ac-comphist-file (concat my-emacs-data-dir "ac-history")) ;; move the history file
  (setq ac-ignore-case t)
  (setq-default ac-sources
                '(ac-source-imenu
                  ac-source-words-in-same-mode-buffers))
  ;; '(ac-source-words-in-buffer ac-source-imenu ac-source-yasnippet)

  (defun jens/ac-quick-help-at-point ()
    (interactive)
    (let* ((position (point))
           (string-under-cursor
            (buffer-substring-no-properties
             (progn (skip-syntax-backward "w_") (point))
             (progn (skip-syntax-forward "w_") (point)))))
      (goto-char position)
      (popup-tip (ac-symbol-documentation (intern string-under-cursor)))))

  (defun jens/ac-rust-mode-setup ()
    (add-hook 'rust-mode-hook #'racer-mode)
    (add-hook 'racer-mode-hook #'eldoc-mode)
    (define-key rust-mode-map (kbd "<C-tab>") #'company-indent-or-complete-common)
    (setq company-tooltip-align-annotations t))

  (defun jens/ac-c++-mode-setup ()
    ;; (require 'ac-clang)
    ;; (require 'ac-c-headers)
    (require 'ac-rtags)

    (defvar c++-include-files
      '("/usr/include"
        "/usr/include/c++/7.3.0"
        "/usr/include/c++/7.3.0/backward"
        "/usr/include/c++/7.3.0/x86_64-unknown-linux-gnu"
        "/usr/lib/gcc/x86_64-unknown-linux-gnu/7.3.0/include"
        "/usr/lib/gcc/x86_64-unknown-linux-gnu/7.3.0/include-fixed"
        "/usr/lib/clang/5.0.1/include"))

    (setq-default achead:include-directories c++-include-files)

    ;; (add-to-list 'ac-sources 'ac-source-semantic)
    (add-to-list 'ac-sources 'ac-source-rtags)
    ;; (add-to-list 'ac-sources 'ac-source-c-headers)
    ;; (add-to-list 'ac-sources 'ac-source-c-header-symbols t)

    ;; (add-to-list 'ac-sources 'ac-source-clang)
    ;; (setq ac-clang-flags (mapcar (lambda (item)(concat "-I" item)) c++-include-files))
    ;; (ac-clang-activate-after-modify)
    )
  (add-hook 'c++-mode-hook 'jens/ac-c++-mode-setup)

  (defun jens/ac-elisp-mode-setup ()
    (add-to-list 'ac-sources 'ac-source-functions) ;; elisp functions
    (add-to-list 'ac-sources 'ac-source-features) ;; elisp features
    (add-to-list 'ac-sources 'ac-source-symbols) ;; elisp symbols
    (add-to-list 'ac-sources 'ac-source-variables)) ;; elisp variables
  (add-hook 'emacs-lisp-mode-hook 'jens/ac-elisp-mode-setup)

  ;; (defun my-ac-latex-mode-setup ()
  ;;   (require 'auto-complete-auctex)
  ;;   (require 'ac-auctex-setup))
  ;; (add-hook 'latex-mode-hook 'my-ac-latex-mode-setup)

  ;;(defun my-ac-octave-mode-setup ()
  ;;  (require 'ac-octave)
  ;;  (add-to-list 'ac-sources 'ac-complete-octave))
  ;; (add-hook 'octave-mode-hook 'my-ac-octave-mode-setup)

  (global-auto-complete-mode t))

;;;;;;;;;;;;;;;;;;;
;; misc packages ;;
;;;;;;;;;;;;;;;;;;;

(use-package chicken-scheme :ensure t :defer t)
(use-package htmlize :ensure t :defer t)
(use-package flx :ensure t)
(use-package flycheck :disabled :ensure t :defer t)
(use-package git-timemachine :ensure t :defer t)
(use-package yasnippet :ensure t :defer t)

(use-package direnv
  :ensure t
  :config
  (advice-add 'cider-jack-in :before 'direnv-update-environment))

(use-package fill-column-indicator
  :disabled
  :ensure t
  :demand t
  :diminish fci-mode
  :config
  (setq fci-rule-width 1)
  (setq fci-rule-color "grey")
  (setq fci-rule-column 80))

(use-package smex
  :ensure t
  :defer 1
  :commands smex-initialize
  :config
  (setq smex-save-file (concat my-emacs-data-dir "smex-items"))
  (smex-initialize))

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
         ("M-K" . sp-backward-kill-sexp)
         ([(control shift e)] . sp-end-of-sexp)
         ([(control e)] . end-of-line)
         ([(shift next)] . sp-split-sexp)
         ([(shift prior)] . sp-join-sexp))
  :config
  (require 'smartparens-config)
  (setq sp-autoescape-string-quote nil)
  (smartparens-global-mode t)
  (show-smartparens-global-mode t))

(use-package paxedit
  :ensure t
  :defer t
  :diminish paxedit-mode
  :bind (("M-t t" .
          (lambda ()
            (interactive)
            (message "f -> transpose forward\nb -> transpose backward")
            (jens/one-shot-keymap '(("f" . paxedit-transpose-forward)
                                    ("b" . paxedit-transpose-backward)))))
         ("M-k" . paxedit-kill)
         ("M-<prior>" . paxedit-backward-up)
         ("M-<next>" . paxedit-backward-end))
  :init
  (add-hook 'emacs-lisp-mode-hook 'paxedit-mode)
  (add-hook 'clojure-mode-hook 'paxedit-mode))

(use-package diff-hl
  :ensure t
  :demand t
  :diminish diff-hl-mode
  :commands (global-diff-hl-mode diff-hl-mode)
  :defines jens/diff-hl-refresh
  :config (global-diff-hl-mode +1)
  (defun jens/diff-hl-refresh ()
    "refresh diff-hl"
    (interactive)
    (diff-hl-mode nil)
    (diff-hl-mode +1))

  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(use-package pdf-tools
  :commands pdf-tools-install
  :config
  (pdf-tools-install)
  ;; need to use plain isearch, pdf-tools hooks into it to handle searching
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward))

(use-package multiple-cursors
  :ensure t
  :bind
  (("C-d" . mc/mark-next-like-this)
   ("C-S-d" . mc/mark-previous-like-this)
   ("C-M-a" . mc/mark-all-like-this)
   ("C-M-m" . (lambda ()
                (interactive)
                (message "n -> mark next \nN -> skip to next \np -> mark previous \nP -> skip to previous")
                (jens/one-shot-keymap
                 '(("n" . mc/mark-next-like-this)
                   ("N" . mc/skip-to-next-like-this)
                   ("p" . mc/mark-previous-like-this)
                   ("P" . mc/skip-to-previous-like-this))))))
  :init
  (setq mc/list-file (concat my-emacs-data-dir "mc-lists")))

(use-package browse-kill-ring
  :ensure t
  :defer t
  :bind ("C-x C-y" . browse-kill-ring)
  :config (setq browse-kill-ring-quit-action 'save-and-restore))

(use-package ace-jump-mode
  :ensure t
  :defer t
  :bind
  (("C-ø" . ace-jump-char-mode)
   ("C-'" . ace-jump-line-mode)))

(use-package ace-jump-buffer
  :ensure t
  :defer t
  :bind ("C-x C-b" . ace-jump-buffer))

(use-package ace-jump-zap
  :ensure t
  :defer t
  :bind ("C-å" . ace-jump-zap-to-char))

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

(use-package clang-format :ensure t :defer t)

(use-package rtags
  :ensure t
  :defer t
  :diminish rtags-mode
  :bind
  (:map c++-mode-map
        ("M-." . rtags-find-symbol-at-point)
        ("M-," . rtags-location-stack-back)))

(use-package magit
  :ensure t
  :defer t
  :functions jens/magit-quit-session
  :bind
  (("C-x m" . magit-status)
   :map magit-file-mode-map
   ("C-x g" . nil)
   :map magit-mode-map
   ("C-c C-a" . magit-commit-amend)
   ("q" . jens/magit-quit-session))
  :config
  (setq magit-auto-revert-mode nil)
  (setq magit-log-arguments '("--graph" "--color" "--decorate" "-n256"))
  (setq magit-merge-arguments '("--no-ff"))

  ;; When using =magit-status=, just fill the entire screen, and jump back the the
  ;; previous window configuration when quitting magit.
  (defun jens/magit-status-fullscreen (orig-fun &rest args)
    "Saves window configuration, then opens magit in fullscreen"
    (window-configuration-to-register :magit-fullscreen)
    (apply orig-fun args)
    (delete-other-windows))
  (advice-add 'magit-status :around #'jens/magit-status-fullscreen)

  (defun jens/magit-quit-session ()
    "Restores the previous window configuration and kills the magit buffer"
    (interactive)
    ;; only kill the buffer if it's the actual buffer, this way we can
    ;; still get back to our previous configuration if we quit magit weirdly
    (if (s-prefix? "*magit:" (buffer-name (current-buffer)))
        (kill-buffer))
    (jump-to-register :magit-fullscreen)))

(use-package magithub
  :ensure t
  :after magit
  :config
  (magithub-feature-autoinject t))

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
  ;; (setq undo-tree-visualizer-timestamps nil)
  (setq undo-tree-visualizer-diff t)
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist `(("." . ,my-emacs-temp-dir)))

  ;; TODO: fix undo-tree-undo in region, in some cases it freezes.
  (defun jens/undo-tree-undo (orig-fun &rest args)
    "Keep the region when undoing inside region"
    (if (use-region-p)
        (let ((m (set-marker (make-marker) (mark)))
              (p (set-marker (make-marker) (point))))
          (apply orig-fun args)
          (goto-char p)
          (set-mark m)
          (set-marker p nil)
          (set-marker m nil))
      (apply orig-fun args)))
  (advice-add 'undo-tree-undo :around #'jens/undo-tree-undo)

  (global-undo-tree-mode))

(use-package smooth-scrolling
  :disabled
  :ensure t
  :config
  (setq smooth-scroll-margin 5)
  (smooth-scrolling-mode))

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
  ;; (which-key-setup-minibuffer)
  (which-key-setup-side-window-right)
  (setq which-key-max-description-length 40)
  (which-key-mode))

(use-package jist
  :ensure t
  :defer t
  :config (setq jist-enable-default-authorized 't))

(use-package wgrep
  :ensure t
  :defer t
  :after grep
  :bind
  (("C-S-g" . rgrep)
   :map grep-mode-map
   ("C-x C-q" . wgrep-change-to-wgrep-mode)
   ("C-x Ck" . wgrep-abort-changes)
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
  :functions qlot-slime
  :commands slime-start
  :config
  (defun qlot-slime (directory)
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
  :defer t
  :config
  (setq-default elfeed-search-filter "@1-month-ago +unread ")

  (defface youtube-elfeed '((t :foreground "#E0CF9F"))
    "face for youtube.com entries"
    :group 'elfeed-faces)
  (push '(youtube youtube-elfeed) elfeed-search-face-alist)
  (defface reddit-elfeed '((t :foreground "#9FC59F"))
    "face for reddit.com entries"
    :group 'elfeed-faces)
  (push '(reddit reddit-elfeed) elfeed-search-face-alist)
  (defface blog-elfeed '((t :foreground "#DCA3A3"))
    "face for blog entries"
    :group 'elfeed-faces)
  (push '(blog blog-elfeed) elfeed-search-face-alist)
  (defface emacs-elfeed '((t :foreground "#94BFF3"))
    "face for emacs entries"
    :group 'elfeed-faces)
  (push '(emacs emacs-elfeed) elfeed-search-face-alist)

  (setq elfeed-feeds (jens/load-from-file (concat my-emacs-dir "elfeeds.el"))))

(use-package elpy
  :ensure t
  :defer t
  :diminish elpy-mode
  :config
  (define-key elpy-mode-map (kbd "<C-up>") nil)
  (define-key elpy-mode-map (kbd "<C-down>") nil))

(use-package rainbow-mode :ensure t :defer t)

(use-package abbrev
  :demand t
  :commands read-abbrev-file
  :config
  (setq abbrev-file-name (concat my-emacs-data-dir "abbreviations"))
  (read-abbrev-file))

(use-package auth-source-pass
  :ensure t
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
  :after (auth-source-pass erc-hl-nicks)
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
        (error "Error: No open terminals."))
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
  (jens/ivy-load-views))

(use-package counsel
  :ensure t
  :after ivy
  :defer 1
  :diminish counsel-mode
  :functions jens/counsel-read-file-name
  :commands (counsel-mode counsel--find-file-matcher)
  :bind
  (("C-s" . counsel-grep-or-swiper)
   ("C-S-s" . counsel-rg)
   ("C-x f" . counsel-recentf)
   ("C-x C-f" . counsel-find-file)
   ("C-x C-i" . counsel-imenu)
   ("M-æ" . counsel-mark-ring)
   ("M-x" . counsel-M-x)
   ("M-b" . counsel-bookmark))
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

  ;; If a region is active, use that as the initial input for searching in the
  ;; buffer.
  (defun jens/counsel-grep-or-swiper (orig-fun &rest args)
    "Start searching with the region as initial input"
    (if (region-active-p)
        (let ((start (region-beginning))
              (end (region-end)))
          (deactivate-mark)
          (apply orig-fun (list (buffer-substring-no-properties start end))))
      (funcall orig-fun)))
  (advice-add 'counsel-grep-or-swiper :around #'jens/counsel-grep-or-swiper)

  (counsel-mode))

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

(use-package ggtags :ensure t :defer t)

(use-package dumb-jump :ensure t :defer t)

(use-package flyspell
  :ensure t
  :defer t
  :functions enable-spellchecking
  :commands (flyspell-mode
             flyspell-buffer)
  :config
  (ispell-change-dictionary "english")

  (defun enable-spellchecking ()
    (interactive)
    (ispell-change-dictionary "english")
    (flyspell-mode)
    (flyspell-buffer)))

(use-package zenburn-theme
  :ensure t
  :demand t
  :config
  (load-theme 'zenburn t)
  :custom-face
  (popup-tip-face ((t (:background "#cbcbbb" :foreground "#2b2b2b"))))
  (ivy-current-match ((t (:background "#4f4f4f" :weight bold :box t))))
  (diredp-dir-priv ((t (:foreground "#8CD0D3"))))
  (diredp-file-name ((t (:foreground "#DCDCCC"))))
  (persp-face-lighter-buffer-not-in-persp ((t (:foreground "#CC9393"))))
  (ac-candidate-face ((t (:foreground "#F0DFAF" :background "#313131"))))
  (ac-selection-face ((t (:foreground "#FEFEFE" :background "#3E3E3E")))))

(use-package fullscreen
  :bind ("M-f" . fullscreen-toggle))

(use-package today
  :demand t
  :config
  (setq today-directory "~/vault/org/planning/"))

;;;;;;;;;;;;;;;;;;;;;;;
;; advices and hooks ;;
;;;;;;;;;;;;;;;;;;;;;;;

;; When popping the mark, continue popping until the cursor actually
;; moves. also, if the last command was a copy - skip past all the
;; expand-region cruft.
(defun jens/pop-to-mark-command (orig-fun &rest args)
  "Call ORIG-FUN until the cursor moves. Try the repeated popping up to 10
  times."
  (let ((p (point)))
    (dotimes (i 10)
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

;; for major modes, replace with use-package
;; use 'C-c C-c' to compile across languages, and use a proper compile command
(add-hook 'c++-mode-hook
          '(lambda ()
             (set (make-local-variable 'compile-command)
                  (format "clang++ -std=c++17 -stdlib=libstdc++ %s -o %s" (jens/get-buffer-file-name+ext) (jens/get-buffer-file-name)))
             (local-set-key (kbd "C-d") nil)
             (local-set-key (kbd "C-c C-c") 'compile)
             (local-set-key (kbd "C-c n") 'clang-format-buffer)))

(add-hook 'java-mode-hook
          '(lambda ()
             (use-local-map nil)
             (set (make-local-variable 'compile-command)
                  (format "javac %s" (jens/get-buffer-file-name+ext)))
             (local-set-key (kbd "C-c C-c") 'compile)))

(add-hook 'csharp-mode-hook
          '(lambda ()
             (set (make-local-variable 'compile-command)
                  (format "xbuild %s" (file-name-directory (buffer-file-name))))
             (local-set-key (kbd "C-c C-c") 'compile)))

(add-hook 'tuareg-mode-hook
          '(lambda ()
             (use-local-map nil)
             (set (make-local-variable 'compile-command)
                  (format "ocamlopt -o %s %s" (jens/get-buffer-file-name) (jens/get-buffer-file-name+ext)))
             (local-set-key (kbd "C-c C-c") 'compile)))

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

;; Evaluate the current buffer/region
(global-set-key (kbd "C-c C-k") 'eval-buffer)
(global-set-key (kbd "C-c k") 'eval-region)

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

;; Move the delete windows, mnemonic is C-x OTHER
(global-set-key (kbd "C-x 0") nil)
(global-set-key (kbd "C-x 1") nil)
(global-set-key (kbd "C-x o") 'delete-other-windows)
(global-set-key (kbd "C-x p") 'delete-window)

;; Make Home and End to to the top and bottom of the buffer, we have C-a/e
(global-set-key (kbd "<home>") 'beginning-of-buffer)
(global-set-key (kbd "<end>") 'end-of-buffer)

;; find things at point
(global-set-key (kbd "M-.") 'xref-find-definitions)
(global-set-key (kbd "M-,") 'xref-pop-marker-stack)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; keybindings for defuns ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Better C-a
(global-set-key (kbd "C-a") 'jens/smart-line-beginning)

;; Join lines (pull the below line up to this one)
(global-set-key (kbd "M-j") 'jens/join-region-or-line)

;; Comment/uncomment block
(global-set-key (kbd "C-c c") 'jens/comment-uncomment-region-or-line)

;; Fix spaces / tabs
(global-set-key (kbd "C-c n") 'jens/cleanup-buffer)

;; Enable backwards killing of lines
(global-set-key (kbd "C-S-k") 'jens/kill-to-beginning-of-line)

;; Toggle window split
(global-set-key (kbd "M-C-<tab>") 'jens/toggle-window-split)
(global-set-key (kbd "M-S-<iso-lefttab>") 'jens/rotate-windows)

;; Transpose stuff with M-t
(global-set-key (kbd "M-t w") 'transpose-words)
(global-set-key (kbd "M-t s") 'transpose-sexps)
;; (global-set-key (kbd "M-t p") 'transpose-params) ;; TODO: make this better

(global-set-key (kbd "C-x b") 'ibuffer)

;; Move windows with S-<arrow>
(windmove-default-keybindings 'shift)

;; Force save a file, mnemonic is C-x TOUCH
(global-set-key (kbd "C-x t") 'jens/touch-buffer-file)

;; Copy current line / region
(global-set-key (kbd "M-w") 'jens/save-region-or-current-line)
(global-set-key (kbd "C-w") 'jens/kill-region-or-current-line)

;; Completion that uses many different methods to find options.
;; (global-set-key (kbd "C-.") 'hippie-expand-no-case-fold)
;; (global-set-key (kbd "C-:") 'hippie-expand-lines)
;; (global-set-key (kbd "C-,") 'completion-at-point)

;; keybindings for window resizing
(global-set-key (kbd "M-S-<left>") 'jens/move-border-left)
(global-set-key (kbd "M-S-<right>") 'jens/move-border-right)
(global-set-key (kbd "M-S-<up>") 'jens/move-border-up)
(global-set-key (kbd "M-S-<down>") 'jens/move-border-down)

;;;;;;;;;;;;;;;;
;; tty things ;;
;;;;;;;;;;;;;;;;

;; Setup for when emacs is running in the terminal (i.e. =emacs -nw=).
;; It tries to fix some of the keys that terminals normally scramble or forget to
;; send over the wire, someday we can hopefully get a terminal that does the right
;; thing.
(if (not (window-system))
    (progn
      (define-key function-key-map "\e[25~" [(control return)])
      (define-key input-decode-map "\e[26~" [(control shift return)])
      (define-key input-decode-map "\e[28~" [(meta shift return)])
      (define-key input-decode-map "\e[29~" [(meta shift left)])
      (define-key input-decode-map "\e[31~" [(meta shift right)])
      (define-key input-decode-map "\e[32~" [(meta shift up)])
      (define-key input-decode-map "\e[33~" [(meta shift down)])
      (define-key input-decode-map "\e[34~" [(meta left)])
      (define-key input-decode-map "\e[35~" [(meta right)])
      (define-key input-decode-map "\e[36~" [(meta up)])
      (define-key input-decode-map "\e[37~" [(meta down)])
      (define-key input-decode-map "\e[38~" [(control left)])
      (define-key input-decode-map "\e[39~" [(control right)])
      (define-key input-decode-map "\e[40~" [(control up)])
      (define-key input-decode-map "\e[41~" [(control down)])
      (define-key input-decode-map "\e[42~" [(shift left)])
      (define-key input-decode-map "\e[43~" [(shift right)])
      (define-key input-decode-map "\e[44~" [(shift up)])
      (define-key input-decode-map "\e[45~" [(shift down)])
      (define-key input-decode-map "\e[46~" [(control shift left)])
      (define-key input-decode-map "\e[47~" [(control shift right)])
      (define-key input-decode-map "\e[48~" [(control shift up)])
      (define-key input-decode-map "\e[49~" [(control shift down)])
      (define-key input-decode-map "\e[50~" [(shift tab)])
      (define-key input-decode-map "\e[51~" [(shift return)])
      (define-key input-decode-map "\e[52~" [(control meta left)])
      (define-key input-decode-map "\e[53~" [(control meta right)])
      (define-key input-decode-map "\e[54~" [(control meta up)])
      (define-key input-decode-map "\e[55~" [(control meta down)])

      (define-key function-key-map "\eOA" [up])
      (define-key function-key-map "\e[A" [up])
      (define-key function-key-map "\eOB" [down])
      (define-key function-key-map "\e[B" [down])
      (define-key function-key-map "\eOC" [right])
      (define-key function-key-map "\e[C" [right])
      (define-key function-key-map "\eOD" [left])
      (define-key function-key-map "\e[D" [left])

      (define-key input-decode-map "^[[A" [up])
      (define-key input-decode-map "^[[B" [down])
      (define-key input-decode-map "^[[C" [right])
      (define-key input-decode-map "^[[D" [left])))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; experimantal things ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar user-agent-string "User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_3) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/44.0.2403.89 Safari/537.36")
(defun get-title-from-link (link)
  (letrec ((-silence-output " -so - ")
           (-follow-redirects "-L")
           (-use-user-agent "'-A User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_3) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/44.0.2403.89 Safari/537.36'")
           (grep-title "grep -iPo '(?<=<title>)(.*)(?=</title>)'")
           (recode-to-utf8 "recode html..utf8")
           (curl-command (concat "curl" " " -follow-redirects " " -use-user-agent " '" link "'" -silence-output " | " grep-title " | " recode-to-utf8)))
    (s-trim (shell-command-to-string curl-command))))

;;(get-title-from-link "https://www.youtube.com/watch?v=mO3Q4bRQZ3k")

(defun paste-website-title-above ()
  (interactive)
  (if (region-active-p)
      (let ((link (buffer-substring (region-beginning) (region-end))))
        (save-excursion
          (beginning-of-line)
          (newline-and-indent)
          (forward-line -1)
          (insert (get-title-from-link link))))))

(defun link-to-markdown-link ()
  (interactive)
  (if (region-active-p)
      (letrec ((link (buffer-substring (region-beginning) (region-end)))
               (link-title (get-title-from-link link)))
        (save-excursion
          (delete-region (region-beginning) (region-end))
          (insert "[" link-title "]")
          (insert "(" link ")")))))

(defun link-to-org-link ()
  (interactive)
  (if (region-active-p)
      (letrec ((link (buffer-substring (region-beginning) (region-end)))
               (link-title (get-title-from-link link)))
        (save-excursion
          (delete-region (region-beginning) (region-end))
          (insert "[")
          (insert "[" link "]")
          (insert "[" link-title "]")
          (insert "]")))))

(msg-success (format "Emacs initialized in %s, with %s garbage collections." (emacs-init-time) gcs-done))

(use-package evil
  :ensure t
  :disabled t)

(defun modal-movement ()
  (interactive)
  (message "MODAL MOVEMENT MODE")
  (jens/one-shot-keymap
   '(("i" . (lambda () (interactive) (previous-line)))
     ("k" . (lambda () (interactive) (next-line)))
     ("j" . (lambda () (interactive) (backward-char)))
     ("l" . (lambda () (interactive) (forward-char)))

     ("C-i" . (lambda () (interactive) (scroll-down 5)))
     ("C-k" . (lambda () (interactive) (scroll-up 5)))
     ("C-j" . (lambda () (interactive) (left-word)))
     ("C-l" . (lambda () (interactive) (right-word)))

     ("w" . (lambda (arg) (interactive "P") (jens/kill-region-or-current-line arg)))
     ("e" . (lambda () (interactive) (end-of-line)))
     ("a" . (lambda () (interactive) (beginning-of-line)))
     ("u" . (lambda () (interactive) (scroll-down-command)))
     ("o" . (lambda () (interactive) (scroll-up-command)))
     ))
  )

(global-set-key (kbd "<escape>") 'modal-movement)

;; reset the things we disabled earlier
;; set garbage collection to 20 mb
(setq gc-cons-threshold 16777216 gc-cons-percentage 0.1)
(setq file-name-handler-alist default-file-name-handler-alist)

(provide 'init)
