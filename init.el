;;; -*- lexical-binding: t -*-
;;; prelude
;;;; early setup

;; use lexical binding for initialization code
(setq-default lexical-binding t)

;; some functions for logging
(defun log-info (txt) (message "\n# %s" txt))
(defun log-warning (txt) (message "! %s" txt))
(defun log-success (txt) (message "@ %s" txt))

(log-info "Started initializing emacs!")

(call-interactively #'emacs-version)
(message "Commit: %s (%s)" emacs-repository-version emacs-repository-branch)

(log-info "Doing early initialization")

;; turn off excess interface early in startup to avoid momentary display
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)

;; user directories/files
(defconst user-elpa-directory (locate-user-emacs-file "elpa/"))
(defconst user-straight-directory (locate-user-emacs-file "straight/"))
(defconst user-lisp-directory (locate-user-emacs-file "lisp/"))
(defconst user-vendor-directory (locate-user-emacs-file "vendor/"))
(defconst user-secrets-file (locate-user-emacs-file "secrets.el.gpg"))

(defconst user-mail-directory "~/private/mail")
(defconst user-contacts-files '("~/vault/contacts.org.gpg"))

;; add user directories to the load-path
(add-to-list 'load-path user-lisp-directory)
(add-to-list 'load-path user-elpa-directory)
(add-to-list 'load-path user-vendor-directory)

;; setup package archives
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")))

;; setup font, a bit convoluted because we also want the first frame
;; spawned by the daemon to use the face.
(defun jens/init-fonts ()
  "Setup font configuration for new frames."
  (let ((my-font "Source Code Pro Semibold 10"))
    (if (not (find-font (font-spec :name my-font)))
        (log-warning (format "could not find font: %s" my-font))
      (add-to-list 'default-frame-alist `(font . ,my-font))
      (set-frame-font my-font))

    ;; only setup fonts once
    (remove-hook 'server-after-make-frame-hook #'jens/init-fonts)))

(unless (daemonp)
  (jens/init-fonts))

(add-hook 'server-after-make-frame-hook #'jens/init-fonts)

;;;; fundamental third-party packages

(log-info "Loading fundamental third-party packages")

;; make sure straight.el is installed
(defvar bootstrap-version)
(let ((bootstrap-file (locate-user-emacs-file "straight/repos/straight.el/bootstrap.el"))
      (bootstrap-version 5))
  (message "bootstrapping straight...")
  (unless (file-exists-p bootstrap-file)
    (log-info "straight.el was not found, installing.")
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(message "bootstrapping use-package...")
(straight-use-package 'use-package)
(setq straight-vc-git-default-protocol 'ssh)

;; need to enable imenu support before requiring `use-package'
(setq use-package-enable-imenu-support t)
;; make use-package tell us what its doing
(setq use-package-verbose t)
(setq use-package-compute-statistics t)
(setq use-package-minimum-reported-time 0.01)

;; needs to be required after its settings are set
(require 'use-package)

(use-package modern :demand t)
(use-package contrib :demand t)

(use-package dash ;; working with lists, -map, -concat, etc
  :demand t
  :straight (dash :host github :repo "magnars/dash.el"
                  :fork (:host github :repo "jensecj/dash.el")))

(use-package dash-functional :commands (-cut) :demand t)

(use-package s ;; string things, s-trim, s-replace, etc.
  :demand t
  :straight (s :host github :repo "magnars/s.el"
               :fork (:host github :repo "jensecj/s.el")))

(use-package f ;; file-system things, f-exists-p, f-base, etc.
  :demand t
  :straight (f :host github :repo "rejeep/f.el"
               :fork (:host github :repo "jensecj/f.el")))

(use-package ht ;; a great hash-table wrapper.
  :demand t
  :straight (ht :host github :repo "Wilfred/ht.el"
                :fork (:host github :repo "jensecj/ht.el")))

(progn
  ;; add :download keyword to `use-package' to easily download files
  ;; from the web
  ;; TODO: make work with list of strings, see :load-path?
  (defun use-package-normalize/:download (_name-symbol keyword args)
    (use-package-only-one (symbol-name keyword) args
      (lambda (_label arg)
        (cond
         ((stringp arg) arg)
         ((symbolp arg) (symbol-name arg))
         (t
          (use-package-error
           ":download wants a url (a string)"))))))

  (defun use-package-handler/:download (name _keyword url rest state)
    (let* ((file (url-unhex-string (f-filename url)))
           (dir user-vendor-directory)
           (path (f-join dir file)))
      (if (f-exists-p path)
          (message "%s already exists, skipping download." file)
        (message "%s does not exist, downloading %s to %s" file url path)
        (when (not (f-exists-p dir))
          (f-mkdir dir))
        (condition-case _ex
            (url-copy-file url path)
          (error '())))
      (use-package-concat
       (use-package-process-keywords name rest state))))

  (add-to-list 'use-package-keywords :download))

(use-package advice-patch ;; easy way to patch packages
  :download "https://raw.githubusercontent.com/emacsmirror/advice-patch/master/advice-patch.el"
  :demand t
  :config
  (advice-add #'advice-patch :before
              (lambda (symbol &rest args)
                "Print what is patched."
                (message "patching %s" symbol))))

;; We are going to use the bind-key (`:bind') and diminish (`:diminish')
;; extensions of `use-package', so we need to have those packages.
(use-package bind-key :straight t :demand t)
(use-package diminish :straight t :demand t)
(use-package delight :straight t :demand t)
(use-package hydra :straight t :demand t)
(use-package shut-up :straight t :demand t)
(use-package ov :straight t :demand t)

(use-package async
  :straight t
  :demand t
  :config
  (defmacro async! (body &optional callback)
    `(async-start
      (lambda ()
        ,(async-inject-variables "^load-path$")
        ,body)
      ,(if callback
           `(lambda (res) (funcall ,callback res))))))

;;;; cache, temp files, etc.

(log-info "Setting up cache / temp-files / etc.")

;; contain extra files in etc/ and var/.
;; load early, and overwrite locations in configs if needed.
(use-package no-littering :straight t :demand t)

(setq temporary-file-directory (no-littering-expand-var-file-name "temp/"))
(setq bookmark-default-file (no-littering-expand-etc-file-name "bookmarks.el"))

(let ((auto-save-dir (no-littering-expand-var-file-name "auto-save/")))
  (setq auto-save-file-name-transforms `((".*" ,auto-save-dir t)))
  (setq auto-save-list-file-prefix auto-save-dir))

(setq auto-save-no-message t)

;; don't use the customize system, all settings are keps in this file
(setq custom-file null-device)

;; don't load default.el
(setq inhibit-default-init t)

;;;; fundamental defuns

(log-info "Defining fundamental defuns")

(defmacro comment (&rest _args) "Ignore everything inside this sexp.")

(defmacro xi (&rest body)
  "Convenience macro for creating interactive lambdas."
  `(lambda ()
     (interactive)
     ,@body))

(defun longest-list (&rest lists)
  (-max-by
   (lambda (a b)
     (if (and (seqp a) (seqp b))
         (> (length a) (length b))
       1))
   lists))

;; (longest-list 'i '(1 2 3) 'z '(a b c) '(æ ø å))

(defun broadcast (&rest args)
  (let ((max-width (length (apply #'longest-list args))))
    (-map-when
     (lambda (a) (atom a))
     (lambda (a) (-repeat max-width a))
     args)))

;; (broadcast 'a '(1 2 3) 'z '(a b c) '(æ ø å))

(defun -mapply (fn atom-or-list)
  "If ATOM-OR-LIST if a list, map FN over it, otherwise apply FN."
  (cond
   ((listp atom-or-list) (-map fn atom-or-list))
   (t (funcall fn atom-or-list))))

(defun transpose (&rest lists)
  (apply #'-mapcar #'-list lists))

;; (transpose '(1 2 3) '(a b c))

(defun apply* (fn &rest args)
  "Apply FN to all combinations of ARGS."
  (let* ((args (-map-when #'atom #'list args))
         (combinations (apply #'-table-flat #'list args)))
    (dolist (c combinations)
      (apply fn c))))

(defun add-hook* (hooks fns &optional append local)
  "Add FNS to HOOKS."
  (apply* (-cut add-hook <> <> append local) hooks fns))

(defun remove-hook* (hooks fns &optional local)
  "Remove FNS from HOOKs."
  (apply* (-cut remove-hook <> <> local) hooks fns))

(defun advice-add* (syms where fns)
  "Advice FNS to SYMS."
  (apply* (-cut advice-add <> where <>) syms fns))

(defun advice-remove* (syms fns)
  "Remove advice FNS from SYMS."
  (apply* #'advice-remove syms fns))

(defun add-to-list* (list-var elements &optional append compare-fn)
  "Add multiple ELEMENTS to a LIST-VAR."
  (dolist (e elements)
    (add-to-list list-var e append compare-fn)))

(defmacro remove-from-list (list-var element &optional compare-fn)
  (let ((compare-fn (or compare-fn #'equal)))
    `(setf ,list-var (seq-remove (lambda (e) (,compare-fn ,element e)) ,list-var))))

(defun advice-nuke (sym)
  "Remove all advice from symbol SYM."
  (advice-mapc (lambda (advice _props) (advice-remove sym advice)) sym))

(defun add-one-shot-hook (hook fn &optional append local)
  "Add FN to HOOK, and remove it after is has triggered once."
  (let ((sym (gensym "one-shot-hook-")))
    (fset sym
          (lambda ()
            (remove-hook hook sym local)
            (funcall fn)))
    (add-hook hook sym append local)))

(defun add-one-shot-hook* (hooks fns &optional append local)
  "Add FNS to HOOKS, and remove them after triggering once."
  (apply* (-cut add-one-shot-hook <> <> append local) hooks fns))

(defmacro before-next-command (&rest body)
  "Execute BODY before the next command is run.

Inside BODY `this-command' is bound to the command that is about
to run and `this-command-keys' returns the key pressed."
  `(add-one-shot-hook 'pre-command-hook
                      (lambda () ,@body)))


(defun fn-checksum (fn &optional checksum)
  "Return a string md5 checksum of FN, or, given CHECKSUM, test
equality of computed checksum and arg."
  (let* ((def (symbol-function fn))
         (str (prin1-to-string def))
         (hash (md5 str)))
    (if checksum
        (string= hash checksum)
      hash)))

;;; built-in
;;;; settings

(log-info "Redefining built-in defaults")

;; location of emacs source files
(let ((src-dir "/home/jens/emacs/src/"))
  (if (f-exists-p src-dir)
      (setq source-directory src-dir)
    (log-warning "Unable to locate emacs source directory.")))

;; hide the splash screen
(setq inhibit-startup-message t)

;; don't disable function because they're confusing to beginners
(setq disabled-command-function nil)

;; never use dialog boxes
(setq use-dialog-box nil)

;; load newer files, even if they have outdated byte-compiled counterparts
(setq load-prefer-newer t)

;; don't blink the cursor
(blink-cursor-mode -1)

;; always highlight current line
(setq global-hl-line-sticky-flag t)
(global-hl-line-mode +1)

;; allow pasting selection outside of Emacs
(setq select-enable-clipboard t)

;; show keystrokes in progress
(setq echo-keystrokes 0.1)

;; move files to trash when deleting
(setq delete-by-moving-to-trash t)

;; don't use shift to mark things
(setq shift-select-mode nil)

;; always display text left-to-right
(setq-default bidi-display-reordering nil) ; FIXME: non-nil values cause "Reordering buffer..."
;; (setq-default bidi-paragraph-direction 'left-to-right)

;; fold characters in searches (e.g. 'a' matches 'â')
(setq search-default-mode 'char-fold-to-regexp)
(setq replace-char-fold t)

;; transparently open compressed files
(auto-compression-mode t)

;; enable syntax highlighting
(global-font-lock-mode t)

;; answering just 'y' or 'n' will do
(defalias 'yes-or-no-p 'y-or-n-p)

;; make re-centering more intuitive
(setq recenter-positions '(top middle bottom))
(global-set-key (kbd "C-l") #'recenter-top-bottom)
(global-set-key (kbd "C-S-L") #'move-to-window-line-top-bottom)

;; use UTF-8
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; make backups of files, even when they're in version control, and set how many
;; backups we want to save for each file.
(setq make-backup-files t
      backup-by-copying t
      vc-make-backup-files t
      version-control t
      delete-old-versions t
      kept-old-versions 9
      kept-new-versions 9
      auto-save-default t)

;; show active region
(transient-mark-mode 1)

;; remove text in active region if inserting text
(delete-selection-mode 1)

;; display line and column numbers in mode-line
(setq line-number-mode t)
(setq column-number-mode t)

(setq-default fill-column 90)

;; show location of cursor in non-selected windows
(setq cursor-in-non-selected-windows t)

;; select the help window after spawning it
(setq help-window-select t)

;; undo/redo window configuration with C-c <left>/<right>
(winner-mode 1)

;; move windows with S-<arrow>
(windmove-default-keybindings 'shift)

;; use spaces instead of tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
;; this messes with less things when indenting,
;; tabs are converted to spaces automatically
(setq-default indent-line-function 'insert-tab)

;; show me empty lines after buffer end
(setq indicate-empty-lines t)

;; don't automatically break lines
(setq-default truncate-lines t)
(setq-default truncate-partial-width-windows t)

;; allow recursive mini buffers
(setq enable-recursive-minibuffers t)

;; show everything that's happening when evaluating something
(setq eval-expression-print-level nil)

;; end files with a newline
(setq require-final-newline nil)

;; save before compiling, don't ask
(setq compilation-ask-about-save nil)

;; save more things in the kill ring
(setq kill-ring-max 500)

;; keep a lot more undo history (expressed in bytes)
(setq undo-limit (* 10 1024 1024))
(setq undo-strong-limit (* 30 1024 1024))

;; remember a lot of messages
(setq message-log-max 10000)

(setq auto-window-vscroll nil)

;; if moving the point more than 10 lines away,
;; center point in the middle of the window, otherwise be conservative.
(setq scroll-conservatively 10)

;; only scroll the current line when moving outside window-bounds
(setq auto-hscroll-mode 'current-line)

(setq fast-but-imprecise-scrolling t)

;; save clipboard from other programs to kill-ring
(setq save-interprogram-paste-before-kill t)

;; always follow symlinks
(setq vc-follow-symlinks t)

;; just give me a clean scratch buffer
(setq initial-scratch-message "")
(setq initial-major-mode 'emacs-lisp-mode)

;; default regexp for files to hide in dired-omit-mode
;; FIXME: this needs to be toplevel, otherwise dired+ fails to load...
(setq-default dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\..+$\\|^\\..+$")

;; don't show trailing whitespace by default
(setq-default show-trailing-whitespace nil)
(setq whitespace-style '(face trailing))

;; timestamp messages in the *Warnings* buffer
(setq warning-prefix-function
      (lambda (_level entry)
        (insert (format-time-string "[%H:%M:%S] "))
        entry))

;; When popping the mark, continue popping until the cursor actually
;; moves. also, if the last command was a copy - skip past all the
;; expand-region cruft.
(defun jens/pop-to-mark-command (orig-fun &rest args)
  "Call ORIG-FUN until the cursor moves.  Try popping up to 10
times."
  (let ((p (point)))
    (dotimes (_ 10)
      (when (= p (point))
        (apply orig-fun args)))))
(advice-add #'pop-to-mark-command :around #'jens/pop-to-mark-command)

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

;; create read-only directory class
(dir-locals-set-class-variables
 'read-only
 '((nil . ((buffer-read-only . t)))))

;; start some files in read-only buffers by default
(dolist (dir (list user-elpa-directory
                   user-vendor-directory
                   source-directory))
  (dir-locals-set-directory-class (file-truename dir) 'read-only))

;; package.el should ignore elpa/ files being read-only
(advice-add #'package-install :around
            (lambda (fn &rest args)
              (let ((inhibit-read-only t))
                (apply fn args))))

;;;;; authentication and security

;; set the paranoia level to medium, warns if connections are insecure
(setq network-security-level 'medium)

(use-package auth-source
  :demand t
  :config
  (setq authinfo-mode)                  ; TODO: make sure reveal-mode hooks to the files
  (setq authinfo-hidden (rx (or "password" "secret")))
  (setq auth-sources `(,(expand-file-name "~/vault/.authinfo.gpg")))
  (setq auth-source-save-behavior nil))

(use-package auth-source-pass
  ;; :straight t  ; TODO: is auth-source-pass now part of emacs?
  :commands (auth-source-pass-enable auth-source-pass-get)
  :config
  (auth-source-pass-enable)
  ;; using authinfo.gpg
  ;; (letrec ((auth (auth-source-search :host "freenode"))
  ;;          (user (plist-get (car auth) :user))
  ;;          (secret (plist-get (car auth) :secret)))
  ;;   (message (format "user: %s, secret: %s" user (funcall secret))))

  ;; using password-store
  ;; (auth-source-pass-get 'secret "irc/freenode/jensecj")
  )

(use-package epa-file
  :demand t
  :commands epa-file-enable
  :config
  (setq epg-pinentry-mode 'loopback)
  (epa-file-enable)

  (defun epa/gpg-key-in-cache (key)
    "Return whether KEY is in the GPG cache."
    ;; TODO: check if key is in gpg-agent cache entirely in elisp
    (when-let ((keys (s-split " " (s-trim (shell-command-to-string "gpg-cached")))))
      (member key keys)))

  (defun epa/gpg-cache-key (key)
    "Call GPG to cache KEY in the GPG-agent."
    (interactive)
    (when-let ((buf (get-buffer-create "*gpg-login*"))
               (pw (read-passwd (format "%s key: " key)))
               (proc (make-process
                      :name "gpg-login-proc"
                      :buffer buf
                      :command `("gpg"
                                 "--quiet"
                                 "--no-greeting"
                                 "--pinentry-mode" "loopback"
                                 "--clearsign"
                                 "--output" "/dev/null"
                                 "--default-key" ,key
                                 "--passphrase-fd" "0"
                                 "/dev/null"))))
      (process-send-string proc (format "%s\n" pw))))
  )

(use-package pinentry ;; enable GPG pinentry through the minibuffer
  :straight t
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

  ;; need to reset the pinentry from time to time, otherwise it stops working?
  (setq jens/gpg-reset-timer (run-with-timer 0 (* 60 45) #'jens/pinentry-reset))
  ;; (cancel-timer gpg-reset-timer)
  )

(defun jens/kill-idle-gpg-buffers ()
  "Kill .gpg buffers after they have not been used for 120
seconds."
  (interactive)
  ;; TODO: maybe use `midnight-mode'
  (let ((buffers-killed 0))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (string-match ".*\.gpg$" (buffer-name buffer))
          (let ((current-time (second (current-time)))
                (last-displayed-time (second buffer-display-time)))
            (when (or (not last-displayed-time)
                      (> (- current-time last-displayed-time) 60))
              (message "killing %s" (buffer-name buffer))
              (when (buffer-modified-p buffer)
                (save-buffer))
              (kill-buffer buffer)
              (incf buffers-killed))))))
    (unless (zerop buffers-killed)
      (message "%s .gpg buffer(s) saved and killed" buffers-killed))))

(setq jens/kill-idle-gpg-buffers-timer (run-with-idle-timer 120 t 'jens/kill-idle-gpg-buffers))

;;;; packages

(log-info "Configuring built-in packages")

;; load theme colors early, so they can be used for customizing other packages
(defvar zenburn-colors
  '(("zenburn-fg+1"     . "#FFFFEF")
    ("zenburn-fg"       . "#DCDCCC")
    ("zenburn-fg-1"     . "#656555")
    ("zenburn-bg-2"     . "#000000")
    ("zenburn-bg-1"     . "#2B2B2B")
    ("zenburn-bg-05"    . "#383838")
    ("zenburn-bg"       . "#3F3F3F")
    ("zenburn-bg+05"    . "#494949")
    ("zenburn-bg+1"     . "#4F4F4F")
    ("zenburn-bg+2"     . "#5F5F5F")
    ("zenburn-bg+3"     . "#6F6F6F")
    ("zenburn-red+2"    . "#ECB3B3")
    ("zenburn-red+1"    . "#DCA3A3")
    ("zenburn-red"      . "#CC9393")
    ("zenburn-red-1"    . "#BC8383")
    ("zenburn-red-2"    . "#AC7373")
    ("zenburn-red-3"    . "#9C6363")
    ("zenburn-red-4"    . "#8C5353")
    ("zenburn-red-5"    . "#7C4343")
    ("zenburn-red-6"    . "#6C3333")
    ("zenburn-orange"   . "#DFAF8F")
    ("zenburn-yellow"   . "#F0DFAF")
    ("zenburn-yellow-1" . "#E0CF9F")
    ("zenburn-yellow-2" . "#D0BF8F")
    ("zenburn-green-5"  . "#2F4F2F")
    ("zenburn-green-4"  . "#3F5F3F")
    ("zenburn-green-3"  . "#4F6F4F")
    ("zenburn-green-2"  . "#5F7F5F")
    ("zenburn-green-1"  . "#6F8F6F")
    ("zenburn-green"    . "#7F9F7F")
    ("zenburn-green+1"  . "#8FB28F")
    ("zenburn-green+2"  . "#9FC59F")
    ("zenburn-green+3"  . "#AFD8AF")
    ("zenburn-green+4"  . "#BFEBBF")
    ("zenburn-cyan"     . "#93E0E3")
    ("zenburn-blue+3"   . "#BDE0F3")
    ("zenburn-blue+2"   . "#ACE0E3")
    ("zenburn-blue+1"   . "#94BFF3")
    ("zenburn-blue"     . "#8CD0D3")
    ("zenburn-blue-1"   . "#7CB8BB")
    ("zenburn-blue-2"   . "#6CA0A3")
    ("zenburn-blue-3"   . "#5C888B")
    ("zenburn-blue-4"   . "#4C7073")
    ("zenburn-blue-5"   . "#366060")
    ("zenburn-magenta"  . "#DC8CC3"))
  "Zenburn colors")

(defun zenburn-get (color)
  (cdr (assoc color zenburn-colors)))

;;;;; major-modes

(use-package octave :mode ("\\.m\\'" . octave-mode))

(use-package sh-script
  :mode (("\\.sh\\'" . shell-script-mode)
         ("\\.zsh\\'" . shell-script-mode)
         ("\\.zshrc\\'" . shell-script-mode)
         ("\\.zshenv\\'" . shell-script-mode)
         ("\\.zprofile\\'" . shell-script-mode)
         ("\\.profile\\'" . shell-script-mode)
         ("\\.bashrc\\'" . shell-script-mode)
         ("\\.bash_profile\\'" . shell-script-mode)
         ("\\.extend.bashrc\\'" . shell-script-mode)
         ("\\.xinitrc\\'" . shell-script-mode)
         ("\\.PKGBUILD\\'" . shell-script-mode))
  :config
  (add-hook* 'sh-mode-hook '(flymake-mode flycheck-mode)))

(use-package conf-mode
  :defer t
  :config
  (defun conf-mode/indent ()
    ;; FIXME: does not work when called with indent-region
    (interactive)
    (save-excursion
      (beginning-of-line)

      (let ((col (syntax-ppss-depth (syntax-ppss))))
        (save-excursion
          ;; indent lines that are part of multi-line commands
          (skip-syntax-backward "-")
          (backward-char 2)
          (if (looking-at (rx ?\\ eol))
              (incf col)))

        (save-excursion
          ;; dont indent the terminating line of a block
          (skip-syntax-forward "-")
          (if (looking-at (rx (or ?\] ?\} ?\))))
              (decf col)))

        (indent-line-to (* col tab-width)))))

  (require 'mode-local)
  (setq-mode-local conf-space-mode indent-line-function #'conf-mode/indent))

(use-package scheme
  :defer t
  :mode ("\\.scm\\'" . scheme-mode)
  :config (setq scheme-program-name "csi -:c"))

(use-package python
  :defer t
  :bind
  (:map python-mode-map
        ("C-c C-c" . projectile-compile-project)
        ("M-," . nil)
        ("M-." . nil)
        ("M--" . nil)))

(use-package cc-mode
  :bind
  (:map java-mode-map
        ("C-c C-c" . projectile-compile-project))
  (:map c++-mode-map
        ("C-c C-c" . projectile-compile-project)
        ("C-c n" . clang-format-buffer))
  :config
  (dolist (k '("\M-," "\M-." "\M--"))
    (bind-key k nil c-mode-base-map)))

(use-package make-mode
  :mode (("\\justfile\\'" . makefile-mode)
         ("\\Makefile\\'" . makefile-mode))
  :config
  (setq-mode-local makefile-mode whitespace-style
                   '(face indentation space-before-tab space-after-tab trailing lines))
  (setq-mode-local makefile-mode indent-tabs-mode t)
  ;; TODO: fix indentation
  (defun line-to-string (&optional line)
    ""
    (save-excursion
      (if line (forward-line line))
      (buffer-substring-no-properties
       (line-beginning-position)
       (line-end-position))))

  (defun make-mode/indent ()
    ""
    (tabify (point-min) (point-max))
    (let* ((indent-column 2)
           (line (line-to-string))
           (prev-line (line-to-string -1))
           (label-line-regex (rx (optional ".") (1+ ascii) ":" (0+ ascii) eol))
           (indented-line-regex (rx (= 2 ;;(eval indent-column)
                                       space) (0+ ascii) eol))
           (multi-line-regex (rx (0+ ascii) ?\\ eol))
           (assignment-regex (rx bol (0+ ascii) ?= (0+ ascii) eol)))
      (cond
       ((s-matches-p multi-line-regex prev-line) (indent-to-left-margin))
       ((and (not (s-matches-p label-line-regex prev-line))
             (s-matches-p assignment-regex line))
        (indent-to-left-margin))
       ((s-matches-p label-line-regex line) (indent-to-left-margin))
       ((s-matches-p label-line-regex prev-line) (indent-line-to indent-column))
       ((s-matches-p indented-line-regex prev-line) (indent-line-to indent-column)))))

  (setq-mode-local makefile-mode indent-line-function #'make-mode/indent))

(use-package dired
  :defer t
  :commands (dired jens/dired-toggle-mark)
  :bind
  (("C-x C-d" . dired-jump)
   :map dired-mode-map
   ("c" . dired-do-copy)
   ("C-." . dired-omit-mode)
   ("SPC" . jens/dired-toggle-mark)
   ("C-+" . dired-create-empty-file))
  :config
  ;; pull in extra functionality for dired
  (load-library "dired-x")
  (load-library "dired-aux")

  (setq dired-listing-switches "-agholXN")
  (setq dired-create-destination-dirs 'always)
  (setq dired-hide-details-hide-symlink-targets nil)
  (setq dired-dwim-target t)

  ;; always delete and copy recursively
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies 'always)

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

  (advice-add #'dired-readin :after #'jens/dired-sort)

  ;; use bigger fringes in dired-mode
  (add-hook 'dired-mode-hook (lambda () (setq left-fringe-width 10)))

  (defun jens/dired-toggle-mark (arg)
    "Toggle mark on the current line."
    (interactive "P")
    (let ((this-line (buffer-substring (line-beginning-position) (line-end-position))))
      (if (s-matches-p (dired-marker-regexp) this-line)
          (dired-unmark arg)
        (dired-mark arg))))

  (defun jens/dired-show-readme ()
    "Popup readme in a temporary view-buffer."
    (interactive)
    (when-let* ((dir (dired-current-directory))
                (files (f-entries dir))
                (readme (-first
                         (lambda (e) (s-match "readme\..*$" e))
                         files)))
      (view-buffer-other-window
       (find-file-noselect readme)
       nil #'kill-buffer-if-not-modified)))
  )

(use-package elisp-mode
  :delight (emacs-lisp-mode "Elisp" :major))

(use-package ediff
  :defer t
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows)
  (setq ediff-split-window-function 'split-window-vertically))

(use-package diff)

(use-package eww
  :defer t
  :bind (("M-s M-o" . #'eww/open-url-at-point)
         ("M-s M-w" . #'eww/search-region)
         ("M-s M-d" . #'eww/download-url-at-point)
         ("M-s M-D" . #'eww/delete-cookies)
         ("M-s M-s" . #'eww-list-buffers)
         ("M-s M-c" . #'url-cookie-list))
  :config
  (setq eww-download-directory (expand-file-name "~/downloads"))

  (defun eww/search-region ()
    "Open eww and search for the contents of the region."
    (interactive)
    (when-let (((use-region-p))
               (query (buffer-substring (region-beginning) (region-end)))
               (url (concat eww-search-prefix query))
               (buf (get-buffer-create "*eww*")))
      (view-buffer-other-window buf)
      (eww url)))

  (defun eww/open-url-at-point ()
    "Open link at point in eww."
    (interactive)
    (when-let ((url (car (eww-suggested-uris)))
               (buf (get-buffer-create "*eww*")))
      (view-buffer-other-window buf)
      (eww url)))

  (defun eww/download-url-at-point ()
    "Download the url at point using eww."
    (interactive)
    (when-let ((url (thing-at-point 'url)))
      (url-retrieve url 'eww-download-callback (list url))))

  (defun eww/delete-cookies ()
    "Delete all eww cookies."
    (interactive)
    (message "deleting cookies...")
    (url-cookie-delete-cookies)))

;;;;; minor modes

(use-package hi-lock :diminish hi-lock-mode)
(use-package outline :diminish outline-minor-mode)

(use-package display-fill-column-indicator
  :hook ((text-mode prog-mode) . display-fill-column-indicator-mode)
  :custom-face
  (fill-column-indicator ((t (:foreground ,(zenburn-get "zenburn-bg+05") :background nil)))))

(use-package ispell
  :defer t
  :config
  (ispell-change-dictionary "english")
  (setq ispell-program-name (executable-find "aspell"))
  (setq ispell-grep-command "rg")
  (setq ispell-silently-savep t))

(use-package flyspell
  :defer t
  :commands (enable-spellchecking
             flyspell-mode
             flyspell-prog-mode
             flyspell-buffer)
  :bind
  (("C-M-," . flyspell-goto-next-error))
  :config
  (defun enable-spellchecking ()
    "Enable spellchecking in the current buffer."
    (interactive)
    (flyspell-prog-mode)
    (flyspell-buffer)))

(use-package whitespace
  :demand t
  :diminish
  :config
  (defun jens/show-trailing-whitespace ()
    "Show trailing whitespace in buffer."
    (interactive)
    (setq show-trailing-whitespace t)
    (whitespace-mode +1))

  (add-hook* '(text-mode-hook prog-mode-hook) #'jens/show-trailing-whitespace))

(use-package elec-pair ;; insert parens-type things in pairs
  :demand t
  :config
  (setq electric-pair-pairs
        '((?\( . ?\))
          (?\[ . ?\])
          (?\{ . ?\})
          (?\` . ?\')
          (?\" . ?\")))

  ;; break open parens when creating newline in between
  (setq electric-pair-open-newline-between-pairs t)

  (electric-pair-mode +1))

(use-package paren ;; highlight matching parens
  :demand t
  :config
  (setq show-paren-delay 0.1)
  (setq show-paren-style 'expression)
  (setq show-paren-when-point-inside-paren t)

  (show-paren-mode +1)
  :custom-face
  (show-paren-match-expression ((t (:foreground nil :background "#353535")))))

(use-package abbrev ;; auto-replace common abbreviations
  :demand t
  :diminish abbrev-mode
  :hook (org-mode . abbrev-mode)
  :commands read-abbrev-file
  :config
  (setq abbrev-file-name (no-littering-expand-etc-file-name "abbreviations.el"))
  (read-abbrev-file)
  (abbrev-mode +1))

(use-package subword ;; easily navigate silly cased words
  :demand t
  :diminish subword-mode
  :config
  (global-subword-mode 1))

(use-package saveplace ;; save point position between sessions
  :demand t
  :config
  (setq save-place-file (no-littering-expand-var-file-name "saveplaces"))
  (save-place-mode +1))

(use-package savehist ;; persist some variables between sessions
  :demand t
  :config
  (setq savehist-file (no-littering-expand-var-file-name "savehist"))
  ;; save every minute
  (setq savehist-autosave-interval 60)
  (setq savehist-additional-variables
        '(kill-ring
          search-ring
          regexp-search-ring))
  ;; just keep all history
  (setq history-length t)
  (setq history-delete-duplicates t)
  (savehist-mode 1))

(use-package autorevert
  ;; always show the version of a file as it appears on disk
  :disabled t
  :demand t
  :diminish auto-revert-mode
  :config
  ;; also auto refresh dired, but be quiet about it
  (setq global-auto-revert-non-file-buffers t)
  (setq auto-revert-verbose nil)

  ;; just revert pdf files without asking
  (setq revert-without-query '("\\.pdf"))

  ;; auto refresh buffers
  (global-auto-revert-mode -1))

(use-package display-line-numbers
  :defer t
  :commands display-line-numbers-mode
  :bind ("M-g M-g" . jens/goto-line-with-feedback)
  :config
  (defun jens/goto-line-with-feedback ()
    "Show line numbers temporarily, while prompting for the line
number input"
    (interactive)
    (unwind-protect
        (progn
          (display-line-numbers-mode 1)
          (call-interactively 'goto-line))
      (display-line-numbers-mode -1))))

(use-package smerge-mode ;; easily handle merge conflicts
  :bind
  (:map smerge-mode-map ("C-c ^" . jens/smerge/body))
  :config
  (defhydra jens/smerge ()
    "Move between buffers."
    ("n" #'smerge-next "next")
    ("p" #'smerge-prev "previous")
    ("u" #'smerge-keep-upper "keep upper")
    ("l" #'smerge-keep-lower "keep lower"))

  (defun jens/enable-smerge-if-diff-buffer ()
    "Enable Smerge-mode if the current buffer is showing a diff."
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^<<<<<<< " nil t)
        (smerge-mode 1))))

  (add-hook* '(find-file-hook after-revert-hook) #'jens/enable-smerge-if-diff-buffer))

(use-package eldoc
  ;; show useful contextual information in the echo-area
  :diminish
  :config
  (setq eldoc-idle-delay 0.2)

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

  (setq lispy-modes
        (-concat '(lisp-mode lisp-interaction-mode inferior-lisp-mode)
                 '(clojure-mode clojurec-mode clojurescript-mode inf-clojure-mode cider-repl-mode)
                 '(emacs-lisp-mode eshell-mode inferior-emacs-lisp-mode)
                 '(common-lisp-mode slime-repl-mode)
                 '(scheme-mode geiser-repl-mode inferior-scheme-mode scheme-interaction-mode)))

  (with-eval-after-load 'dokument
    (defun jens/lispify-eldoc-message (eldoc-msg)
      "Change the format of eldoc messages for functions to `(fn args)'."
      (shut-up
        (save-window-excursion
          (save-mark-and-excursion
            (if (and eldoc-msg
                     (member major-mode lispy-modes))
                (let* ((parts (s-split ": " eldoc-msg))
                       (sym-name (car parts))
                       (sym (intern sym-name))
                       (args (cadr parts))
                       (doc (and (fboundp sym) (documentation sym 'raw)))
                       (short-doc (when doc (substring doc 0 (string-match "\n" doc))))
                       (short-doc (when short-doc (dokument-elisp--fontify-as-doc short-doc))))
                  (cond
                   ((string= args "()") (format "(%s)" sym-name))
                   (t (format "(%s %s)\t\t%s" sym-name
                              (substring args 1 (- (length args) 1))
                              short-doc))))
              eldoc-msg)))))
    (advice-add #' elisp-get-fnsym-args-string :filter-return #'jens/lispify-eldoc-message))

  (global-eldoc-mode +1)
  :custom-face
  (eldoc-highlight-function-argument ((t (:inherit font-lock-warning-face))))
  (eldoc-highlight-&s-face ((t (:inherit font-lock-preprocessor-face)))))

(use-package fringe
  :commands fringe-mode
  :config
  (set-fringe-mode '(5 . 5))
  :custom-face
  (fringe ((t (:background "#3f3f3f")))))

;;;;; misc packages

(use-package package
  :config
  (defun jens/package-menu-show-updates (&rest args)
    "Show a list of the packages marked as upgrading."
    (let* ((packages (->>
                      (package-menu--find-upgrades)
                      (-map #'car)
                      (-map #'symbol-name)
                      (s-join "\n")))
           (this-buffer (current-buffer)))
      (unless (s-blank-p packages)
        (with-current-buffer (get-buffer-create "*Package Menu Updates*")
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert packages)
            (goto-char (point-min))
            (view-buffer (current-buffer))))
        (view-buffer-other-window (get-buffer "*Messages*"))
        (set-buffer this-buffer))))

  (advice-add #'package-menu-execute :before #'jens/package-menu-show-updates))

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

(use-package uniquify ;; give buffers unique names
  :demand t
  :config (setq uniquify-buffer-name-style 'forward))

(use-package tramp ;; easily access and edit files on remote machines
  :defer t
  :config
  (setq tramp-verbose 6)

  (setq tramp-persistency-file-name (no-littering-expand-var-file-name "tramp/"))
  (setq tramp-default-method "ssh")

  (defun tramp/get-method-parameter (method param)
    "Return the method parameter PARAM.
If the `tramp-methods' entry does not exist, return NIL."
    (let ((entry (assoc param (assoc method tramp-methods))))
      (when entry (cadr entry))))

  (defun tramp/set-method-parameter (method param newvalue)
    "Set the method paramter PARAM to VALUE for METHOD.

If METHOD does not yet have PARAM, add it.
If METHOD does not exist, do nothing."
    (let ((method-params (assoc method tramp-methods)))
      (when method-params
        (let ((entry (assoc param method-params)))
          (if entry
              (setcar (cdr entry) newvalue)
            (setcdr (last method-params) '(param newvalue))))))))

(use-package recentf ;; save a list of recently visited files.
  :demand t
  :commands (recentf-mode jens/recentf)
  :bind (("C-x f" . jens/recentf))
  :config
  ;; TODO: maybe move to var directory?
  (setq recentf-save-file
        (recentf-expand-file-name (no-littering-expand-etc-file-name "recentf.el")))
  (setq recentf-exclude
        `(,(regexp-quote
            (locate-user-emacs-file no-littering-var-directory))
          "COMMIT_EDITMSG"
          (f-join user-mail-directory "*")))

  ;; save a bunch of recent items
  (setq recentf-max-saved-items 1000)

  ;; clean the list every 5 minutes
  (setq recentf-auto-cleanup 300)

  ;; save recentf file every 30s, but don't bother us about it
  (setq recentf-auto-save-timer
        (run-with-idle-timer 30 t (lambda ()
                                    (shut-up (recentf-save-list)))))

  (defun path-colorize-tail (path face)
    (let ((last-part (-last-item (f-split path))))
      ;; TODO: replace in-string, dont use regexp, it matches other things as well
      (replace-regexp-in-string
       (regexp-quote last-part)
       (lambda (s) (propertize s 'face face))
       path)))

  (defun path-colorize-whole (path face)
    (propertize path 'face face))

  (defun path-colorize-file (file)
    (path-colorize-tail
     file
     (cond
      ((member (f-ext file) '("org" "md" "rst" "tex" "txt"))
       '(:foreground "#ffed4a"))
      ((member (f-ext file) '("css" "less" "sass" "scss" "htm" "html"))
       '(:foreground "#eb5286"))
      ((member (f-ext file) '("docm" "doc" "docx" "odb" "odt" "pdb" "pdf" "ps" "rtf" "djvu" "epub" "odp" "ppt" "pptx"))
       '(:foreground "#9561e2"))
      ((member (f-ext file) '("xml" "xsd" "xsl" "xslt" "wsdl" "bib" "json" "toml" "rss" "yaml" "yml"))
       '(:foreground "#f2d024"))
      ((member (f-ext file) '("7z" "zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar"))
       '(:foreground "#51d88a"))

      ;; TODO: f-ext does not work with dotfiles
      ((member (f-ext file) '("bashrc" "bash_profile" "bash_history" "zshrc" "zshenv" "zprofile" "zsh_history" "xinitrc" "xsession" "Xauthority" "Xclients" "profile" "inputrc" "Xresources"))
       '(:foreground "#6C6063"))

      ((member (f-ext file) '("asm" "cl" "lisp" "el" "c" "c++" "cpp" "cxx" "h" "h++" "hpp" "hxx" "m" "cc" "cs" "go" "rs" "hi" "hs" "pyc" "java"))
       '(:foreground "#F0DFAF"))
      ((member (f-ext file) '("sh" "tmux" "zsh" "py" "ipynb" "rb" "pl" "mysql" "pgsql" "sql" "cljr" "clj" "cljs" "scala" "js"))
       '(:foreground "#8FB28F"))
      ((member (f-ext file) '("git" "gitignore" "gitattributes" "gitmodules"))
       '(:foreground "#8CD0D3"))

      (t 'font-lock-builtin-face))))

  (defun path-colorize (path)
    ""
    (cond
     ((file-remote-p path)
      (let* ((remote-part (file-remote-p path))
             (path-part (substring path (length remote-part))))
        (s-concat
         (path-colorize-whole remote-part 'font-lock-string-face)
         (path-colorize-tail path-part 'font-lock-builtin-face))))
     ((f-dir-p path) (path-colorize-tail path 'dired-directory))
     ((f-file-p path) (path-colorize-file path))
     (t (path-colorize-whole path 'font-lock-warning-face))))

  (defun recentf/track-opened-file ()
    "Insert the name of the dired or file just opened or written into the recent list."
    (when-let ((path (or buffer-file-name (and (derived-mode-p 'dired-mode) default-directory))))
      (recentf-add-file path))
    ;; Must return nil because it is run from `write-file-functions'.
    nil)

  (add-hook 'dired-after-readin-hook #'recentf/track-opened-file)

  (defun jens/recentf ()
    "Show list of recently visited files, colorized by type."
    (interactive)
    (let* ((recent-files (mapcar #'substring-no-properties recentf-list))
           (colorized-files (-map #'path-colorize recent-files)))

      (ivy-read "recent files: "
                colorized-files
                :action (lambda (f)
                          (with-ivy-window
                            (find-file f)))
                :require-match t
                :caller 'jens/recentf)))

  (defun jens/recentf-cleanup (orig-fun &rest args)
    "Silence `recentf-auto-cleanup'."
    (shut-up (apply orig-fun args)))

  (advice-add #'recentf-cleanup :around #'jens/recentf-cleanup)
  (recentf-mode +1))

(use-package replace
  :defer t
  :bind
  (("C-c r" . jens/replace)
   ("C-c q" . jens/query-replace))
  :config
  (defun jens/--replace (fn)
    "Get replace arguments and delegate to replace FN."
    (let ((from (if (use-region-p)
                    (buffer-substring (region-beginning) (region-end))
                  (completing-read "Replace: " '())))
          (to (completing-read "Replace with: " '())))
      (deactivate-mark)
      (save-excursion
        (funcall fn from to nil (point-min) (point-max)))))

  (defun jens/replace ()
    "Replace occurrence of regexp in the entire buffer."
    (interactive)
    (jens/--replace #'replace-regexp))

  (defun jens/query-replace ()
    "Interactively replace occurrence of regexp in the entire buffer."
    (interactive)
    (jens/--replace #'query-replace-regexp)))

(use-package semantic
  ;; semantic analysis in supported modes (cpp, java, etc.)
  :disabled t
  :defer t
  :config
  ;; persist the semantic parse database
  (setq semanticdb-default-save-directory
        (no-littering-expand-var-file-name "semantic/"))

  (unless (file-exists-p semanticdb-default-save-directory)
    (make-directory semanticdb-default-save-directory))

  ;; save parsing results into a persistent database
  (global-semanticdb-minor-mode -1)
  ;; re-parse files on idle
  (global-semantic-idle-scheduler-mode -1)
  (semantic-mode -1))

(use-package compile
  :bind
  (("M-g n" . jens/next-error)
   ("M-g p" . jens/previous-error))
  :commands (jens/next-error jens/previous-error)
  :config
  ;; don't keep asking for the commands
  (setq compilation-read-command nil)
  ;; stop scrolling compilation buffer when encountering an error
  (setq compilation-scroll-output 'first-error)
  ;; wrap lines
  (add-hook 'compilation-mode-hook #'visual-line-mode)

  (require 'ansi-color)
  (defun jens/colorize-compilation-buffer ()
    (unless (derived-mode-p 'grep-mode)
      (ansi-color-apply-on-region compilation-filter-start (point))))
  (add-hook 'compilation-filter-hook #'modi/colorize-compilation-buffer)

  (defhydra jens/goto-error-hydra ()
    "Hydra for navigating between errors."
    ("n" #'next-error "next error")
    ("p" #'previous-error "previous error"))

  (defun jens/next-error ()
    "Go to next error in buffer, and start goto-error-hydra."
    (interactive)
    (next-error)
    (jens/goto-error-hydra/body))

  (defun jens/previous-error ()
    "Go to previous error in buffer, and start goto-error-hydra."
    (interactive)
    (previous-error)
    (jens/goto-error-hydra/body)))

(use-package browse-url
  :defer t
  :bind
  ("C-c C-o" . #'jens/open-url-at-point)
  :config
  (setq browse-url-firefox-program "firefox-developer-edition")

  (defun jens/open-url-at-point ()
    "Open the URL-at-point, dwim."
    (interactive)
    (let ((url))
      (cond
       ((thing-at-point 'url t) (browse-url-at-point))
       ((setq url (shr-url-at-point nil)) (browse-url url))
       ((org-extra-url-at-point) (org-open-at-point))))))

(use-package shr
  :defer t
  :config
  (setq shr-use-colors nil)
  (setq shr-width 100)
  (setq shr-cookie-policy nil))

(use-package sendmail
  :after notmuch
  :config
  (setq mail-envelope-from 'header)
  (setq sendmail-program "msmtp")

  ;; this belongs in `message.el', but using (use-package message) breaks #'message.
  (setq message-sendmail-envelope-from 'header)
  (setq message-sendmail-extra-arguments '("--read-envelope-from"))
  (setq message-sendmail-f-is-evil t)
  (setq message-fill-column fill-column)

  (setq mail-signature nil)

  (require 'async)
  (defun async-sendmail-send-it ()
    (let ((to (message-field-value "To"))
          (buf-content (buffer-substring-no-properties
                        (point-min) (point-max))))
      (message "Delivering message to %s..." to)
      (async-start
       `(lambda ()
          (require 'sendmail)
          (require 'message)
          (with-temp-buffer
            (insert ,buf-content)
            (set-buffer-multibyte nil)
            ,(async-inject-variables
              "\\`\\(sendmail\\|message-\\|\\(user-\\)?mail\\)-\\|auth-sources\\|epg\\|nsm"
              nil "\\`\\(mail-header-format-function\\|smtpmail-address-buffer\\|mail-mode-abbrev-table\\)")
            (message-send-mail-with-sendmail)))
       (lambda (&optional _ignore)
         (message "Delivering message to %s...done" to)))))

  (setq message-send-mail-function 'async-sendmail-send-it))

;;;;; org-mode

(log-info "Setting up org-mode")

(progn ;; the straight.el org-mode hack
  (require 'subr-x)
  (straight-use-package 'git)

  (defun org-git-version ()
    "The Git version of org-mode.
  Inserted by installing org-mode or when a release is made."
    (require 'git)
    (let ((git-repo (expand-file-name
                     "straight/repos/org/" user-emacs-directory)))
      (string-trim
       (git-run "describe"
                "--match=release\*"
                "--abbrev=6"
                "HEAD"))))

  (defun org-release ()
    "The release version of org-mode.
  Inserted by installing org-mode or when a release is made."
    (require 'git)
    (let ((git-repo (expand-file-name
                     "straight/repos/org/" user-emacs-directory)))
      (string-trim
       (string-remove-prefix
        "release_"
        (git-run "describe"
                 "--match=release\*"
                 "--abbrev=0"
                 "HEAD")))))

  (provide 'org-version)

  (comment
   (straight-use-package
    '(org-plus-contrib
      :repo "https://code.orgmode.org/bzg/org-mode.git"
      :local-repo "org"
      :files (:defaults "contrib/lisp/*.el")
      :includes (org))))

  (straight-use-package 'org-plus-contrib))

;; I want to use the version of org-mode from upstream.
;; remove the built-in org-mode from the load path, so it does not get loaded
(setq load-path (-remove (lambda (x) (string-match-p "org$" x)) load-path))
;; remove org-mode from the built-ins list, because we're using upstream
(setq package--builtins (assq-delete-all 'org package--builtins))

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
   ("M-<next>" . org-move-subtree-down)
   ("M-<prior>" . org-move-subtree-up))
  :config
  ;;;;;;;;;;;;;;;;;;;;;;
  ;; general settings ;;
  ;;;;;;;;;;;;;;;;;;;;;;

  (setq org-catch-invisible-edits 'show-and-error)
  (setq org-log-done 'time)

  ;; fontify src blocks
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)

  ;; keep #+BEGIN_SRC blocks aligned with their contents
  (setq org-edit-src-content-indentation 0)

  (setq org-use-speed-commands t)

  ;; don't indent things
  (setq org-adapt-indentation nil)

  (setq org-archive-save-context-info '(time file olpath itags))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)
     (latex . t)
     (ditaa . t)
     (plantuml . t)
     (dot . t)
     (python . t)
     (gnuplot . t)))

  (let* ((ditaa-dir "/usr/share/java/ditaa/")
         (ditaa-jar
          (-as-> (f-entries ditaa-dir) es
                 (-filter (lambda (e)
                            (and
                             (s-starts-with-p "ditaa" (f-filename e))
                             (string= "jar" (f-ext e))))
                          es)
                 (car es))))
    (setq org-ditaa-jar-path ditaa-jar))

  ;; try to get non-fuzzy latex fragments
  (plist-put org-format-latex-options :scale 1.6)
  (setq org-preview-latex-default-process 'dvisvgm)

  (setq org-html-head
        (s-join " "
                '("<style type=\"text/css\">"
                  "body {max-width: 800px; margin: 0 auto;}"
                  "img {max-width: 100%;}"
                  "</style>")))

  (setq org-outline-path-complete-in-steps t)
  (setq org-refile-allow-creating-parent-nodes (quote confirm))
  (setq org-refile-use-outline-path t)
  (setq org-refile-targets '( (nil . (:maxlevel . 1))))

  ;;;;;;;;;;;;;;;
  ;; exporting ;;
  ;;;;;;;;;;;;;;;

  (require 'ox-latex)
  (add-to-list 'org-latex-packages-alist '("" "minted"))
  (setq org-latex-listings 'minted)

  (setq org-latex-pdf-process
        '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

  ;;;;;;;;;;;;
  ;; advice ;;
  ;;;;;;;;;;;;

  (defun jens/org-summary-todo (n-done n-not-done)
    "Switch entry to DONE when all sub-entries are done, to TODO otherwise."
    (let (org-log-done org-log-states)   ; turn off logging
      (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

  (add-hook 'org-after-todo-statistics-hook #'jens/org-summary-todo)

  ;; `$ $' is a pair for latex-math in org-mode
  (setq org-extra-electric-pairs '((?\$ . ?\$)))

  (defun jens/org-add-electric-pairs ()
    (setq-local electric-pair-pairs (-concat org-extra-electric-pairs electric-pair-pairs)))

  (add-hook 'org-mode-hook #'jens/org-add-electric-pairs)

  ;;;;;;;;;;;;;;;;;;;;;;
  ;; helper functions ;;
  ;;;;;;;;;;;;;;;;;;;;;;

  (defun jens/toggle-org-babel-safe ()
    "Toggle whether it is safe to eval babel code blocks in the current buffer."
    (interactive)
    (set (make-variable-buffer-local 'org-confirm-babel-evaluate)
         (not org-confirm-babel-evaluate)))

  (defun jens/org-indent ()
    "Indent line or region in org-mode."
    (interactive)
    (if (region-active-p)
        (org-indent-region (region-beginning) (region-end)))
    (org-indent-line)
    (message "indented"))

  ;; (defun jens/load-org-agenda-files ()
  ;;   (interactive)
  ;;   (setq org-agenda-files
  ;;         (append '("")
  ;;                 (f-glob "**/*.org" "~/vault/org/planning"))))

  ;; (advice-add 'org-agenda :before #'jens/load-org-agenda-files)

  ;; syntax highlight org-mode code blocks when exporting as pdf
  ;; (setq-default org-latex-listings 'minted
  ;;               org-latex-packages-alist '(("" "minted"))
  ;;               org-latex-pdf-process
  ;;               '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
  ;;                 "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
(use-package reftex
  :straight t
  :after org-mode)

(use-package bibtex
  :straight t
  :after org-mode)

(use-package org-ref
  :straight t
  :after org-mode)

(use-package org-contacts
  :after notmuch-mojn
  :init
  (setq org-contacts-enable-completion nil)
  (setq org-contacts-icon-use-gravatar nil)
  :config
  (setq org-contacts-files user-contacts-files)

  (defun jens/org-contacts ()
    "Return all contacts from `org-contacts', in 'NAME <EMAIL>' format."
    (let ((data (-map #'caddr (org-contacts-db))))
      (-map
       (lambda (d)
         (let ((email (cdr (assoc-string org-contacts-email-property d)))
               (name (cdr (assoc-string "ITEM" d))))
           (format "%s <%s>" (or name "") (or email ""))))
       data)))

  (add-to-list 'notmuch-mojn-candidate-functions #'jens/org-contacts))

(use-package org-agenda
  :defer t
  ;; TODO: have a look at https://github.com/alphapapa/org-super-agenda
  )

;;; homemade
;;;; defuns

(log-info "Defining homemade defuns")

;;;;; convenience

(defun jens/clean-view ()
  "Create a scratch buffer, and make it the only buffer visible."
  (interactive)
  (b-jump-to-empty-scratch-buffer)
  (delete-other-windows))

(defun jens/clone-buffer ()
  "Open a clone of the current buffer."
  (interactive)
  (let ((newbuf (b-new-scratch-buffer))
        (content (buffer-string))
        (p (point)))
    (with-current-buffer newbuf
      (insert content))
    (switch-to-buffer newbuf)
    (goto-char p)))

(defun jens/sudo-find-file (filename)
  "Open FILENAME with superuser permissions."
  (let ((remote-method (file-remote-p default-directory 'method))
        (remote-user (file-remote-p default-directory 'user))
        (remote-host (file-remote-p default-directory 'host))
        (remote-localname (file-remote-p filename 'localname)))
    (find-file (format "/%s:root@%s:%s"
                       (or remote-method "sudoedit")
                       (or remote-host "localhost")
                       (or remote-localname filename)))))

(defun jens/sudo-edit (&optional arg)
  "Re-open current buffer file with superuser permissions.
With prefix ARG, ask for file to open."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (read-file-name "Find file:"))

  (let ((place (point))
        (mode major-mode))
    (jens/sudo-find-file buffer-file-name)
    (goto-char place)
    (funcall mode)))

(defun jens/inspect-variable-at-point (&optional arg)
  "Inspect variable at point."
  (interactive "P")
  (require 'dokument)
  (let* ((sym (symbol-at-point))
         (value (cond
                 ((fboundp sym) (symbol-function sym))
                 ((boundp sym) (symbol-value sym)))))
    (if arg
        (with-current-buffer (get-buffer-create "*Inspect*")
          (let ((inhibit-read-only t))
            (erase-buffer)
            (pp value (current-buffer))
            (emacs-lisp-mode)
            (goto-char 0))
          (view-buffer-other-window (current-buffer)))

      ;; TODO: don't use `dap' posframe, create a posframe for all-purpose emacs things

      (funcall dokument-display-fn
               (dokument-elisp--fontify-as-code
                (with-output-to-string
                  (pp value)))))))

;;;;; files

(defun jens/byte-compile-this-file ()
  "Byte compile the current buffer."
  (interactive)
  (if (f-exists? (concat (f-no-ext (buffer-file-name)) ".elc"))
      (byte-recompile-file (buffer-file-name))
    (byte-compile-file (buffer-file-name))))

(defun jens/get-buffer-file-directory ()
  "Get the directory of the file belonging to the current
buffer."
  (file-name-directory (buffer-file-name)))

(defun jens/file-age (file)
  "Return the number of seconds since FILE was last modified."
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

(defun jens/reopen-this-file ()
  "Reopen the current file."
  (interactive)
  (let ((file (buffer-file-name)))
    (cond
     ((not (f-exists-p file)) (message "file does not exist on disk, cannot re-open."))
     ((buffer-modified-p (current-buffer)) (message "buffer has not been saved, aborting re-open."))
     (t
      (kill-buffer-if-not-modified (current-buffer))
      (find-file file)))))

;;;;; lisp

(defun jens/one-shot-keybinding (key command)
  "Set a keybinding that disappear once you press a key that is
not in the overlay-map"
  (set-transient-map
   (let ((map (make-sparse-keymap)))
     (define-key map (kbd key) command)
     map) t))
;; example
;; (jens/one-shot-keybinding "a" (xi (forward-line -1)))

(defun jens/one-shot-keymap (key-command-pairs)
  "Set a keybinding that disappear once you press a key that is
not in the overlay-map"
  (set-transient-map
   (let ((map (make-sparse-keymap)))
     (dolist (kvp key-command-pairs)
       (let ((key (car kvp))
             (cmd (cdr kvp)))
         (define-key map (kbd key) cmd)))
     map) t))

;; example:
;; (jens/one-shot-keymap
;;  `(("a" . ,(xi (message "a")))
;;    ("b" . ,(xi (message "b")))
;;    ("c" . ,(xi (message "c")))
;;    ("d" . ,(xi (message "d")))))

(defun jens/try-require (feature)
  "Try to require FEATURE, if an exception is thrown, log it."
  (condition-case ex
      (progn
        (log-info (format "= Requiring \"%s\" " (symbol-name feature)))
        (require feature))
    ('error (log-warning (format "@ Error requiring \"%s\": %s" (symbol-name feature) ex)))))

(defun jens/eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defun jens/remove-text-properties-region (beg end)
  "Remove text properties from text in region between BEG and END."
  (set-text-properties beg end nil))

(defun jens/remove-text-propertiex-in-region ()
  "Remove text properties from all text in active region."
  (interactive)
  (when (region-active-p)
    (jens/remove-text-properties-region
     (region-beginning) (region-end))))

(defun jens/space-to-dash-in-region ()
  "Replace all spaces in region with dashes."
  (interactive)
  (when (region-active-p)
    (let* ((p (point))
           (str (buffer-substring (region-beginning) (region-end)))
           (dashed (s-replace " " "-" str)))
      (delete-region (region-beginning) (region-end))
      (insert dashed)
      (goto-char p))))

(defun jens/foreach-line-in-region (fn &optional beg end)
  "Call FN on each line in region (BEG END)."
  (let ((beg (or beg (region-beginning)))
        (end (or end (region-end))))
    (goto-char beg)
    (beginning-of-line)
    (while (< (point) end)
      (funcall fn)
      (forward-line))))

(defun jens/save-to-file (data filename)
  "Save lisp object DATA to FILENAME."
  (with-temp-file filename
    (prin1 data (current-buffer))))

(defun jens/load-from-file (filename)
  "Load lisp object from FILENAME."
  (when (file-exists-p filename)
    (with-temp-buffer
      (insert-file-contents filename)
      (cl-assert (eq (point) (point-min)))
      (read (current-buffer)))))

(defun jens/sexp-up ()
  (interactive)
  (condition-case nil
      (forward-sexp -1)
    (error (backward-char))))

(defun jens/sexp-down ()
  (interactive)
  (condition-case nil
      (forward-sexp 1)
    (error (forward-char))))

(defun jens/open-line-below ()
  "Insert a line below the current line, indent it, and move to
the beginning of that line."
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))

(defun jens/open-line-above ()
  "Insert a line above the current line, indent it, and move to
the beginning of that line."
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
  "If a region is active then it is killed, otherwise the current
line is killed."
  (interactive "P")
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (save-excursion
      (kill-whole-line arg))))

(defun jens/clean-current-line ()
  "Delete the contents of the current line."
  (interactive)
  (delete-region (line-beginning-position) (line-end-position)))

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
line.  Otherwise join the line below the current line, with the
current line, placing it after."
  (interactive)
  (if (region-active-p)
      (jens/join-region)
    (join-line -1)))

(defun jens/join-line-down ()
  "Pull the line above down to the end of this line."
  (interactive)
  (save-excursion
    (let ((cp (point)))
      (forward-line -1)
      (when (not (= (point) cp))
        (call-interactively #'jens/kill-region-or-current-line)
        (end-of-line)
        (save-excursion (insert " " (s-chomp (current-kill 0))))
        (just-one-space)))))

(defun jens/wrap-region (b e text-begin text-end)
  "Wrap region from B to E with TEXT-BEGIN and TEXT-END."
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
  "If region is active, comment or uncomment it (based on what it
currently is), otherwise comment or uncomment the current line."
  (interactive)
  (if (region-active-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (line-beginning-position) (line-end-position))))

(defmacro easy-eldoc (hook fn)
  "Add FN to `eldoc-documentation-function' in HOOK."
  (let ((name (intern (format "easy-eldoc-%s-%s" (symbol-name hook) (symbol-name fn)))))
    `(progn
       (defun ,name ()
         (set (make-local-variable 'eldoc-documentation-function) #',fn))
       (add-hook ',hook #',name))))

;;;;; misc

(defun jens/get-package-reqs (package)
  (when-let* ((desc (assq 'cider package-alist))
              (desc (cadr desc)))
    (package-desc-reqs desc)))

(defun jens/required-by (package)
  (let ((descs (-map #'cdr package-alist))
        (res '()))
    (dolist (d descs res)
      (let ((pkg-name (package-desc-name (car d)))
            (reqs (-map #'car (package-desc-reqs (car d)))))
        (when (-contains-p reqs package)
          (push pkg-name res))))))

(defun jens/insert-todays-date ()
  "Insert the current date at point."
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

(defun jens/insert-todays-date-compact ()
  "Insert the current date at point, in a compact format."
  (interactive)
  (insert (format-time-string "%Y%m%d")))

(defun jens/goto-msg-buffer ()
  "View the *Messages* buffer, return to previous buffer when
done."
  (interactive)
  (with-current-buffer (get-buffer "*Messages*")
    (goto-char (point-max))
    (view-buffer (current-buffer))))

(defun jens/goto-next-line-with-same-indentation ()
  "Jump to the next line with the same indentation level as the
current line."
  (interactive)
  (back-to-indentation)
  (re-search-forward
   (s-concat "^" (s-repeat (current-column) " ") "[^ \t\r\n\v\f]")
   nil nil (if (= 0 (current-column)) 2 1))
  (back-to-indentation))

(defun jens/goto-prev-line-with-same-indentation ()
  "Jump to a previous line with the same indentation level as the
current line."
  (interactive)
  (back-to-indentation)
  (re-search-backward
   (s-concat "^" (s-repeat (current-column) " ") "[^ \t\r\n\v\f]"))
  (back-to-indentation))

(defun jens/function-def-string (fnsym)
  "Return function definition of FNSYM as a string."
  (let* ((buffer-point (condition-case nil (find-definition-noselect fnsym nil) (error nil)))
         (new-buf (car buffer-point))
         (new-point (cdr buffer-point)))
    (cond (buffer-point
           ;; try to get original definition
           (with-current-buffer new-buf
             (save-excursion
               (goto-char new-point)
               (buffer-substring-no-properties (point) (save-excursion (end-of-defun) (point))))))
          ;; fallback: just print the functions definition
          (t (concat (prin1-to-string (symbol-function fnsym)) "\n")))))

(defun jens/function-def-org-code-block (fnsym)
  "Return function definition of FNSYM as an org code-block."
  (concat "#+begin_src emacs-lisp\n"
          (jens/function-def-string fnsym)
          "#+end_src"))

(defun jens/copy-sexp-at-point ()
  "Save the `symbol-at-point' to the `kill-ring'."
  (interactive)
  (let ((sexp (thing-at-point 'sexp t)))
    (kill-new sexp)))

(defun jens/kill-sexp-at-point ()
  "Kill the sexp at point."
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'sexp)))
    (delete-region (car bounds) (cdr bounds))))

(defun jens/copy-buffer-file-path ()
  "Copy the current buffers file path to the clipboard."
  (interactive)
  (let ((path (buffer-file-name)))
    (with-temp-buffer
      (insert path)
      (clipboard-kill-ring-save (point-min) (point-max)))
    path))

(defun jens/goto-repo ()
  "Quickly jump to a repository, defined in repos.el"
  (interactive)
  (let* ((repos (jens/load-from-file (locate-user-emacs-file "repos.el")))
         (repos-propped
          (-map (lambda (r)
                  (let ((last-part (-last-item (f-split (car r)))))
                    (replace-regexp-in-string
                     last-part
                     (lambda (s) (propertize s 'face 'font-lock-keyword-face))
                     (car r))))
                repos))
         (pick (completing-read "Repo: " repos-propped nil t)))
    (cond
     ((f-directory? pick) (dired pick))
     ((f-file? pick) (find-file pick))
     (t (message "unable to locate repo: %s" pick)))))

(defun jens/load-secrets ()
  "Load secrets from `user-secrets-file'`"
  (interactive)
  (shut-up
    (with-temp-buffer
      (insert-file-contents user-secrets-file)
      (eval-buffer))))

(defun jens/get-secret (secret)
  "Get a secret from `user-secrets-file'`."
  (jens/load-secrets)
  (when (boundp secret)
    (symbol-value secret)))

(defun jens/--headings-org-level-1 ()
  "Return list of level 1 heading in an org-buffer."
  (require 'org-ql)
  (let* ((level-1-entries (org-ql (buffer-file-name) (level 1)))
         (cleaned-entries (-map (lambda (e) (cadr e)) level-1-entries))
         (headings (-map (lambda (h)
                           (cons (plist-get h ':raw-value)
                                 (plist-get h ':begin)))
                         cleaned-entries)))
    headings))

(defun jens/--headings-comment-boxes ()
  "Return list of comment-boxes in the current file."
  ;; TODO: use comment-box syntax to collect comment boxes from different modes,
  ;; not just emacs-lisp syntax
  (let* ((semi-line '((1+ ";") "\n"))
         (desc-line '(";;" (1+ (or alnum whitespace)) ";;" "\n"))
         (box (eval `(rx (and line-start ,@desc-line))))
         (content (buffer-substring-no-properties (point-min) (point-max)))
         (boxes (s-match-strings-all box content))
         (boxes (-map #'car boxes))
         (boxes (-map #'s-trim boxes))
         (positions (s-matched-positions-all box content))
         (positions (-map #'cdr positions))
         (headings (-zip boxes positions)))
    headings))

(defun jens/headings ()
  "Jump to a heading in the current file."
  (interactive)
  (let* ((headings (-remove #'null
                            (-flatten
                             (-cons*
                              (jens/--headings-comment-boxes)
                              (when (derived-mode-p 'org-mode)
                                (jens/--headings-org-level-1))))))
         (_ (sort headings (lambda (a b) (< (cdr a) (cdr b)))))
         (ivy-sort-functions-alist nil)
         (pick (completing-read "jump to heading: " headings nil t)))
    (goto-char (cdr (assoc pick headings)))))

(defhydra jens/shortcut (:exit t)
  ("m" #'jens/goto-msg-buffer "*messages*")
  ("n" #'notmuch-mojn "notmuch")
  ("e" #'elfeed "elfeed")
  ("c" #'b-jump-to-empty-scratch-buffer "new *scratch*"))

(defun jens/buffer-carousel-previous ()
  (interactive)
  (previous-buffer)
  (jens/buffer-carousel-hydra/body))

(defun jens/buffer-carousel-next ()
  (interactive)
  (next-buffer)
  (jens/buffer-carousel-hydra/body))

(defhydra jens/buffer-carousel-hydra ()
  "Move between buffers."
  ("<left>" #'previous-buffer "previous")
  ("<right>" #'next-buffer "next"))

;; TODO: make tail-buffer work for the buffer its called in, not just *Messages*
(defun jens/tail-message-buffer ()
  "Toggle tailing the *Message* buffer every time something is written to it."
  (interactive)
  (unless (fboundp 'tmb/message-buffer-goto-end)
    (defun tmb/message-buffer-goto-end (res)
      (dolist (w (get-buffer-window-list "*Messages*"))
        (with-selected-window w
          (set-window-point w (point-max))))
      res))

  (unless (boundp 'tail-message-buffer-mode)
    (define-minor-mode tail-message-buffer-mode
      "Tail the *Messages* buffer every time something calls `message'."
      nil " tail" (make-keymap)
      (if (bound-and-true-p tail-message-buffer-mode)
          (advice-add #'message :filter-args #'tmb/message-buffer-goto-end)
        (advice-remove #'message #'tmb/message-buffer-goto-end))))

  (with-current-buffer (get-buffer "*Messages*")
    (if (not (bound-and-true-p tail-message-buffer-mode))
        (tail-message-buffer-mode +1)
      (tail-message-buffer-mode -1))))

;; auto-tail the *Messages* buffer by default
(jens/tail-message-buffer)

(defun jens/toggle-focus ()
  "Toggle left and right window margins, centering `fill-column' lines."
  (interactive)
  (let ((margins (window-margins))
        (width (or 100 fill-column)))
    (if (and (null (car margins)) (null (cdr margins)))
        (let* ((win-width (window-width (selected-window)))
               (margin (/ (- win-width width) 2)))
          (set-window-margins (selected-window) margin margin))
      (set-window-margins (selected-window) 0 0))))

(defun jens/emacs-init-loc ()
  "Total lines of emacs-lisp code in my emacs configuration.

Requires the system tools `tokei' and `jq'."
  (interactive)
  (let* ((locs '("init.el"
                 "early-init.el"
                 "experimental.el"
                 "repos.el"
                 "lisp/*.el"
                 "modes/*.el"
                 "straight/repos/dokument.el/*.el"
                 "straight/repos/notmuch-mojn.el/*.el"
                 "straight/repos/etmux.el/*.el"
                 "straight/repos/lowkey-mode-line.el/*.el"
                 "straight/repos/today.el/*.el"
                 "straight/repos/views.el/*.el"
                 "straight/repos/sane-windows.el/*.el"
                 "straight/repos/replace-at-point.el/*.el"))
         (full-paths (-map (lambda (l) (f-full (concat user-emacs-directory l))) locs))
         (all-files (-flatten (-map (lambda (p) (f-glob p)) full-paths)))
         (all-files (s-join " " all-files))
         (cloc-cmd "tokei -o json")
         (format-cmd "jq '.inner.Elisp.code'")
         (final-cmd (format "%s %s | %s" cloc-cmd all-files format-cmd))
         (lines-of-code (s-trim (shell-command-to-string final-cmd))))
    lines-of-code))

(defun jens/cloc-this-file ()
  "Count the number of code lines in the current file."
  (interactive)
  (if-let ((file (buffer-file-name)))
      (message (s-trim (shell-command-to-string (format "tokei %s" file))))
    (message "This buffer does not have a file.")))

(defun jens/download-file (url &optional newname dir overwrite)
  "Download a file from URL to DIR, optionally OVERWRITE an existing file.
If DIR is nil, download to current directory."
  (let* ((dir (or dir default-directory))
         (file (or newname (url-unhex-string (f-filename url))))
         (path (f-join dir file)))
    (condition-case _ex
        (progn
          (url-copy-file url path overwrite)
          path)
      (error 'file-already-exists))))

(defun uuid ()
  "Generate a random UUID.

With `prefix-arg', insert the UUID at point in the current buffer."
  (interactive)
  (let ((id (s-trim (shell-command-to-string "uuidgen --random"))))
    (when current-prefix-arg
      (insert id))
    (message "%s" id)))

(defun jens/list-active-minor-modes ()
  "List all active minor modes."
  (interactive)
  (let ((modes (--filter (and (boundp it) (symbol-value it)) minor-mode-list)))
    (ivy-read "Active Minor Modes: " modes
              :action (lambda (m) (dokument-other-buffer m)))))

(defun jens/diff (a b)
  "Diff A with B, either should be a location of a document, local or remote."
  (interactive)
  (let* ((a-name (f-filename a))
         (b-name (f-filename b))
         (file_a (if (f-exists-p a) a
                   (jens/download-file a (concat (uuid) "-" a-name) temporary-file-directory)))
         (file_b (if (f-exists-p b) b
                   (jens/download-file b (concat (uuid) "-" b-name) temporary-file-directory))))
    (diff file_a file_b)))

(defun jens/diff-pick ()
  "Pick two files and diff them."
  (interactive)
  (let ((a (read-file-name "File A: " (f-dirname (buffer-file-name))))
        (b (read-file-name "File B: " (f-dirname (buffer-file-name)))))
    (jens/diff a b)))

(defun jens/diff-emacs-news ()
  "Check what's new in NEWS."
  (interactive)
  (let* ((news-files (f-glob "NEWS*" "/home/jens/emacs/src/etc/"))
         (news-file (completing-read "news file: " news-files nil t)))
    (jens/diff news-file
               "https://git.savannah.gnu.org/cgit/emacs.git/plain/etc/NEWS")))


;;;; packages

(use-package epdh
  :straight (emacs-package-dev-handbook
             :host github :repo "alphapapa/emacs-package-dev-handbook"
             :fork (:host github :repo "jensecj/emacs-package-dev-handbook")))

(log-info "Loading homemade packages")

(use-package b ;; buffer extentions
  :demand t)

(use-package org-extra
  :after org
  :demand t
  :bind ("M-o" . org-extra/commands/body)
  :config
  (defhydra org-extra/commands (:foreign-keys run)
    ("r" #'org-extra-rate "rate")
    ("h" #'org-extra-refile-here "refile here")
    ("o" #'org-extra-refile-to-open-org-files "refile to open")
    ("u" #'org-extra-copy-url-at-point "copy url")
    ("q" nil "quit")))

(use-package dev-extra :demand t)

(use-package notmuch-mojn
  :straight (notmuch-mojn :type git :repo "git@github.com:jensecj/notmuch-mojn.el.git")
  :defer t
  :bind*
  ;; TODO: this is just silly, i could use apply*, but this should be a part of `use-package'
  ((:map notmuch-show-mode-map
         ("u" . notmuch-mojn-refresh)
         ("g" . notmuch-refresh-this-buffer)
         ("G" . notmuch-mojn-fetch-mail))
   (:map notmuch-search-mode-map
         ("u" . notmuch-mojn-refresh)
         ("g" . notmuch-refresh-this-buffer)
         ("G" . notmuch-mojn-fetch-mail))
   (:map notmuch-tree-mode-map
         ("u" . notmuch-mojn-refresh)
         ("g" . notmuch-refresh-this-buffer)
         ("G" . notmuch-mojn-fetch-mail)))
  :commands notmuch-mojn
  :config
  (jens/load-secrets)
  (require 'notmuch nil 'noerror)

  ;; TODO: action for adding contact to org-contacts?
  ;; TODO: action for viewing a mail in org-mode

  (setq notmuch-mojn-really-delete-mail t)

  (remove-hook 'notmuch-mojn-post-refresh-hook #'notmuch-mojn-mute-retag-messages)

  (defun notmuch-mojn/cache-mail-key ()
    (unless (epa/gpg-key-in-cache "pass")
      (epa/gpg-cache-key user-gpg-mail-key)))

  (add-hook 'notmuch-mojn-pre-fetch-hook #'notmuch-mojn/cache-mail-key)

  (advice-add #'notmuch-address-expand-name
              :override
              #'notmuch-mojn-complete-address))

(use-package straight-ui
  :defer t
  :commands straight-ui
  :straight (straight-ui :type git :repo "git@github.com:jensecj/straight-ui.el.git"))

(use-package blog
  :load-path "~/vault/blog/src/"
  :defer t
  :commands (blog-publish
             blog-find-posts-file))

(use-package struere
  :defer t
  :bind (("C-c n" . struere-buffer))
  :config
  (struere-add 'org-mode #'jens/org-indent)
  (struere-add 'python-mode #'blacken-buffer))

(use-package augment
  :straight (augment :type git :repo "git@github.com:jensecj/augment.el.git")
  :defer t)

(use-package views
  :straight (views :type git :repo "git@github.com:jensecj/views.el.git")
  :defer t
  :bind
  (("M-v" . views-hydra/body))
  :config
  (defhydra views-hydra (:exit t)
    ""
    ("s" #'views-switch "switch")
    ("p" #'views-push "push")
    ("k" #'views-pop "pop")))

(use-package sane-windows
  :straight (sane-windows :type git :repo "git@github.com:jensecj/sane-windows.el.git")
  :defer t
  :bind* (("C-x 0" . nil)
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
  :defer t
  :bind ("M-f" . fullscreen-toggle))

(use-package etmux
  :straight (etmux :repo "git@github.com:jensecj/etmux.el.git")
  :defer t
  :commands (etmux-jackin etmux-spawn-here)
  :bind (("C-S-Z" . etmux-spawn-here)))

(use-package highlight-bookmarks
  :demand t
  :commands highlight-bookmarks-in-this-buffer
  :config
  (add-hook* '(find-file-hook after-save-hook) #'highlight-bookmarks-in-this-buffer)
  (advice-add* '(bookmark-jump bookmark-set bookmark-delete)
               :after
               #'highlight-bookmarks-in-this-buffer))

(use-package today
  :straight (today :type git :repo "git@github.com:jensecj/today.el.git")
  :defer t
  :commands (today-hydra/body)
  :bind
  (("C-x t" . today-hydra/body))
  :config
  (setq today-directory "~/vault/git/org/archive/")
  (setq today-file "~/vault/git/org/today.org")
  (setq today-inbox-file "~/vault/git/org/inbox.org")

  (defhydra today-refile-pl-hydra (:foreign-keys run)
    "
^Bindings^        ^ ^
^^^^^^^^-------------------
_r_: Rust        _g_: Git
_c_: C/C++       _l_: Lisp
_C_: Clojure     _m_: ML
_p_: Python      ^ ^
_j_: Java        ^ ^
"
    ("c" (today-refile "today.org" "C/C++"))
    ("C" (today-refile "today.org" "Clojure"))
    ("g" (today-refile "today.org" "Git"))
    ("j" (today-refile "today.org" "Java"))
    ("l" (today-refile "today.org" "Lisp"))
    ("m" (today-refile "today.org" "ML"))
    ("p" (today-refile "today.org" "Python"))
    ("r" (today-refile "today.org" "Rust"))

    ("x" today-refile-hydra/body "Refile entries" :exit t)
    ("z" org-refile-goto-last-stored "Jump to last refile")
    ("q" nil "quit"))

  (defhydra today-refile-hydra (:foreign-keys run)
    "
^Bindings^         ^ ^                     ^ ^                         ^ ^                   ^ ^
^^^^^^^^-------------------------------------------------------------------------------------------------
 _a_: AI            _d_: DevOps            _n_: Next                    _S_: Statistics     _x_: PL-Hydra
 _A_: Algorithms    _e_: Emacs             _o_: Other Talks             _t_: Tech Talks
 _b_: Business      _l_: Linux             _p_: Programming             _T_: TED Talks
 _c_: Climate       _m_: Machine Learning  _P_: Programming Languages   _w_: Work
 _C_: Courses       _M_: Math              _s_: Computer Science        _W_: Web
"
    ("a" (today-refile "today.org" "AI"))
    ("A" (today-refile "today.org" "Algorithms"))
    ("b" (today-refile "today.org" "Business"))
    ("c" (today-refile "today.org" "Climate"))
    ("C" (today-refile "today.org" "Courses"))
    ("d" (today-refile "today.org" "DevOps"))
    ("e" (today-refile "today.org" "Emacs"))
    ("l" (today-refile "today.org" "Linux"))
    ("m" (today-refile "today.org" "Machine Learning"))
    ("M" (today-refile "today.org" "Math"))
    ("n" (today-refile "today.org" "Next"))
    ("o" (today-refile "today.org" "Other Talks"))
    ("p" (today-refile "today.org" "Programming"))
    ("P" (today-refile "today.org" "Programming Languages"))
    ("s" (today-refile "today.org" "Computer Science"))
    ("S" (today-refile "today.org" "Statistics"))
    ("t" (today-refile "today.org" "Tech Talks"))
    ("T" (today-refile "today.org" "TED Talks"))
    ("w" (today-refile "today.org" "Work"))
    ("W" (today-refile "today.org" "Web"))

    ("x" today-refile-pl-hydra/body "Refile programming languages" :exit t)
    ("z" org-refile-goto-last-stored "Jump to last refile")
    ("q" nil "quit"))

  (defhydra today-capture-hydra (:foreign-keys run)
    "
^Capture^
^^^^^^^^------------------------------
_r_: capture read task
_R_: capture read task from clipboard
_w_: capture watch task
_W_: capture watch task from clipboard
_H_: capture task from clipboard to this buffer
"
    ("r" (lambda () (interactive) (today-capture-link-with-task 'read)))
    ("R" (lambda () (interactive) (today-capture-link-with-task-from-clipboard 'read)))
    ("w" (lambda () (interactive) (today-capture-link-with-task 'watch)))
    ("W" (lambda () (interactive) (today-capture-link-with-task-from-clipboard 'watch)))

    ("H" #'today-capture-here-from-clipboard)

    ("q" nil "quit"))

  (defhydra today-hydra (:foreign-keys run)
    "
^Today^
^^^^^^^^------------------------------
_c_: Capture
_a_: Archive completed todos
_l_: list all archived files
_f_: refile hydra
_g_: move entry to today-file

_t_: go to today-file
"
    ("c" #'today-capture-hydra/body :exit t)

    ("a" #'today-archive-done-todos :exit t)

    ("t" #'today :exit t)
    ("T" #'today-visit-todays-file :exit t)
    ("l" #'today-list :exit t)
    ("f" #'today-refile-hydra/body :exit t)

    ("g" #'today-move-to-today)

    ("q" nil "quit")))

(use-package dokument
  :straight (dokument :repo "git@github.com:jensecj/dokument.el.git")
  :demand t
  :bind
  (("C-+" . dokument))
  :commands (dokument
             dokument-company-menu-selection-quickhelp
             dokument-use-defaults)
  :config
  (setq dokument--posframe-font "Source Code Pro Semibold")

  (with-eval-after-load 'company
    (bind-key "C-+" #'dokument-company-menu-selection-quickhelp company-active-map))

  (dokument-use-defaults)
  :custom-face
  (internal-border ((t (:background "#777777")))))

(use-package replace-at-point
  :straight (replace-at-point :repo "git@github.com:jensecj/replace-at-point.el.git")
  :defer t
  :bind ("C-M-SPC" . replace-at-point)
  :config
  (replace-at-point-setup-defaults))

(use-package lowkey-mode-line
  :straight (lowkey-mode-line :repo "git@github.com:jensecj/lowkey-mode-line.el.git")
  :demand t
  :after pdf-tools
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

;;; third-party packages

(log-info "Loading third-party packages")

;;;; themes
(use-package zenburn-theme
  :straight t
  :demand t
  :config
  ;; replace wavy-underlines with straight ones in all faces
  (mapatoms (lambda (atom)
              (let ((underline nil))
                (when (and (facep atom)
                           (setq underline
                                 (face-attribute atom
                                                 :underline))
                           (eq (plist-get underline :style) 'wave))
                  (plist-put underline :style 'line)
                  (set-face-attribute atom nil
                                      :underline underline)))))

  (load-theme 'zenburn t)
  :custom-face
  (cursor ((t (:foreground ,(zenburn-get "zenburn-fg-1") :background ,(zenburn-get "zenburn-fg")))))
  (region ((t (:background  "#2B2B2B"))))
  (mode-line ((t (:box nil :background  "#2B2B2B"))))
  (header-line ((t (:box nil))))
  (mode-line-inactive ((t (:box nil))))
  (hl-line ((t (:background "gray30"))))
  (highlight ((t (:background nil :foreground nil)))))

;;;; major modes and extentions

(use-package lsp-mode :straight t :defer t)
(use-package cmake-mode :straight t :defer t :mode "\\CmakeLists.txt\\'")
(use-package yaml-mode :straight t :defer t :mode ("\\.yaml\\'" "\\.yml\\'"))
(use-package toml-mode :straight t :defer t :mode ("\\.toml\\'"))
(use-package dockerfile-mode :straight t :defer t :mode "\\Dockerfile\\'")
(use-package gitconfig-mode :straight t :defer t :mode "\\.gitconfig\\'")
(use-package gitignore-mode :straight t :defer t :mode "\\.gitignore\\'")
(use-package lua-mode :straight t :defer t :mode "\\.lua\\'")
(use-package markdown-mode :straight t :defer t :mode ("\\.md\\'" "\\.card\\'"))
(use-package scss-mode :straight t :defer t :mode "\\.scss\\'")
(use-package tuareg :straight t :defer t :mode ("\\.ml\\'" "\\.mli\\'" "\\.mli\\'" "\\.mll\\'" "\\.mly\\'"))
(use-package restclient :straight t :defer t)
(use-package json-mode :straight t :defer t :hook ((json-mode . flycheck-mode)))
(use-package ini-mode :straight t :defer t)
(use-package systemd :straight t :defer t)
(use-package nginx-mode :straight t :defer t)


(use-package rust-mode
  :straight t
  :defer t
  :bind
  (:map rust-mode-map
        ("C-c n" . rust-format-buffer))
  :mode "\\.rs\\'"
  :config
  (unbind-key "M-," rust-mode-map)
  (unbind-key "M-." rust-mode-map)
  (unbind-key "M--" rust-mode-map))

(use-package racer
  :straight t
  :after rust-mode
  :hook ((rust-mode . racer-mode)
         (racer-mode . eldoc-mode)))

(use-package clojure-mode
  :straight t
  :defer t
  :after (company-mode cider clj-refactor)
  :functions jens/company-clojure-quickhelp-at-point
  :commands (cider-create-doc-buffer
             cider-try-symbol-at-point)
  :bind
  (:map clojure-mode-map
        ("C-+" . jens/company-clojure-quickhelp-at-point))
  :config
  (unbind-key "M-," clojure-mode-map)
  (unbind-key "M-." clojure-mode-map)
  (unbind-key "M--" clojure-mode-map)

  (company-mode +1)
  (cider-mode +1)
  (clj-refactor-mode +1)

  ;; (setq cider-cljs-lein-repl
  ;;       "(do (require 'figwheel-sidecar.repl-api)
  ;;        (figwheel-sidecar.repl-api/start-figwheel!)
  ;;        (figwheel-sidecar.repl-api/cljs-repl))")
  )

(use-package cider
  :straight t
  :after clojure-mode
  :hook (clojure-mode . cider-mode)
  :config
  (setq cider-repl-use-pretty-printing t
        cider-prompt-for-symbol nil
        cider-print-fn 'pprint
        cider-repl-pop-to-buffer-on-connect nil
        cider-default-cljs-repl nil
        cider-check-cljs-repl-requirements nil))

(use-package clj-refactor
  :straight t
  :after clojure-mode
  :hook (clojure-mode . clj-refactor-mode)
  :config
  ;; don't warn on refactor eval
  (setq cljr-warn-on-eval nil))

(use-package magit
  :straight t
  :defer 30
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
  (setq magit-section-visibility-indicator '("…", t))

  (add-hook 'git-commit-mode-hook #'flyspell-prog-mode))

(use-package magithub
  ;; TODO: replace with forge.el?
  ;; https://github.com/vermiculus/magithub/blob/9fb9c653d0dad3da7ccff3ae321fa6e54c08f41b/magithub.el#L223
  ;; https://github.com/vermiculus/ghub-plus
  ;; https://github.com/vermiculus/apiwrap.el
  ;; https://github.com/magit/ghub/issues/84
  ;; :disabled t
  :straight t
  :after magit
  :config
  (magithub-feature-autoinject t))

(use-package magit-todos
  :straight t
  :after magit
  :hook (magit-status-mode . magit-todos-mode)
  :commands magit-todos-mode
  :config
  (setq magit-todos-exclude-globs '("var/*" "venv/*")))

(use-package auctex
  :straight t
  :defer t
  :hook (LaTeX-mode-hook . reftex-mode)
  :defines (TeX-view-program-selection
            TeX-view-program-list)
  :functions (TeX-PDF-mode TeX-source-correlate-mode)
  :config
  (setq TeX-PDF-mode t) ;; default to pdf
  (setq TeX-global-PDF-mode t) ;; default to pdf
  (setq TeX-parse-self t) ;; parse on load
  (setq TeX-auto-save t) ;; parse on save
  (setq TeX-save-query nil) ;; save before compiling
  (setq TeX-master nil) ;; try to figure out which file is the master
  (setq reftex-plug-into-AUCTeX t) ;; make reftex and auctex work together
  (setq doc-view-resolution 300)

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

(use-package elfeed
  :straight t
  :defer 60
  :commands (elfeed elfeed-search-selected jens/load-elfeed)
  :functions jens/elfeed-copy-link-at-point
  :bind
  (:map elfeed-search-mode-map
        ("t" . today-capture-elfeed-at-point)
        ("c" . jens/elfeed-copy-link-at-point)
        ("V" . jens/elfeed-play-video-at-point))
  :config
  (setq elfeed-search-filter "@1-month-ago +unread ")

  (setq elfeed-search-trailing-width 25)

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

  (defun jens/load-elfeed ()
    (interactive)
    (setq elfeed-feeds (jens/load-from-file (locate-user-emacs-file "elfeeds.el"))))

  (jens/load-elfeed)

  (defun jens/elfeed-select-emacs-after-browse-url (fn &rest args)
    "Activate the emacs-window"
    (let* ((emacs-window (shell-command-to-string "xdotool getactivewindow"))
           (active-window "")
           (counter 0))
      (apply fn args)
      (sleep-for 0.7)
      (while (and (not (string= active-window emacs-window))
                  (< counter 15))
        (sleep-for 0.2)
        (incf counter)
        (setq active-window (shell-command-to-string "xdotool getactivewindow"))
        (shell-command-to-string (format "xdotool windowactivate %s" emacs-window)))))

  (advice-add #'elfeed-search-browse-url :around #'jens/elfeed-select-emacs-after-browse-url)

  (defun jens/elfeed-play-video-at-point ()
    "Attempt to play the video link of the elfeed entry at point."
    (interactive)
    (letrec ((entry (car (elfeed-search-selected)))
             (link (elfeed-entry-link entry))
             (mpv-buf (get-buffer-create "*mpv*")))
      (start-process "mpv-ytdl" mpv-buf "mpv" "--ytdl-format=bestvideo[width<=1920][height<=1080]" "--ytdl" link)
      (view-buffer-other-window mpv-buf)))

  (defun jens/elfeed-copy-link-at-point ()
    "Copy the link of the elfeed entry at point to the
clipboard."
    (interactive)
    (letrec ((entry (car (elfeed-search-selected)))
             (link (elfeed-entry-link entry)))
      (with-temp-buffer
        (insert link)
        (clipboard-kill-ring-save (point-min) (point-max))
        (message (format "copied %s to clipboard" link)))))

  :custom-face
  (elfeed-search-date-face ((t (:underline nil))))
  (elfeed-search-unread-title-face ((t (:height 100 :strike-through nil))))
  (elfeed-search-title-face ((t (:height 90 :strike-through t)))))

(use-package pdf-tools
  :straight t
  :demand t
  :commands pdf-tools-install
  :hook (doc-view-mode . pdf-tools-install)
  :bind
  ;; need to use plain isearch, pdf-tools hooks into it to handle searching
  (:map pdf-view-mode-map ("C-s" . isearch-forward))
  :config
  (add-hook 'pdf-view-mode-hook #'auto-revert-mode)
  (add-hook 'doc-view-mode-hook #'auto-revert-mode)

  ;; TODO: figure out how to disable epdf asking to rebuild when starting
  ;; emacsclient, it does not work.

  ;; (pdf-tools-install)
  )

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
  (defalias #'describe-key #'helpful-key))

(use-package erc
  :defer t
  :after auth-source-pass
  :functions ercgo
  :commands (erc-tls ercgo)
  :config
  (setq erc-rename-buffers t
        erc-interpret-mirc-color t
        erc-prompt ">"
        erc-track-enable-keybindings nil
        erc-insert-timestamp-function 'erc-insert-timestamp-left
        erc-lurker-hide-list '("JOIN" "PART" "QUIT"))

  (setq erc-user-full-name user-full-name)
  (erc-hl-nicks-enable)

  (setq erc-autojoin-channels-alist '(("freenode.net" "#emacs" "##java")))

  (defun ercgo ()
    (interactive)
    (erc-tls :server "irc.freenode.net"
             :port 6697
             :nick "jensecj"
             :password (auth-source-pass-get 'secret "irc/freenode/jensecj"))))

(use-package notmuch
  ;; TODO: setup notmuch for multiple mail-profiles
  ;; see https://www.djcbsoftware.nl/code/mu/mu4e/Contexts-example.html
  :straight t
  :defer 40
  :bind
  (:map notmuch-show-mode-map
        ("B" . #'jens/notmuch-show-list-links)
        ("N" . #'notmuch-show/goto-next-unread-message)
        ("P" . #'notmuch-show/goto-previous-unread-message)
        :map notmuch-search-mode-map
        ("N" . #'notmuch-search/goto-next-unread-thread)
        ("P" . #'notmuch-search/goto-previous-unread-thread)
        ("<C-return>" . #'notmuch-search/show-thread-first-unread)
        :map notmuch-message-mode-map
        ("C-c C-a" . mail-add-attachment)
        :map notmuch-show-part-map
        ("V" . #'notmuch-show/view-mime-part-at-point-in-mode))
  :config
  (setq notmuch-fcc-dirs "sent +sent -new")
  (setq notmuch-column-control 1.0)
  (setq notmuch-wash-wrap-lines-length 80)
  (setq notmuch-wash-citation-lines-prefix 20)
  (setq notmuch-wash-citation-lines-suffix 0)
  (setq notmuch-wash-button-signature-hidden-format "[ signature -- click to show ]")
  (setq notmuch-wash-button-signature-visible-format "[ signature -- click to hide ]")
  (setq notmuch-wash-button-citation-hidden-format "[ %d more lines -- click to show ]")
  (setq notmuch-wash-button-citation-visible-format "[ %d more lines -- click to hide ]")

  ;; notmuch-message-forwarded-tags
  ;; notmuch-draft-tags
  ;; notmuch-message-replied-tags
  (add-to-list* 'notmuch-archive-tags '("+archived" "-deleted"))

  (defface notmuch-search-muted-face
    `((t (:foreground ,(zenburn-get "zenburn-bg+3"))))
    "Face used in search modes for muted threads.")

  (add-to-list 'notmuch-search-line-faces
               '("muted" . notmuch-search-muted-face))

  (defun notmuch/excl (query &rest tags)
    "Exclude TAGS from QUERY."
    (let* ((default-excludes '("archived" "lists" "builds" "draft" "sent"))
           (all-tags (-concat default-excludes tags))
           (excludes (s-join " " (-map (lambda (tag) (format "and not tag:%s" tag)) all-tags))))
      (format "%s %s" query excludes)))

  (setq notmuch-saved-searches
        `((:name "unread"  :key "u" :query ,(notmuch/excl "tag:unread") :sort-order newest-first)
          (:name "today"   :key "t" :query ,(notmuch/excl "date:today..") :sort-order newest-first)
          (:name "7 days"  :key "W" :query ,(notmuch/excl "date:7d..") :sort-order newest-first)
          (:name "30 days" :key "M" :query ,(notmuch/excl "date:30d..") :sort-order newest-first)
          (:name "drafts"  :key "d" :query "tag:draft" :sort-order newest-first)
          (:name "inbox"   :key "i" :query "tag:inbox" :sort-order newest-first)
          (:blank t)
          (:name "emacs-devel" :key "ld" :query "tag:lists/emacs-devel" :sort-order newest-first)
          (:name "emacs-help"  :key "lh" :query "tag:lists/emacs-help" :sort-order newest-first)
          (:blank t)
          (:name "builds"   :key "B" :query "tag:builds" :sort-order newest-first)
          (:name "bills"    :key "b" :query "tag:bills" :sort-order newest-first)
          (:name "personal" :key "p" :query "tag:personal" :sort-order newest-first)
          (:name "work"     :key "w" :query "tag:work" :sort-order newest-first)
          (:blank t)
          (:name "all mail" :key "a" :query "not tag:lists and not tag:draft" :sort-order newest-first)
          (:name "archive"  :key "r" :query "tag:archived" :sort-order newest-first)
          (:name "sent"     :key "s" :query "tag:sent" :sort-order newest-first)
          (:name "trash"    :key "h" :query "tag:deleted" :sort-order newest-first)))

  (setq notmuch-tagging-keys
        '(("a" ("-inbox" "+archived") "archive")
          ("r" ("-unread") "mark read")
          ("d" ("-unread" "-inbox" "-archived" "+deleted") "delete")
          ("f" ("+flagged") "flag")
          ("b" ("+bills" "-inbox" "-archived") "bill")
          ("p" ("+personal" "-inbox" "-archived") "personal")
          ("w" ("+work" "-inbox" "-archived") "work")
          ("m" ("-unread" "+muted") "mute")))

  (defun notmuch/quicktag (mode key tags)
    "Easily add a new tag keybinding to a `notmuch' mode-map."
    (if (member mode '(show tree search))
        (let ((mode-map (intern (format "notmuch-%s-mode-map" mode)))
              (fn (intern (format "notmuch-%s-add-tag" mode))))
          (define-key mode-map key (lambda () (interactive) (funcall fn tags))))
      (error "%s is not a proper notmuch mode" mode)))

  ;; delete mail in all modes with "d"
  (apply* #'notmuch/quicktag '(show search tree) "d" '(("-inbox" "-archived" "+deleted")))
  ;; mute mail in all modes with "M"
  (apply* #'notmuch/quicktag '(show search tree) "M" '(("-unread" "+muted")))

  ;;;;;;;;;;;;;;;;;;;;;;;
  ;; notmuch-show mode ;;
  ;;;;;;;;;;;;;;;;;;;;;;;

  (setq notmuch-show-logo nil)
  (setq notmuch-show-indent-messages-width 1)
  (setq notmuch-message-headers-visible nil)

  (defun jens/notmuch-show-list-links ()
    "List links in the current message, if one is selected, browse to it."
    (interactive)
    (let ((links (notmuch-show--gather-urls)))
      (if links
          (browse-url (completing-read "Links: " links)))))

  (defun notmuch-show/eldoc ()
    "Simple eldoc handler for `notmuch-show-mode'.

Inserts information about senders, and the mail subject into eldoc."
    (let* ((headers (notmuch-show-get-prop :headers))
           (from (map-elt headers :From))
           (subject (map-elt headers :Subject)))
      (format "%s - %s" from subject)))

  (easy-eldoc notmuch-show-mode-hook notmuch-show/eldoc)

  (defun notmuch/enable-debbugs ()
    (setq bug-reference-url-format "https://debbugs.gnu.org/%s")
    (bug-reference-mode +1))

  (add-hook 'notmuch-show-mode-hook #'notmuch/enable-debbugs)

  (defun notmuch-show/view-mime-part-at-point-in-mode ()
    "Open MIME-part at point in a specific major-mode."
    (interactive)

    (let* ((modes '(org-mode text-mode))
           (mode (intern (completing-read "mode: " modes nil t))))
      (notmuch-show-apply-to-current-part-handle
       (lambda (handle)
         (let ((buf (get-buffer-create (generate-new-buffer-name
				                                (concat " *notmuch-internal-part*")))))
           (switch-to-buffer buf)
           (if (eq (mm-display-part handle) 'external)
	             (kill-buffer buf)
             (goto-char (point-min))
             (set-buffer-modified-p nil)
             (funcall mode)
             (if (eq major-mode 'org-mode)
                 (org-show-all))
             (view-buffer buf #'kill-buffer-if-not-modified)
             (font-lock-fontify-buffer))))
       "text/plain")))

  (defun notmuch-show/message-has-tag-p (tag)
    "Return non-nil if the message-at-point is tagged with TAG."
    (let ((tags (notmuch-show-get-prop :tags))
          (orig-tags (notmuch-show-get-prop :orig-tags)))
      (member tag (-concat tags orig-tags))))

  (defun notmuch-show/goto-message-with-tag (tag &optional backwards)
    "Jump to the next message tagged with TAG.

if BACKWARDS is non-nil, jump backwards instead."
    (let (msg)
      (while (and (setq msg (if backwards
                                (notmuch-show-goto-message-previous)
                              (notmuch-show-goto-message-next)))
		              (not (notmuch-show/message-has-tag-p tag))))
      (if msg
	        (notmuch-show-message-adjust))
      msg))

  (defun notmuch-show/goto-next-unread-message ()
    "Jump forwards to the next unread message."
    (interactive)
    (notmuch-show/goto-message-with-tag "unread"))

  (defun notmuch-show/goto-previous-unread-message ()
    "Jump backwards to the previous unread message."
    (interactive)
    (notmuch-show/goto-message-with-tag "unread" 'backwards))

  ;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; notmuch-search mode ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;

  (setq notmuch-search-oldest-first nil)

  (defun notmuch-search/eldoc ()
    "Simple eldoc handler for `notmuch-search-mode'.

Inserts information about the authors of the mail-at-point
into eldoc."
    (let ((data (notmuch-search-get-result)))
      (map-elt data :authors)))

  (easy-eldoc notmuch-search-mode-hook notmuch-search/eldoc)

  (defun notmuch-search/thread-has-tag-p (tag)
    "Return non-nil if the thread-at-point is tagged with TAG."
    (let ((tags (notmuch-search-get-tags)))
      (member tag tags)))

  (defun notmuch-search/goto-thread-with-tag (tag &optional backwards)
    "Jump forwards to the next thread that is tagged with TAG.

if BACKWARDS is non-nil, jump backwards instead."
    (let (thread)
      (while (and (setq thread (if backwards
                                   (notmuch-search-previous-thread)
                                 (notmuch-search-next-thread)))
		              (not (notmuch-search/thread-has-tag-p tag))))
      thread))

  (defun notmuch-search/goto-next-unread-thread ()
    "Jump forward to the next unread thread."
    (interactive)
    (notmuch-search/goto-thread-with-tag "unread"))

  (defun notmuch-search/goto-previous-unread-thread ()
    "Jump back to the previous unread thread."
    (interactive)
    (notmuch-search/goto-thread-with-tag "unread" 'backwards))

  (defun notmuch-search/show-thread-first-unread ()
    "Show the thread-at-point, but jump to the first unread message."
    (interactive)
    (notmuch-search-show-thread)
    (goto-char (point-min))
    (unless (notmuch-show/message-has-tag-p "unread")
      (notmuch-show/goto-next-unread-message)))

  ;;;;;;;;;;;;
  ;; extras ;;
  ;;;;;;;;;;;;

  ;; TODO: fix these so org-store-link stores link to the message at ;; point in search mode.
  (require 'org-notmuch)
  (define-key notmuch-show-mode-map (kbd "C-c C-l") #'org-store-link)
  (define-key notmuch-search-mode-map (kbd "C-c C-l") #'org-store-link)

  :custom-face
  (notmuch-wash-cited-text ((t (:inherit font-lock-comment-face))))
  (notmuch-wash-toggle-button ((t (:foreground ,(zenburn-get "zenburn-yellow") :background ,(zenburn-get "zenburn-bg")))))
  (notmuch-message-summary-face ((t (:background ,(zenburn-get "zenburn-bg-1") :height 105 :extend t))))
  (notmuch-search-unread-face ((t (:weight bold :foreground ,(zenburn-get "zenburn-yellow")))))
  (notmuch-tag-deleted ((t (:foreground ,(zenburn-get "zenburn-red") :underline "red" :strike-through nil))))
  (notmuch-tag-face ((t (:foreground "#11ff11")))))

;;;; extensions to built-in packages

(use-package erc-hl-nicks
  :straight t
  :defer t
  :commands erc-hl-nicks-enable)

(use-package dired-filter :straight t :after dired)
(use-package dired-collapse :straight t :after dired)

(use-package dired-subtree
  :straight t
  :after dired
  :bind
  (:map dired-mode-map
        ("<tab>" . dired-subtree-toggle))
  :config
  (setq dired-subtree-use-backgrounds nil))

(use-package dired-rsync
  :straight t
  :after dired
  :bind
  (:map dired-mode-map
        ("C" . dired-rsync))
  :config
  (setq dired-rsync-options "-azz --info=progress2"))

(use-package dired-rainbow
  :straight t
  :after dired
  :config
  (dired-rainbow-define-chmod directory "#6cb2eb" "d.*")
  (dired-rainbow-define html "#eb5286" ("css" "less" "sass" "scss" "htm" "html" "jhtm" "mht" "eml" "mustache" "xhtml"))
  (dired-rainbow-define xml "#f2d024" ("xml" "xsd" "xsl" "xslt" "wsdl" "bib" "json" "msg" "pgn" "rss" "yaml" "yml" "rdata"))
  (dired-rainbow-define document "#9561e2" ("docm" "doc" "docx" "odb" "odt" "pdb" "pdf" "ps" "rtf" "djvu" "epub" "odp" "ppt" "pptx"))
  (dired-rainbow-define markdown "#ffed4a" ("org" "etx" "info" "markdown" "md" "mkd" "nfo" "pod" "rst" "tex" "textfile" "txt"))
  (dired-rainbow-define database "#6574cd" ("xlsx" "xls" "csv" "accdb" "db" "mdb" "sqlite" "nc"))
  (dired-rainbow-define media "#de751f" ("mp3" "mp4" "MP3" "MP4" "avi" "mpeg" "mpg" "flv" "ogg" "mov" "mid" "midi" "wav" "aiff" "flac"))
  (dired-rainbow-define image "#DCA3A3" ("tiff" "tif" "cdr" "gif" "ico" "jpeg" "jpg" "png" "psd" "eps" "svg"))
  (dired-rainbow-define log "#c17d11" ("log"))
  (dired-rainbow-define shell "#f6993f" ("awk" "bash" "bat" "sed" "sh" "zsh" "vim"))
  (dired-rainbow-define interpreted "#38c172" ("py" "ipynb" "rb" "pl" "t" "msql" "mysql" "pgsql" "sql" "r" "clj" "cljs" "scala" "js"))
  (dired-rainbow-define compiled "#F0DFAF" ("asm" "cl" "lisp" "el" "c" "h" "c++" "h++" "hpp" "hxx" "m" "cc" "cs" "cp" "cpp" "go" "f" "for" "ftn" "f90" "f95" "f03" "f08" "s" "rs" "hi" "hs" "pyc" ".java"))
  (dired-rainbow-define executable "#8FB28F" ("exe" "msi"))
  (dired-rainbow-define compressed "#51d88a" ("7z" "zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar"))
  (dired-rainbow-define packaged "#faad63" ("deb" "rpm" "apk" "jad" "jar" "cab" "pak" "pk3" "vdf" "vpk" "bsp"))
  (dired-rainbow-define encrypted "#ffed4a" ("gpg" "pgp" "asc" "bfe" "enc" "signature" "sig" "p12" "pem"))
  (dired-rainbow-define fonts "#6cb2eb" ("afm" "fon" "fnt" "pfb" "pfm" "ttf" "otf"))
  (dired-rainbow-define partition "#e3342f" ("dmg" "iso" "bin" "nrg" "qcow" "toast" "vcd" "vmdk" "bak"))
  (dired-rainbow-define vc "#8CD0D3" ("git" "gitignore" "gitattributes" "gitmodules"))
  (dired-rainbow-define-chmod executable-unix "#8FB28F" "-.*x.*"))

;; FIXME: does not load properly, only after calling one of the bound keys
(use-package dired+
  :straight (dired+ :type git :host github :repo "emacsmirror/dired-plus")
  :after dired
  :bind
  (:map dired-mode-map
        ("<backspace>" . diredp-up-directory-reuse-dir-buffer)
        ("C-<up>" . nil)
        ("C-<down>" . nil))
  :config
  (toggle-diredp-find-file-reuse-dir +1)
  (global-dired-hide-details-mode +1)
  :custom-face
  (diredp-omit-file-name ((t (:foreground "#afafaf" :inherit diredp-ignored-file-name :strike-through nil))))
  (diredp-dir-heading ((t (:background "#4f4f4f"))))
  (diredp-dir-priv ((t (:foreground "#8CD0D3"))))
  (diredp-file-name ((t (:foreground "#DCDCCC"))))
  (diredp-dir-name ((t (:foreground "#8CD0D3")))))

(use-package slime
  :defer t
  :straight t
  :after macrostep
  :commands (jens/qlot-slime slime-start)
  :bind (:map slime-mode-map
              ("C-x C-e" . jens/slime-eval-last-sexp))
  :config
  (setq inferior-lisp-program "sbcl")
  (setq slime-contribs '(slime-fancy))

  (defun jens/slime-eval-last-sexp ()
    "Show the result of evaluating the last-sexp in an overlay."
    (interactive)
    (slime-eval-async
     `(swank:eval-and-grab-output ,(slime-last-expression))
     (lambda (result)
       (cl-destructuring-bind (output value) result
         (let ((string (s-replace "\n" " " (concat output value))))
           (message string)
           (eros--eval-overlay string (point))))))
    (slime-sync))

  (defun jens/qlot-slime (directory)
    "Start Common Lisp REPL using project-local libraries via
`roswell' and `qlot'."
    (interactive (list (read-directory-name "Project directory: ")))
    (slime-start
     :program "~/.roswell/bin/qlot"
     :program-args '("exec" "ros" "-S" "." "run")
     :directory directory
     :name 'qlot
     :env (list (concat "PATH="
                        (mapconcat 'identity exec-path ":"))
                (concat "QUICKLISP_HOME="
                        (file-name-as-directory directory) "quicklisp/")))))

(use-package geiser
  :straight t
  :defer t
  :hook (scheme-mode . geiser-mode)
  :config
  (setq geiser-active-implementations '(chicken)))

(use-package elpy
  :straight t
  :defer t
  :delight " elpy"
  :commands (elpy-goto-definition)
  :hook (python-mode . elpy-mode)
  :bind (:map elpy-mode-map ("C-c C-c" . nil))
  :custom
  (elpy-modules
   '(elpy-module-sane-defaults
     elpy-module-company
     elpy-module-eldoc
     elpy-module-pyvenv)))

(use-package blacken
  :straight t
  :after python-mode)

(use-package highlight-defined
  :straight t
  :diminish highlight-defined-mode
  :hook (emacs-lisp-mode . highlight-defined-mode)
  :custom-face
  (highlight-defined-function-name-face ((t (:foreground "#BDE0F3"))))
  (highlight-defined-builtin-function-name-face ((t (:foreground "#BFBFBF")))))

(use-package highlight-thing
  :straight t
  :diminish highlight-thing-mode
  :config
  (setq highlight-thing-ignore-list '("nil" "t"))
  (setq highlight-thing-delay-seconds 0.4)
  (setq highlight-thing-case-sensitive-p nil)
  (setq highlight-thing-exclude-thing-under-point nil)
  (global-highlight-thing-mode +1)
  :custom-face
  (highlight-thing ((t (:background "#5f5f5f" :weight bold)))))

(use-package fontify-face
  :straight t
  :defer t
  :diminish fontify-face-mode
  :hook (emacs-lisp-mode . fontify-face-mode))

(use-package package-lint :straight t :defer t :commands (package-lint-current-buffer))
(use-package relint :straight t :defer t :commands (relint-current-buffer relint-file relint-directory))
(use-package flycheck-package :straight t :defer t :commands (flycheck-package-setup))

(use-package ggtags
  :straight t
  :defer t
  :delight " ggtags "
  :hook (c++-mode . ggtags-mode)
  :custom-face
  (ggtags-highlight ((t ()))))

;; language server for c++
(use-package ccls
  :ensure t
  :hook ((c++-mode . #'lsp))
  :commands (lsp))

(use-package clang-format :straight t :defer t)

(use-package flymake-shellcheck
  :straight t
  :commands flymake-shellcheck-load
  :init
  (add-hook 'sh-mode-hook #'flymake-shellcheck-load))

(use-package rmsbolt :straight t :defer t)

(use-package ob-async
  :disabled t
  :straight t
  :defer t)

(use-package ob-clojure
  :disabled t
  :requires cider
  :config
  (setq org-babel-clojure-backend 'cider))

(use-package ox-pandoc :straight t :defer t :after org)
(use-package ox-asciidoc :straight t :defer t :after org)
(use-package org-ref :straight t :defer t)

;;;; minor modes

(use-package git-timemachine :straight t :defer t)
(use-package centered-cursor-mode :straight t :defer t)
(use-package rainbow-mode :straight t :defer t :diminish t) ;; highlight color-strings (hex, etc.)

(use-package outshine
  :straight t
  :diminish t
  :hook ((emacs-lisp-mode) . outshine-mode)
  :bind
  (:map outshine-mode-map
        ("M-<up>" . nil)
        ("M-<down>" . nil))
  :config
  (setq outshine-fontify-whole-heading-line t)
  (setq outshine-use-speed-commands t)
  (setq outshine-preserve-delimiter-whitespace nil)

  (setq outshine-speed-commands-user '(("g" . counsel-outline)))

  (defun jens/outshine-refile-region ()
    "Refile the contents of the active region to another heading
in the same file."
    (interactive)
    (if (not (use-region-p))
        (message "No active region to refile")
      (let ((content (buffer-substring (region-beginning) (region-end)))
            (settings (cdr (assq major-mode counsel-outline-settings))))
        (ivy-read "refile to: " (counsel-outline-candidates settings)
                  :action (lambda (x)
                            (save-excursion
                              (kill-region (region-beginning) (region-end))
                              (goto-char (cdr x))
                              (forward-line)
                              (insert content)
                              (newline)))))))

  ;; fontify the entire outshine-heading, including the comment
  ;; characters (;;;)
  (if (not (fn-checksum #'outshine-fontify-headlines
                        "77bb9734c4c082edc89a0270eb79ac77"))
      (log-warning "`outshine-fontify-headlines' changed definition, ignoring patch.")
    (advice-patch #'outshine-fontify-headlines
                  '(font-lock-new-keywords
                    `((,heading-1-regexp 0 'outshine-level-1 t)
                      (,heading-2-regexp 0 'outshine-level-2 t)
                      (,heading-3-regexp 0 'outshine-level-3 t)
                      (,heading-4-regexp 0 'outshine-level-4 t)
                      (,heading-5-regexp 0 'outshine-level-5 t)
                      (,heading-6-regexp 0 'outshine-level-6 t)
                      (,heading-7-regexp 0 'outshine-level-7 t)
                      (,heading-8-regexp 0 'outshine-level-8 t)))
                  '(font-lock-new-keywords
                    `((,heading-1-regexp 1 'outshine-level-1 t)
                      (,heading-2-regexp 1 'outshine-level-2 t)
                      (,heading-3-regexp 1 'outshine-level-3 t)
                      (,heading-4-regexp 1 'outshine-level-4 t)
                      (,heading-5-regexp 1 'outshine-level-5 t)
                      (,heading-6-regexp 1 'outshine-level-6 t)
                      (,heading-7-regexp 1 'outshine-level-7 t)
                      (,heading-8-regexp 1 'outshine-level-8 t)))))
  :custom-face
  (outshine-level-1 ((t (:inherit outline-1 :background nil :weight bold :foreground "#DFAF8F"))))
  (outshine-level-2 ((t (:inherit outline-2 :background nil :weight bold))))
  (outshine-level-3 ((t (:inherit outline-3 :background nil :weight bold))))
  (outshine-level-4 ((t (:inherit outline-4 :background nil :weight bold))))
  (outshine-level-5 ((t (:inherit outline-5 :background nil :weight bold)))))

(use-package outline-minor-faces
  ;; required by `backline'
  :straight t
  :custom-face
  (outline-minor-1 ((t (:inherit outshine-level-1))))
  (outline-minor-2 ((t (:inherit outshine-level-2))))
  (outline-minor-3 ((t (:inherit outshine-level-3))))
  (outline-minor-4 ((t (:inherit outshine-level-4))))
  (outline-minor-5 ((t (:inherit outshine-level-5)))))

(use-package backline
  :straight t
  :after outshine
  :config
  ;; highlight the entire line with outline-level face, even if collapsed.
  (advice-add #'outline-flag-region :after 'backline-update))

(use-package flycheck
  :straight t
  :defer t
  :config
  (defun magnars/adjust-flycheck-automatic-syntax-eagerness ()
    "Adjust how often we check for errors based on if there are any.
  This lets us fix any errors as quickly as possible, but in a
  clean buffer we're an order of magnitude laxer about checking."
    (setq flycheck-idle-change-delay
          (if flycheck-current-errors 0.3 3.0)))

  ;; Each buffer gets its own idle-change-delay because of the
  ;; buffer-sensitive adjustment above.
  (make-variable-buffer-local 'flycheck-idle-change-delay)

  (add-hook 'flycheck-after-syntax-check-hook
            #'magnars/adjust-flycheck-automatic-syntax-eagerness)

  ;; Remove newline checks, since they would trigger an immediate check
  ;; when we want the idle-change-delay to be in effect while editing.
  (setq-default flycheck-check-syntax-automatically '(save idle-change mode-enabled)))

(use-package yasnippet
  :straight t
  :defer t
  :diminish yas-minor-mode
  :bind ("C-<return>" . yas-expand)
  :config
  (setq yas-snippet-dirs (list (locate-user-emacs-file "snippets")))
  (setq yas-indent-line 'fixed)
  (setq yas-also-auto-indent-first-line t)
  (setq yas-also-indent-empty-lines t)
  (yas-reload-all)
  (yas-global-mode +1))

(use-package smartparens
  ;; TODO: replace this
  :straight t
  :defer t
  :bind (("M-<up>" .  sp-backward-barf-sexp)
         ("M-<down>" . sp-forward-barf-sexp)
         ("M-<left>" . sp-backward-slurp-sexp)
         ("M-<right>" . sp-forward-slurp-sexp)
         ("C-S-a" . sp-beginning-of-sexp)
         ("C-S-e" . sp-end-of-sexp)
         ("S-<next>" . sp-split-sexp)
         ("S-<prior>" . sp-join-sexp)))

(use-package macrostep
  :straight t
  :bind ("C-c e" . macrostep-expand))

(use-package diff-hl
  :straight t
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
   (dired-mode . diff-hl-dired-mode-unless-remote))
  :config
  (setq diff-hl-dired-extra-indicators t)
  (setq diff-hl-draw-borders nil)

  (defun jens/diff-hl-refresh ()
    (diff-hl-mode +1))

  (defhydra jens/diff-hl-hydra ()
    "Move to changed VC hunks."
    ("n" #'diff-hl-next-hunk "next")
    ("p" #'diff-hl-previous-hunk "previous"))

  (global-diff-hl-mode +1)
  :custom-face
  (diff-hl-dired-unknown ((t (:inherit diff-hl-dired-insert))))
  (diff-hl-dired-ignored ((t (:background "#2b2b2b")))))

(use-package diff-hl-dired
  ;; git highlighting for dired-mode, installed as part of `diff-hl'
  :after diff-hl
  :defer t
  :config
  (defun jens/empty-fringe-bmp (_type _pos)
    "Always use the clean empty BMP for fringe display"
    'diff-hl-bmp-empty)

  ;; Use clean fringe style for highlighting
  (setq diff-hl-fringe-bmp-function #'jens/empty-fringe-bmp)

  (if (not (fn-checksum #'diff-hl-dired-highlight-items
                        "e06f15da2bf831295e015c9c82f11539"))
      (message "`diff-hl-dired-highlight-items' changed definition, ignoring patch.")
    (advice-patch #'diff-hl-dired-highlight-items
                  'jens/empty-fringe-bmp
                  'diff-hl-fringe-bmp-from-type)))

(use-package hl-todo
  :straight t
  :demand t
  :diminish hl-todo-mode
  :commands global-hl-todo-mode
  :config
  (global-hl-todo-mode +1))

(use-package shackle
  :straight t
  :demand t
  :config
  (setq shackle-rules
        '(((rx "*undo-tree*") :regexp t :select t :align right :inhibit-window-quit t)
          ((rx "*xref*") :regexp t :same t :inhibit-window-quit t)))
  (shackle-mode +1))

(use-package projectile
  :straight t
  :defer t
  :diminish projectile-mode
  :bind
  (("M-p c" . projectile-compile-project)
   ("M-p t" . projectile-test-project)
   ("M-p r" . projectile-run-project))
  :config
  (setq projectile-completion-system 'ivy)
  (setq projectile-enable-caching t)

  (projectile-register-project-type
   'ant
   '("build.xml")
   :compile "ant build"
   :test "ant test"
   :run "ant run")

  (projectile-mode +1))

(use-package counsel-projectile
  :straight t
  :after (counsel projectile)
  :commands counsel-projectile-mode
  :config (counsel-projectile-mode))

(use-package keyfreq
  :straight t
  :commands (keyfreq-mode keyfreq-autosave-mode)
  :config
  (keyfreq-mode +1)
  (keyfreq-autosave-mode +1))

;;;; misc packages

(use-package flx :straight t) ;; fuzzy searching for ivy, etc.
(use-package rg :straight t :after wgrep) ;; ripgrep in emacs
(use-package org-ql :straight (org-ql :type git :host github :repo "alphapapa/org-ql") :defer t)
(use-package dumb-jump :straight t :defer t)
(use-package gist :straight t :defer t) ;; work with github gists
(use-package popup :straight t)

(use-package auto-compile
  :straight t
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

(use-package frog-menu
  :straight t
  :after flyspell
  :config
  (setq frog-menu-avy-padding t)
  (setq frog-menu-min-col-padding 4)
  (setq frog-menu-format 'vertical)

  (defun frog-menu-flyspell-correct (candidates word)
    "Run `frog-menu-read' for the given CANDIDATES.

List of CANDIDATES is given by flyspell for the WORD.

Return selected word to use as a replacement or a tuple
of (command . word) to be used by `flyspell-do-correct'."
    (interactive)
    (let* ((corrects (if flyspell-sort-corrections
                         (sort candidates 'string<)
                       candidates))
           (actions `(("C-s" "Save word"         (save    . ,word))
                      ("C-a" "Accept (session)"  (session . ,word))
                      ("C-b" "Accept (buffer)"   (buffer  . ,word))
                      ("C-c" "Skip"              (skip    . ,word))))
           (prompt   (format "Dictionary: [%s]"  (or ispell-local-dictionary
                                                     ispell-dictionary
                                                     "default")))
           (res      (frog-menu-read prompt corrects actions)))
      (unless res
        (error "Quit"))
      res))

  (setq flyspell-correct-interface #'frog-menu-flyspell-correct))

(use-package spinner
  :straight t
  :config
  (defun jens/package-spinner-start (&rest _arg)
    "Create and start package-spinner."
    (when-let ((buf (get-buffer "*Packages*")))
      (with-current-buffer buf
        (spinner-start 'progress-bar 2))))

  (defun jens/package-spinner-stop (&rest _arg)
    "Stop the package-spinner."
    (when-let ((buf (get-buffer "*Packages*")))
      (with-current-buffer (get-buffer "*Packages*")
        (spinner-stop))))

  ;; create a spinner when launching `list-packages' and waiting for archive refresh
  (advice-add #'list-packages :after #'jens/package-spinner-start)
  (add-hook 'package--post-download-archives-hook #'jens/package-spinner-stop)

  ;; create a spinner when updating packages using `U x'
  (advice-add #'package-menu--perform-transaction :before #'jens/package-spinner-start)
  (advice-add #'package-menu--perform-transaction :after #'jens/package-spinner-stop))

(use-package org-web-tools
  :straight t
  :defer t
  :after (org s)
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
    "Get the duration of a youtube video, requires system tool
`youtube-dl'."
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
        (error "No url at point")))))

(use-package help-fns+
  :download "https://raw.githubusercontent.com/emacsmirror/emacswiki.org/master/help-fns%2B.el"
  :demand t)

(use-package pp+
  :download "https://raw.githubusercontent.com/emacsmirror/emacswiki.org/master/pp%2b.el"
  :demand t
  :bind (("M-:" . #'pp-eval-expression)))

(use-package amx
  :straight t
  :config
  (amx-mode))

(use-package prescient
  :straight t
  :demand t)

(use-package ivy-prescient
  :after (prescient ivy)
  :straight t
  :config
  (ivy-prescient-mode +1))

(use-package company-prescient
  :straight t
  :after (prescient company)
  :config
  (company-prescient-mode +1))

(use-package smart-jump
  :straight t
  :defer t
  :commands (smart-jump-register
             smart-jump-simple-find-references)
  :functions (jens/smart-jump-find-references-with-rg
              smart-jump-refs-search-rg
              jens/select-rg-window)
  :bind*
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

          (smart-jump-refs-search-rg
           (cond ((use-region-p)
                  (buffer-substring-no-properties (region-beginning)
                                                  (region-end)))
                 ((symbol-at-point)
                  (substring-no-properties
                   (symbol-name (symbol-at-point)))))))

      (message "Install `rg' to use `smart-jump-simple-find-references-with-rg'.")))

  (defun jens/select-rg-window nil
    "Select the `rg' buffer, if visible."
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
   :order 4)

  (smart-jump-register
   :modes 'c++-mode
   :jump-fn 'ggtags-find-tag-dwim
   :pop-fn 'ggtags-prev-mark
   :refs-fn 'ggtags-find-reference
   :should-jump  (lambda () (bound-and-true-p ggtags-mode))
   :heuristic 'point
   :async 500
   :order 3)

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
   :order 2)

  (smart-jump-register
   :modes 'c++-mode
   :jump-fn #'lsp-find-definition
   :pop-fn #'pop-tag-mark
   :refs-fn #'lsp-find-references
   :should-jump (lambda () (fboundp #'lsp))
   :heuristic 'point
   :async 500
   :order 1))

(use-package paxedit
  ;; TODO: replace this
  :straight t
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

(use-package elisp-demos
  :straight t
  :after helpful
  :config
  (advice-add #'helpful-update :after #'elisp-demos-advice-helpful-update))

(use-package iedit
  :straight t
  :bind* ("C-;" . iedit-mode))

(use-package multiple-cursors
  :straight t
  :bind
  (("C-d" . mc/mark-next-like-this)
   ("C-S-d" . mc/mark-previous-like-this)
   ("C-M-a" . mc/mark-all-like-this))
  :init
  (setq mc/list-file (no-littering-expand-etc-file-name "mc-lists.el")))

(use-package ace-mc
  :straight t
  :bind ("C-M-<return>" . ace-mc-add-multiple-cursors))

(use-package browse-kill-ring
  :straight t
  :defer t
  :bind ("C-x C-y" . browse-kill-ring)
  :config (setq browse-kill-ring-quit-action 'save-and-restore))

(use-package browse-at-remote :straight t :defer t)

(use-package avy
  :straight t
  :defer t
  :bind
  (("C-ø" . avy-goto-char)
   ("C-'" . avy-goto-line))
  :config
  (setq avy-background 't)

  (defun jens/avy-disable-highlight-thing (fn &rest args)
    "Disable `highlight-thing-mode' when avy-goto mode is active,
re-enable afterwards."
    (let ((toggle (bound-and-true-p highlight-thing-mode)))
      (when toggle (highlight-thing-mode -1))
      (unwind-protect
          (apply fn args)
        (when toggle (highlight-thing-mode +1)))))

  (advice-add #'avy-goto-char :around #'jens/avy-disable-highlight-thing)
  :custom-face
  (avy-background-face ((t (:foreground "gray50" :background "#313131"))))
  (avy-lead-face ((t (:background "#2B2B2B"))))
  (avy-lead-face-0 ((t (:background "#2B2B2B"))))
  (avy-lead-face-1 ((t (:background "#2B2B2B"))))
  (avy-lead-face-2 ((t (:background "#2B2B2B")))))

(use-package avy-zap
  :straight t
  :bind ("C-å" . avy-zap-to-char))

(use-package expand-region
  :straight t
  :bind
  (("M-e" . er/expand-region)
   ("C-M-e" . er/contract-region)))

(use-package change-inner
  :straight t
  :bind
  (("M-i" . copy-inner)
   ("M-o" . copy-outer)
   ("M-I" . change-inner)
   ("M-O" . change-outer)))

(use-package move-text
  :straight t
  :bind
  (("C-S-<up>" . move-text-up)
   ("C-S-<down>" . move-text-down)))

(use-package fullframe
  :straight t
  :config
  (fullframe magit-status magit-mode-quit-window)
  (fullframe list-packages quit-window))

(use-package undo-tree
  :straight t
  :defer t
  :diminish undo-tree-mode
  :commands global-undo-tree-mode
  :bind*
  (("C-x u" . undo-tree-visualize)
   ("C-_" . undo-tree-undo)
   ("M-_" . undo-tree-redo)
   :map undo-tree-visualizer-mode-map
   ("<return>" . undo-tree-visualizer-quit))
  :config
  (setq undo-tree-visualizer-diff t)
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist `(("." . ,no-littering-var-directory)))
  (global-undo-tree-mode))

(use-package goto-chg
  :straight t
  :defer t
  :bind ("M-ø" . goto-last-change))

(use-package beginend
  :straight t
  :defer t
  :diminish beginend-global-mode
  :commands beginend-global-mode
  :bind (("M-<" . beginning-of-buffer)
         ("M->" . end-of-buffer))
  :config
  ;; diminish all the `beginend' modes
  (mapc (lambda (s) (diminish (cdr s))) beginend-modes)
  (beginend-global-mode))

(use-package fold-this
  :straight t
  :bind
  (([(meta shift h)] . fold-this-unfold-all)
   ("M-h" . fold-this)
   (:map emacs-lisp-mode-map ("M-h" . fold-this-sexp)))
  :custom-face
  (fold-this-overlay ((t (:foreground nil :background "#5f5f5f")))))

(use-package which-key
  :straight t
  :diminish which-key-mode
  :commands (which-key-mode
             which-key-setup-side-window-right)
  :config
  (which-key-setup-side-window-right)
  (setq which-key-max-description-length 40)
  (which-key-mode))

(use-package wgrep
  :straight t
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

(use-package grep-context
  :straight t
  :bind (:map compilation-mode-map
              ("+" . grep-context-more-around-point)
              ("-" . grep-context-less-around-point)
              :map grep-mode-map
              ("+" . grep-context-more-around-point)
              ("-" . grep-context-less-around-point)
              :map ivy-occur-grep-mode-map
              ("+" . grep-context-more-around-point)
              ("-" . grep-context-less-around-point)))

(use-package exec-path-from-shell
  :straight t
  :defer 3
  :config
  (exec-path-from-shell-initialize)
  ;; try to grab the ssh-agent if it is running
  (exec-path-from-shell-copy-env "SSH_AGENT_PID")
  (exec-path-from-shell-copy-env "SSH_AUTH_SOCK"))

(use-package vterm
  :defer t
  :straight (vterm :type git :repo "https://github.com/akermu/emacs-libvterm.git")
  :init
  (add-to-list 'load-path (expand-file-name "~/software/emacs-libvterm"))
  :config
  (setq vterm-max-scrollback 10000))

(use-package multi-libvterm
  :after vterm
  :straight (multi-libvterm :type git :repo "https://github.com/suonlight/multi-libvterm.git")
  :bind (("C-z" . multi-libvterm))
  :config
  )

(use-package multi-term
  :disabled t
  :straight t
  :defer t
  :commands (multi-term-get-buffer
             multi-term-internal)
  :bind* (("C-z" . jens/multi-term)
          :map term-raw-map
          ("C-c C-j" . term-line-mode)
          :map term-mode-map
          ("C-c C-k" . term-char-mode))
  :config
  (setq multi-term-program "/bin/zsh")
  ;; (setq term-bind-key-alist '()) ;; clear the binds list, defaulting to emacs binds
  (setq term-buffer-maximum-size 10000)

  (defun jens/term-paste (&optional string)
    "Paste a string to the process of the current buffer, fixes
paste for multi-term mode."
    (interactive)
    (process-send-string
     (get-buffer-process (current-buffer))
     (if string string (current-kill 0))))
  (define-key term-raw-map (kbd "C-y") 'jens/term-paste)
  ;; (add-to-list 'term-bind-key-alist '("<C-left>" . term-send-backward-word))
  ;; (add-to-list 'term-bind-key-alist '("<C-right>" . term-send-forward-word))
  ;; (add-to-list 'term-bind-key-alist '("<C-backspace>" . (lambda () (interactive) (term-send-raw-string "\C-h")))) ;; backwards-kill-word
  ;; (add-to-list 'term-bind-key-alist '("<C-del>" . (lambda () (interactive) (term-send-raw-string "\e[3;5~")))) ;; forwards-kill-word

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
      (rename-buffer buffer-new-name))))

(use-package ivy
  :straight t
  :demand t
  :diminish ivy-mode
  :commands (ivy-read
             ivy-mode
             ivy-pop-view-action
             ivy-default-view-name
             ivy--get-window)
  :bind
  (("C-x C-b" . ivy-switch-buffer)
   ("C-c C-r" . ivy-resume)
   :map ivy-minibuffer-map
   ;; TODO: fix this, should jump to selected entries dir, not the candidates
   ("C-d" . (lambda () (interactive) (ivy-quit-and-run (dired ivy--directory))))
   ("C-<return>" . ivy-immediate-done)
   :map ivy-occur-grep-mode-map
   ("D" . ivy-occur-delete-candidate))
  :config
  (setq ivy-height 15)
  (setq ivy-count-format "")
  (setq ivy-use-virtual-buffers t)
  (setq ivy-re-builders-alist '((t . ivy--regex-plus)))
  (setq ivy-on-del-error-function nil)

  (ivy-mode +1)
  :custom-face
  (ivy-current-match ((t (:foreground nil :background "#292929" :height 110 :underline nil)))))

(use-package counsel
  :straight t
  :after ivy
  :defer 1
  :diminish counsel-mode
  :functions jens/counsel-read-file-name
  :commands (counsel-mode counsel--find-file-matcher)
  :bind
  (("C-S-s" . jens/ripgrep)
   ("C-x d" . counsel-dired)
   ("C-x C-f" . counsel-find-file)
   ("C-S-f" . counsel-fzf)
   ("C-x C-i" . counsel-imenu)
   ("C-x i" . counsel-outline)
   ("M-æ" . counsel-mark-ring)
   ("M-x" . counsel-M-x)
   ("M-b" . counsel-bookmark)
   :map help-map
   ("l" . counsel-find-library)
   :map org-mode-map
   ("C-c C-q" . counsel-org-tag))
  :init
  (setenv "FZF_DEFAULT_COMMAND" "fd --type f --hidden --follow --exclude .git")
  :config
  (setq counsel-outline-display-style 'path)
  (setq counsel-outline-face-style 'verbatim)

  (setq counsel-fzf-cmd "fzf -i -x -f \"%s\"")

  (setq counsel-yank-pop-height 25)
  (add-to-list 'ivy-height-alist '(counsel-yank-pop . 10))

  (setq counsel-yank-pop-separator
        (propertize "\n------------------------------------------\n"
                    'face `(:foreground "#111111")))

  (setq counsel-rg-base-command "rg --hidden -S --no-heading --line-number --color never %s .")

  (defun jens/ripgrep ()
    "Interactively search the current directory. Jump to result using ivy."
    (interactive)
    (let ((counsel-ag-base-command counsel-rg-base-command)
          (initial-input (if (use-region-p)
                             (buffer-substring-no-properties
                              (region-beginning) (region-end)))))
      (deactivate-mark)
      (counsel-rg initial-input default-directory)))

  (counsel-mode))

(use-package counsel-tramp :straight t :defer t)

(use-package swiper
  :straight t
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
      (call-interactively #'swiper)))
  :custom-face
  (swiper-line-face ((t (:foreground nil :background "#292929" :height 110)))))

(use-package bookmark+
  :straight (bookmark+ :type git :host github :repo "emacsmirror/bookmark-plus")
  :config
  (advice-add #'bookmark-jump :around
              (lambda (fn &rest args)
                "Push point to the marker-stack before jumping to bookmark."
                (let ((pm (point-marker)))
                  (apply fn args)
                  (xref-push-marker-stack pm)))))

(use-package flyspell-correct
  :straight t
  :after flyspell
  :bind
  (:map flyspell-mode-map
        ("C-," . flyspell-correct-at-point))
  :commands flyspell-correct-at-point)

(use-package synosaurus
  :straight t
  :config
  (setq synosaurus-backend #'synosaurus-backend-wordnet)

  (defun jens/synosaurus-word-at-point ()
    "Suggest synonyms for the word at point, using `wordnet' as a thesaurus."
    (interactive)
    (let* ((word (thing-at-point 'word))
           (bounds (bounds-of-thing-at-point 'word))
           (synonyms (when word (-uniq (-flatten (funcall synosaurus-backend word)))))
           (pick))
      (setq pick
            (cond
             ((not word) (message "No word at point"))
             ((not synonyms) (message "No synonyms found for '%s'" word))
             (t (completing-read "Synonyms: " synonyms))))
      (when pick
        (save-mark-and-excursion
          (delete-region (car bounds) (cdr bounds))
          (goto-char (car bounds))
          (insert pick))))))

(use-package so-long
  :straight (so-long :type git :repo "https://git.savannah.gnu.org/git/so-long.git/")
  :demand t
  :commands so-long-enable
  :config
  (setq so-long-threshold 500)
  (so-long-enable))

(use-package treemacs
  :straight t
  :defer t
  :config
  (treemacs-resize-icons 15))

(use-package posframe
  :straight t
  :demand t
  :config
  (setq posframe-mouse-banish nil))

(use-package eros
  :straight t
  :demand t
  :hook (emacs-lisp-mode . eros-mode)
  :config
  (eros-mode +1))

;;;; auto completion

(use-package company
  :straight t
  :defer t
  :diminish company-mode
  :hook (emacs-lisp-mode . company-mode)
  :bind
  (("<backtab>" . #'completion-at-point)
   ("M-<tab>" . #'jens/complete)
   ("C-<tab>" . #'company-complete))
  :config
  (setq company-search-regexp-function 'company-search-flex-regexp)
  (setq company-require-match nil)

  (setq company-backends
        '(company-elisp
          company-semantic
          company-clang
          company-cmake
          company-capf
          company-files
          (company-dabbrev-code company-gtags company-etags company-keywords)
          company-dabbrev))

  ;; don't show the company menu automatically
  (setq company-begin-commands nil)

  (defun jens/complete ()
    "Show company completions using ivy."
    (interactive)
    (unless company-candidates
      (let ((company-frontends nil))
        (company-complete)))

    (when-let ((prefix (symbol-name (symbol-at-point)))
               (bounds (bounds-of-thing-at-point 'symbol)))
      (when company-candidates
        (when-let ((pick
                    (ivy-read "complete: " company-candidates
                              :initial-input prefix)))

          ;; replace the candidate with the pick
          (delete-region (car bounds) (cdr bounds))
          (insert pick))))))

(use-package company-lsp
  :straight t
  :defer t
  :config
  (push 'company-lsp company-backends))

(use-package company-flx
  :straight t
  :hook (company-mode . company-flx-mode))

(use-package company-c-headers
  :straight t
  :defer t
  :config
  (setq c++-include-files
        '("/usr/include/"
          "/usr/include/c++/8.2.1/"
          "/usr/lib/clang/7.0.1/include/"
          "/usr/lib/gcc/x86_64-pc-linux-gnu/8.2.1/include/"
          "/usr/lib/gcc/x86_64-pc-linux-gnu/8.2.1/include-fixed/"))

  (setq company-c-headers-path-system
        (-uniq (-concat c++-include-files company-c-headers-path-system)))

  (add-to-list 'company-backends 'company-c-headers))

;;; keybindings

(log-info "Setting keybindings")

;; keys for quickly going to common files
(bind-key* "\e\ei" (xi (find-file "~/vault/git/org/inbox.org")))
(bind-key* "\e\ek" (xi (find-file "~/vault/git/org/tracking.org")))
(bind-key* "\e\em" (xi (find-file "~/vault/git/org/roadmap.org")))
(bind-key* "\e\et" (xi (find-file "~/vault/git/org/today.org")))
(bind-key* "\e\er" (xi (find-file "~/vault/git/org/read.org")))
(bind-key* "\e\ew" (xi (find-file "~/vault/git/org/watch.org")))
(bind-key* "\e\ec" (xi (find-file "~/.emacs.d/init.el")))

;;;; for built-in things

;; handle special keys
(define-key key-translation-map [S-dead-circumflex] "^")
(define-key key-translation-map [dead-tilde] "~")
(define-key key-translation-map [S-dead-grave] "´")
(define-key key-translation-map [dead-acute] "`")
(define-key key-translation-map [dead-diaeresis] "¨")

;; Insert tilde with a single keystroke
(global-set-key (kbd "<menu>") (xi (insert "~")))

;; Easily mark the entire buffer
(bind-key* "C-x a" 'mark-whole-buffer)

;; Quit emacs, mnemonic is C-x REALLY QUIT
(bind-key* "C-x r q" 'save-buffers-kill-terminal)
;; Kill emacs, mnemonic is C-x REALLY KILL
(bind-key* "C-x r k" 'save-buffers-kill-emacs)

(bind-key* "C-c C-SPC" #'pop-to-mark-command)

;; don't close emacs
(unbind-key "C-x C-c")

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

(global-set-key (kbd "C-M-n") #'forward-paragraph)
(global-set-key (kbd "C-M-p") #'backward-paragraph)

(global-set-key (kbd "C-x b") 'ibuffer)

;; Scroll the buffer without moving the point (unless we over-move)
(bind-key*
 "C-<up>"
 (lambda ()
   (interactive)
   (scroll-down 5)))

(bind-key*
 "C-<down>"
 (lambda ()
   (interactive)
   (scroll-up 5)))

;; dont use the mouse
(unbind-key "<down-mouse-1>")
(unbind-key "<down-mouse-2>")
(unbind-key "<down-mouse-3>")
(unbind-key "C-<down-mouse-1>")
(unbind-key "C-<down-mouse-2>")
(unbind-key "C-<down-mouse-3>")
(unbind-key "S-<down-mouse-1>")
(unbind-key "S-<down-mouse-2>")
(unbind-key "S-<down-mouse-3>")
(unbind-key "M-<down-mouse-1>")
(unbind-key "M-<down-mouse-2>")
(unbind-key "M-<down-mouse-3>")

;; Disable suspend-frame
(unbind-key "C-x C-z")

;; Make Home and End go to the top and bottom of the buffer, we have C-a/e for lines
(bind-key* "<home>" 'beginning-of-buffer)
(bind-key* "<end>" 'end-of-buffer)

;; Completion that uses many different methods to find options.
(global-set-key (kbd "C-.") 'hippie-expand)

;;;; for homemade things

;; Better C-a
(bind-key* "C-a" 'jens/smart-beginning-of-line)

(bind-key* "M-<prior>" #'jens/sexp-up)
(bind-key* "M-<next>" #'jens/sexp-down)

;; Join lines (pull the below line up to this one, or the above one down)
(bind-key* "M-j" 'jens/join-region-or-line)
(bind-key* "M-J" 'jens/join-line-down)

;; Comment/uncomment block
(bind-key* "C-c c" 'jens/comment-uncomment-region-or-line)

;; Enable backwards killing of lines
(bind-key* "C-S-k" 'jens/kill-to-beginning-of-line)

;; Force save a file, mnemonic is C-x TOUCH
(global-set-key (kbd "C-x C-t") 'jens/touch-buffer-file)

;; Copy current line / region
(bind-key* "M-w" 'jens/save-region-or-current-line)
(bind-key* "C-w" 'jens/kill-region-or-current-line)
(bind-key* "C-M-w" 'jens/clean-current-line)

;; jump between indentation levels
(bind-key* "s-n" 'jens/goto-next-line-with-same-indentation)
(bind-key* "s-p" 'jens/goto-prev-line-with-same-indentation)

(bind-key* "C-M-k" #'jens/copy-sexp-at-point)
(bind-key* "M-K" #'jens/kill-sexp-at-point)

(bind-key* "M-r" #'jens/goto-repo)

(bind-key "t" #'jens/tail-message-buffer messages-buffer-mode-map)

(global-set-key (kbd "<f12>") #'jens/inspect-variable-at-point)

(global-set-key (kbd "C-<escape>") #'jens/shortcut/body)

(bind-key "C-x <left>" #'jens/buffer-carousel-previous)
(bind-key "C-x <right>" #'jens/buffer-carousel-next)

;;; epilogue

(log-success (format "Configuration is %s lines of emacs-lisp. (excluding 3rd-parties)"
                     (jens/emacs-init-loc)))
(log-success (format "Initialized in %s, with %s garbage collections.\n"
                     (emacs-init-time) gcs-done))

(defun jens/show-initial-important-messages ()
  "Show all lines in *Messages* matching a regex for important messages."
  (let* ((regex
          (rx (or
               ;; show info about loaded files with auto-save data
               "recover-this-file"
               ;; show warning messages that occurred during init
               (group bol "!")
               ;; lines containing the word `warning'
               (group bol (0+ any) "warning" (0+ any) eol)
               (group bol (0+ any) "error" (0+ any) eol))))
         (messages (with-current-buffer "*Messages*" (buffer-string)))
         (important
          (with-temp-buffer
            (insert messages)
            (delete-non-matching-lines regex (point-min) (point-max))
            (buffer-string))))
    (when (> (length important) 0)
      (with-current-buffer (get-buffer-create "*Important Messages*")
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert important)
          (view-buffer-other-window (current-buffer))))))
  ;; only show the messages buffer in the first frame created
  (remove-hook #'server-after-make-frame-hook #'jens/show-initial-important-messages))

(add-hook #'server-after-make-frame-hook #'jens/show-initial-important-messages)

;; TODO: get buffer string without `with-current-buffer', to speed things up?
;; TODO: replace `magithub' with `forge'
;; TODO: colorize message/compile buffer
;; TODO: add separator in compile mode
;; TODO: test with ert?

(provide 'init)
;;; init.el ends here
