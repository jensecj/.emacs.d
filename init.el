;;; -*- lexical-binding: t -*-

;;; prelude
;;;; early setup

;; TODO: look into (require 'message), and if it breaks #'message

;; some functions for logging
(defmacro log-info (txt &rest args)    `(message ,(concat "\n" txt) ,@args))
(defmacro log-warning (txt &rest args) `(message ,(concat "~ "  txt) ,@args))
(defmacro log-error (txt &rest args)   `(message ,(concat "! "  txt) ,@args))
(defmacro log-success (txt &rest args) `(message ,(concat "@ "  txt) ,@args))

(call-interactively #'emacs-version)
(message "Commit: %s (%s)" emacs-repository-version emacs-repository-branch)

(log-info "Doing early initialization")

;; user directories/files
(defconst user-home-directory (getenv "HOME"))
(defconst user-elpa-directory (locate-user-emacs-file "elpa/"))
(defconst user-straight-directory (locate-user-emacs-file "straight/"))
(defconst user-lisp-directory (locate-user-emacs-file "lisp/"))
(defconst user-vendor-directory (locate-user-emacs-file "vendor/"))
(defconst user-secrets-file (locate-user-emacs-file "secrets.el"))

(defconst user-mail-directory "~/private/mail")
(defconst user-contacts-files '("~/vault/contacts.org.gpg"))

;; add user directories to the load-path
(add-to-list 'load-path user-lisp-directory)
(add-to-list 'load-path user-elpa-directory)
(add-to-list 'load-path user-vendor-directory)

;; setup package archives
(setq package-archives
      '(("elpa" . "https://elpa.gnu.org/packages/")
        ("nongnu-elpa" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/") ;; TODO: remove this after org 9.5, since it's moving to NonGNU elpa
        ))

;; setup fonts
;; a bit convoluted because we also want the first frame
;; spawned by the daemon to use the face.
(defun jens/init-fonts ()
  "Setup font configuration for new frames."
  (let ((my-font "Source Code Pro Semibold 11"))
    (if (not (find-font (font-spec :name my-font)))
        (log-warning "could not find font: %s" my-font)
      (add-to-list 'default-frame-alist `(font . ,my-font))
      (set-frame-font my-font))

    ;; only setup fonts once
    (remove-hook 'server-after-make-frame-hook #'jens/init-fonts)))

(if (daemonp)
    (add-hook 'server-after-make-frame-hook #'jens/init-fonts)
  (jens/init-fonts))

(defvar idle-with-no-active-frames-functions '())
(defun jens/idle-with-no-active-frames ()
  (when (and (server-running-p)
             (string= (map-elt (frame-parameters (selected-frame)) 'name) "F1"))
    (run-hooks 'idle-with-no-active-frames-functions)))

(run-with-idle-timer 30 t #'jens/idle-with-no-active-frames)

;;;; fundamental third-party packages

(log-info "Loading fundamental third-party packages")

(message "bootstrapping straight.el...")

(setq straight-vc-git-default-protocol 'ssh)
(setq straight-check-for-modifications '(check-on-save find-when-checking))

(message "bootstrapping use-package...")
(defvar bootstrap-version)
(let ((bootstrap-file (locate-user-emacs-file "straight/repos/straight.el/bootstrap.el"))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (log-info "straight.el was not found, installing.")
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

;; need to enable imenu support before requiring `use-package'
(setq use-package-enable-imenu-support t)
;; make use-package tell us what its doing
(setq use-package-verbose t)
(setq use-package-compute-statistics t)
(setq use-package-minimum-reported-time 0.01)

;; needs to be required after its settings are set
(require 'use-package)

(message "loading common elisp libraries...")

(use-package subr-x :demand t)
(use-package cl-lib :demand t)
(use-package mode-local :demand t)

(use-package contrib :demand t) ; the mailing list is against a lot of these, but I think they're nifty

(use-package dash ;; working with lists, -map, -concat, etc
  :demand t
  :straight (dash :host github :repo "magnars/dash.el"
                  :fork (:host github :repo "jensecj/dash.el"))
  :config
  (defun -flatmap (f list)
    (-flatten (-map f list)))

  (defun -mapcar (f list)
    (-map f (-map #'car list)))

  (defun -mapcdr (f list)
    (-map f (-map #'cdr list)))

  (defun -mapply (fn atom-or-list)
    "If ATOM-OR-LIST is a list, map FN over it, otherwise apply FN."
    (cond
     ((listp atom-or-list) (-map fn atom-or-list))
     (t (funcall fn atom-or-list))))
  )

(use-package s ;; string things, s-trim, s-replace, etc.
  :demand t
  :straight (s :host github :repo "magnars/s.el"
               :fork (:host github :repo "jensecj/s.el")))

(use-package f ;; file-things, f-exists-p, f-base, etc.
  :demand t
  :straight (f :host github :repo "rejeep/f.el"
               :fork (:host github :repo "jensecj/f.el")))

(use-package ht ;; great hash-table wrapper.
  :demand t
  :straight (ht :host github :repo "Wilfred/ht.el"
                :fork (:host github :repo "jensecj/ht.el")))

(use-package ts ;; time api
  :demand t
  :straight (ts :host github :repo "alphapapa/ts.el"))

(progn ;; add `:download' keyword to `use-package' to easily download files
  (defun use-package-normalize/:download (_name-symbol keyword args)
    (use-package-only-one (symbol-name keyword) args
      (lambda (_label arg)
        (cond
         ((stringp arg) arg)
         ((and (listp arg) (-all-p #'stringp arg)) arg)
         (t
          (use-package-error
           ":download wants a url (a string), or a list of urls"))))))

  (defun use-package-handler/:download (name _keyword one-or-more-urls rest state)
    (let ((byte-compile-verbose t))
      (cl-labels ((download-file (url dest)
                                 (let ((dir (f-dirname dest)))
                                   (when (not (f-exists-p dir))
                                     (f-mkdir dir))
                                   (with-demoted-errors "Error: %S"
                                     (url-copy-file url dest)
                                     (byte-compile-file dest))))
                  (handle-url (url)
                              (let* ((file (url-unhex-string (f-filename url)))
                                     (dir user-vendor-directory)
                                     (path (f-join dir file)))
                                (if (f-exists-p path)
                                    (let ((days (file-age path 'days)))
                                      (if (< days 100)
                                          (message "%s already exists, skipping download. (%s days old)" file days)
                                        (message "updating %s. (%s days old)" file days)
                                        (let* ((archive-date (file-age path 'date))
                                               (archive-name (format "%s--%s.%s" (f-base file) archive-date (f-ext file)))
                                               (archive-path (f-join dir archive-name)))
                                          (f-move path archive-path))
                                        (download-file url path)))
                                  (message "%s does not exist, downloading %s to %s" file url path)
                                  (download-file url path))
                                (use-package-concat
                                 (use-package-process-keywords name rest state)))))
        (if (stringp one-or-more-urls)
            (handle-url one-or-more-urls)
          (dolist (url one-or-more-urls)
            (handle-url url))))))

  (add-to-list 'use-package-keywords :download 'append))

(defun checksum-fn (fsym)
  "Return a md5 checksum of the function FSYM."
  (let* ((def (symbol-function fsym))
         (str (prin1-to-string def)))
    (md5 str)))

(defun checksum (sym)
  "Return a string md5 checksum of SYM."
  (cond
   ((functionp sym) (checksum-fn sym))
   ((symbolp sym) (md5 (symbol-value sym)))
   ((stringp sym) (md5 sym))
   (t (error "Dont know how to checksum %s (%s)" sym (type-of sym)))))

(defun checksum-at-point ()
  "Return the md5 checksum for the symbol at point."
  (interactive)
  (let ((hash (checksum (intern-soft (thing-at-point 'symbol)))))
    (kill-new hash)
    hash))

(use-package advice-patch
  :straight t
  :demand t
  :config
  (defun patch-add (name hash old new)
    (if (not (string= (checksum name) hash))
        (log-warning "%s changed definition, ignoring patch." name)
      (message "patching %s" name)
      (advice-patch name new old))))

(use-package bind-key :straight t :demand t)
(use-package diminish :straight t :demand t)
(use-package delight :straight t :demand t)
(use-package shut-up :straight t :demand t)
(use-package transient :straight t :demand t)
(use-package ov :straight t :demand t)
(use-package git :straight t :demand t)

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

;;;; themes

(use-package zent-theme
  :straight (zent-theme :type git :repo "git@github.com:jensecj/zent-theme.el.git")
  :demand t
  :functions zent
  :config
  (load-theme 'zent t))

;;;; cache, temp files, etc.

(log-info "Setting up cache / temp-files / etc.")

;; contain extra files in etc/ and var/.
;; load early, and overwrite locations in configs if needed.
(use-package no-littering :straight t :demand t)

(setq temporary-file-directory (no-littering-expand-var-file-name "temp/"))

(let ((auto-save-dir (no-littering-expand-var-file-name "auto-save/")))
  (setq auto-save-file-name-transforms `((".*" ,auto-save-dir t)))
  (setq auto-save-list-file-prefix auto-save-dir))

(setq auto-save-no-message t)

;; don't use the customize system, all settings are keps in this file
(setq custom-file null-device)

;; don't load default.el
(setq inhibit-default-init t)

;;;; fundamental defuns
;; TODO: move to contrib.el?
(log-info "Defining fundamental defuns")

(defmacro comment (&rest _) "Ignore everything inside this sexp." nil)

(defmacro xi (&rest body)
  "Convenience macro for creating no-argument interactive lambdas."
  `(lambda ()
     (interactive)
     ,@body))

(defun longest-list (&rest lists)
  "Return the longest list in LISTS."
  (-max-by
   (lambda (a b)
     (if (and (seqp a) (seqp b))
         (> (length a) (length b))
       1))
   lists))
;; (longest-list 'i '(1 2 3) 'z '(a b c) '(æ ø å))

(defun broadcast (&rest args)
  "Broadcast all elements in ARGS to be of equal length."
  (let ((max-width (length (apply #'longest-list args))))
    (-map-when
     (lambda (a) (atom a))
     (lambda (a) (-repeat max-width a))
     args)))
(broadcast 'a '(1 2 3) 'z '(a b c) '(æ ø å)); => ((a a a) (1 2 3) (z z z) (a b c) (æ ø å))

;; TODO: fix #'transpose, `-mapcar' is wrong in this context
(defun transpose (&rest lists)
  (apply #'-mapcar #'-list lists))

;; (transpose '(1 2 3) '(a b c)) ; => ((1 a) (2 b) (3 c))
;; (transpose '(1 a) '(2 b) '(3 c)) ; => ((1 2 3) (a b c))

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
  "Remove FNS from HOOKS."
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
  "Remove ELEMENT from the list at LIST-VAR."
  (let ((compare-fn (or compare-fn #'equal)))
    `(setf ,list-var (seq-remove (lambda (e) (,compare-fn ,element e)) ,list-var))))

(defun advice-nuke (sym)
  "Remove all advice from symbol SYM."
  (advice-mapc (lambda (advice _props) (advice-remove sym advice)) sym))

;;; built-in
;;;; settings

(log-info "Redefining built-in defaults")

;; location of emacs source files
(let ((src-dir (expand-file-name  "~/emacs/emacs/")))
  (if (f-exists-p src-dir)
      (setq source-directory src-dir)
    (log-warning "Unable to locate emacs source directory.")))

;; hide the splash screen
(setq inhibit-startup-message t)

;; don't disable function because they're confusing to beginners
(setq disabled-command-function nil)

;; recurse deeper by default, note: can be dangerous
(setq max-lisp-eval-depth 2000)

;; never use dialog boxes
(setq use-dialog-box nil)

;; load newer files, even if they have outdated byte-compiled counterparts
(setq load-prefer-newer t)

;; don't blink the cursor
(blink-cursor-mode -1)

;; don't try to scroll smoothly, it does not work
(pixel-scroll-mode -1)

;; allow pasting selection outside of Emacs
(setq select-enable-clipboard t)

;; show keystrokes in progress
(setq echo-keystrokes 0.1)

;; try to keep 4 lines on context when scrolling
(setq next-screen-context-lines 4)

;; add just a bit of linespacing
(setq-default line-spacing 0.08)

;; move files to trash when deleting
(setq delete-by-moving-to-trash t)

;; don't use shift to mark things
(setq shift-select-mode nil)

;; always display text left-to-right
(setq bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

;; fold characters in searches (e.g. 'a' matches 'â')
(setq search-default-mode 'char-fold-to-regexp)
(setq replace-char-fold t)

;; transparently open compressed files
(auto-compression-mode t)

;; enable syntax highlighting
(global-font-lock-mode t)

;; answering just 'y' or 'n' will do
(defalias 'yes-or-no-p 'y-or-n-p)
(setq use-short-answers t)

;; make re-centering more intuitive
(setq recenter-positions '(top middle bottom))
(bind-key* "C-l" #'recenter-top-bottom)
(bind-key* "C-S-l" #'move-to-window-line-top-bottom)

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

(setq-default fill-column 95)

;; show location of cursor in non-selected windows
(setq cursor-in-non-selected-windows t)

;; select the help window after spawning it
(setq help-window-select t)

;; show eldoc help for the thing at-point
(setq help-at-pt-display-when-idle t)

(setq idle-update-delay 0.5)

;; undo/redo window configuration with C-c <left>/<right>
(winner-mode 1)

;; use spaces instead of tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(electric-indent-mode +1)

;; show me empty lines after buffer end
(setq indicate-empty-lines t)

;; don't automatically break lines
(setq-default truncate-lines t)
(setq-default truncate-partial-width-windows t)


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

;; just keep all history
(setq history-length t)
(setq history-delete-duplicates t)

;; TODO: maybe set amalgamating-undo-limit?, see if it replaces `jens/pop-to-mark-command'
;; (setq amalgamating-undo-limit 50)

;; remember a lot of messages
(setq message-log-max 50000)

;; allow processes to read big chuncks of process output at a time
(setq read-process-output-max (* 1024 1024))
;; just read process output as-is
(setq process-adaptive-read-buffering nil)

;; try to show "tall-lines" (images, etc.) properly
(setq auto-window-vscroll t)

;; if moving the point more than 10 lines away,
;; center point in the middle of the window, otherwise be conservative.
(setq scroll-conservatively 10)

;; only scroll the current line when moving outside window-bounds
(setq auto-hscroll-mode 'current-line)

;; scroll faster, may cause temporary incorrect font-locking
(setq fast-but-imprecise-scrolling t)

;; save clipboard from other programs to kill-ring
(setq save-interprogram-paste-before-kill t)

;; always follow symlinks
(setq vc-follow-symlinks t)

;; just give me a clean scratch buffer
(setq initial-scratch-message nil)
(setq initial-major-mode 'emacs-lisp-mode)

;; timestamp messages in the *Warnings* buffer
(setq warning-prefix-function
      (lambda (_level entry)
        (insert (format-time-string "[%H:%M:%S] "))
        entry))

;; When popping the mark, continue popping until the cursor actually
;; moves. also, if the last command was a copy - skip past all the
;; expand-region cruft.
(defun jens/pop-to-mark-command (orig-fun &rest args)
  "Call ORIG-FUN until the cursor has moved.  Try popping up to 10 times."
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

(defun jens/ensure-read-only ()
  "Ensure that files opened from some common paths are read-only by default."
  (when-let* ((paths (list (expand-file-name "elpa/" user-emacs-directory)
                           (expand-file-name "straight/" user-emacs-directory)
                           (expand-file-name "vendor/" user-emacs-directory)
                           (expand-file-name "var/" user-emacs-directory)
                           (expand-file-name "etc/" user-emacs-directory)
                           (expand-file-name "emacs/emacs" user-home-directory)
                           (expand-file-name "emacs/build" user-home-directory)))
              (file buffer-file-name)
              (dir (f-dirname file)))
    (let ((includes '())
          (excludes '("\.git/" ".*-autoloads\\.el")))
      (when (or
             (-any? (lambda (p) (f-descendant-of-p dir p)) paths)
             (-any? (lambda (p) (string-match p file)) includes))
        (unless (-any (lambda (p) (string-match p file)) excludes)
          (read-only-mode +1))))))

(add-hook 'find-file-hook #'jens/ensure-read-only)

;;;;; authentication and security

;; set the paranoia level to medium, warns if connections are insecure
(setq network-security-level 'high)

(use-package auth-source
  :demand t
  :config
  (setq authinfo-mode)                  ; TODO: make sure reveal-mode hooks to the files
  (setq authinfo-hidden (rx (or "password" "secret")))
  (setq auth-sources (list (expand-file-name "~/vault/.authinfo.gpg")))
  (setq auth-source-save-behavior nil))

(use-package auth-source-pass
  :commands (get-secret auth-source-pass-enable auth-source-pass-get)
  :after auth-source
  :config
  (auth-source-pass-enable)

  (defun load-secrets ()
    (if (file-exists-p user-secrets-file)
        (with-temp-buffer
          (insert-file-contents user-secrets-file)
          (read (current-buffer)))
      (error "secrets file does not exist")))

  (defun get-secret (secret)
    (cond
     ((symbolp secret) (alist-get secret (load-secrets)))
     ((stringp secret) (auth-source-pass-get 'secret secret))
     (t (message "secret not found"))))

  (setq user-full-name (get-secret 'user-full-name))
  (setq user-mail-address (get-secret 'user-mail-address)))

(use-package epa-file
  :demand t
  :commands epa-file-enable
  :config
  (setq epg-pinentry-mode 'loopback)

  (shut-up (epa-file-enable))

  (defun gpg/key-in-cache-p (key)
    "Return whether KEY is in the GPG-agents cache."
    ;; TODO: check if key is in gpg-agent cache entirely in elisp
    (when-let* ((command (format "gpg-cached %s" key))
                (output (shell-command-to-string command)))
      (unless (or (string-empty-p output))
        (mapcar #'string-trim (split-string output)))))

  (defun gpg/cache-key (key)
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
      (process-send-string proc (format "%s\n" pw)))))

(use-package pinentry ;; enable GPG pinentry through the minibuffer
  :straight t
  :demand t
  :commands (pinentry-start pinentry-stop)
  :config
  (setenv "GPG_AGENT_INFO" nil)

  (defun pinentry/reset ()
    "Reset the `pinentry' service."
    (interactive)
    (pinentry-stop)
    (pinentry-start))

  (pinentry/reset)

  ;; need to reset the pinentry from time to time, otherwise it stops working?
  (setq pinentry/gpg-reset-timer (run-with-timer 0 (* 60 45) #'pinentry/reset)))

(defun jens/kill-idle-gpg-buffers ()
  "Kill .gpg buffers after they have not been used for 120 seconds."
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
              (cl-incf buffers-killed))))))
    (unless (zerop buffers-killed)
      (message "%s .gpg buffer(s) saved and killed" buffers-killed))))

(setq jens/kill-idle-gpg-buffers-timer (run-with-idle-timer 120 t #'jens/kill-idle-gpg-buffers))

;;;; packages

(log-info "Configuring built-in packages")

(use-package debug
  :config
  (setq debugger-stack-frame-as-list t))

(use-package edebug
  :config
  (defun edebug/instrument-last-sexp ()
    (interactive)
    (let ((edebug-all-defs t))
      (destructuring-bind (beg . end) (bounds-of-thing-at-point 'sexp)
        (eval-region beg end)))))

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
  (add-to-list 'interpreter-mode-alist '("bb" . clojure-mode))
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
              (cl-incf col)))

        (save-excursion
          ;; dont indent the terminating line of a block
          (skip-syntax-forward "-")
          (if (looking-at (rx (or ?\] ?\} ?\))))
              (cl-decf col)))

        (indent-line-to (* col tab-width)))))

  (setq-mode-local conf-space-mode indent-line-function #'conf-mode/indent))

(use-package doc-view
  :hook (doc-view-mode . auto-revert-mode)
  :defines doc-view-mode-hook
  :config
  (setq doc-view-resolution 300))

(use-package scheme
  :defer t
  :mode ("\\.scm\\'" . scheme-mode)
  :config)

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
  ("C-d" . nil)
  (:map java-mode-map
        ("C-c C-c" . projectile-compile-project))
  :config
  (dolist (k '("\M-," "\M-." "\M--" "\C-d"))
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
    (save-excursion
      (if line (forward-line line))
      (buffer-substring-no-properties
       (line-beginning-position)
       (line-end-position))))

  (defun make-mode/indent ()
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

(use-package hl-line
  :config
  (setq global-hl-line-sticky-flag t)
  (global-hl-line-mode +1))

(use-package ibuffer
  :bind ("C-x b" . ibuffer)
  :config
  (setq ibuffer-movement-cycle nil)
  (setq ibuffer-formats
        '((mark modified read-only " "
                (name 60 -1 :left) " "
                (filename-and-process 70 -1))
          (mark " " (name 16 -1) " " filename))))

(use-package bookmark
  :config
  (advice-add #'bookmark-jump :before
              (lambda (&rest _)
                "Push point to the marker-stack before jumping to bookmark."
                (xref-push-marker-stack (point-marker)))))

(use-package dired
  :defer t
  :commands (dired dired/toggle-mark)
  :bind
  (("C-x C-d" . dired-jump)
   :map dired-mode-map
   ("<backspace>" . dired-up-directory)
   ("<prior>" . dired/previous-5)
   ("<next>" . dired/next-5)
   ("c" . dired-do-copy)
   ("C-." . dired-omit-mode)
   ("SPC" . dired/toggle-mark)
   ("C-+" . dired-create-empty-file))
  :config
  ;; pull in extra functionality for dired
  (load-library "dired-x")
  (load-library "dired-aux")

  (defun dired/always-hide-details-information ()
    "Always hide the top information line in dired buffers."
    (add-to-invisibility-spec 'dired-hide-details-information))

  (advice-add #'dired-hide-details-update-invisibility-spec
              :after
              #'dired/always-hide-details-information)

  (add-hook* '(dired-mode-hook wdired-mode-hook) #'dired/always-hide-details-information)

  (setq dired-kill-when-opening-new-dired-buffer t)
  (setq dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\..+$\\|^\\..+$")
  (setq dired-listing-switches "-AghoXN --group-directories-first")
  (setq dired-create-destination-dirs 'always)
  (setq dired-hide-details-hide-symlink-targets nil)
  (setq dired-dwim-target t)
  (setq dired-mark-region t)

  ;; always delete and copy recursively
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies 'always)

  ;; use bigger fringes in dired-mode
  (add-hook 'dired-mode-hook (lambda () (setq left-fringe-width 10)))

  (defun dired/toggle-mark (arg)
    "Toggle mark on the current line."
    (interactive "P")
    (let ((this-line (buffer-substring (line-beginning-position) (line-end-position))))
      (if (s-matches-p (dired-marker-regexp) this-line)
          (dired-unmark arg)
        (dired-mark arg))))

  (defun dired/next-5 ()
    "Move up 5 lines."
    (interactive)
    (dired-next-line 5))

  (defun dired/previous-5 ()
    "Move down 5 lines."
    (interactive)
    (dired-next-line -5))

  (defun dired/wrap-move (fn &rest args)
    "Wrap around when moving point outside of top / bottom of dired buffer."
    (let ((arg (car args)))
      (cond
       ((and (< arg 0) (bobp)) (goto-char (point-max)))
       ((and (> arg 0) (eobp)) (goto-char (point-min)))
       (t (apply fn args)))))

  (advice-add #'dired-next-line :around #'dired/wrap-move))

(use-package wdired
  :config
  (setq wdired-allow-to-redirect-links t)
  (setq wdired-allow-to-change-permissions t))

(use-package ediff
  :defer t
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally))

(use-package diff
  :config
  ;;files are diffed with `diff old new', why apply patches in reverse?
  (setq diff-jump-to-old-file t)
  (setq diff-default-read-only t)
  (setq diff-font-lock-prettify t)

  (defun diff/-get-file-strings ()
    (let ((files))
      (save-excursion
        (goto-line 3)
        (push (buffer-substring-no-properties (line-beginning-position) (line-end-position)) files)
        (goto-line 2)
        (push (buffer-substring-no-properties (line-beginning-position) (line-end-position)) files))
      files))

  (defun diff/-extract-file-from-file-string (s)
    (nth 1
         (s-match (rx (or "---" "+++") (1+ space)
                      (group (1+ any)) (1+ space)
                      (= 4 num) "-" (= 2 num) "-" (= 2 num) (1+ space)
                      (= 2 num) ":" (= 2 num) ":" (= 2 num) (optional "." (1+ num)) (1+ space)
                      (or "-" "+") (1+ num))
                  s)))

  (defun diff/get-files ()
    (let ((files (diff/-get-file-strings)))
      (-map #'diff/-extract-file-from-file-string files)))

  (defun diff-fullscreen (old new)
    "Like `diff', but hide delete other windows, fullscreening the diff buffer.

Works well being called from a terminal:
`emacsclient -c --eval '(diff-fullscreen \"oldfile.txt\" \"newfile.txt\")'"
    (interactive)
    (delete-other-windows (diff old new))))

(use-package url
  :config
  ;; don't store cookies.
  (setq url-cookie-file nil)
  (setq url-cookie-trusted-urls nil)
  (setq url-cookie-confirmation nil))

(use-package shr
  :defer t
  :config
  (setq shr-use-colors nil)
  (setq shr-use-fonts nil)
  (setq shr-width fill-column)
  (setq shr-discard-aria-hidden t)
  (setq shr-cookie-policy nil))

(use-package browse-url
  :defer t
  :bind
  ("C-c C-o" . #'browse-url/open-url-at-point)
  :config
  (setq browse-url-firefox-program "firefox-developer-edition")

  (defun browse-url/get-url-at-point ()
    "Return the url at point, if any."
    (or
     (thing-at-point 'url t)
     (shr-url-at-point nil)
     (org-extra-url-at-point)
     (if (fboundp 'augment-at-point) (augment-at-point)) ;url from augment
     ;; bug-links from bug-reference-mode
     (-any (lambda (o) (overlay-get o 'bug-reference-url)) (overlays-at (point)))))

  (defun browse-url/open-url-at-point ()
    "Open the URL-at-point, dwim."
    (interactive)
    (browse-url (browse-url/get-url-at-point))))

(use-package eww
  :defer t
  :bind (("M-s M-o" . #'eww/open-url-at-point)
         ("M-s M-w" . #'eww/search-region)
         ("M-s M-d" . #'eww/download-url-at-point)
         ("M-s M-D" . #'eww/delete-cookies)
         ("M-s M-s" . #'eww-list-buffers)
         ("M-s M-c" . #'url-cookie-list)
         :map eww-mode-map
         ("q" . #'eww/quit)
         ("g" . #'eww/reload))
  :config
  (require 'browse-url)

  (setq eww-browse-url-new-window-is-tab nil)
  (setq eww-download-directory (expand-file-name "~/downloads"))

  (defun eww/reload ()
    (interactive)
    (let ((p (point))
          (ws (window-start)))
      (add-hook 'eww-after-render-hook
                (defun eww/restore ()
                  (goto-char p)
                  (set-window-start (selected-window) ws)
                  (remove-hook 'eww-after-render-hook #'eww/restore)))
      (eww-reload)))

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
    (when-let ((url (or (car (eww-suggested-uris)) (browse-url/get-url-at-point)))
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
    (url-cookie-delete-cookies))

  (defun eww/quit ()
    (interactive)
    (eww/delete-cookies)
    (quit-window)))

(use-package minibuffer
  :bind ("C-<tab>" . #'completion-at-point)
  :config
  (setq enable-recursive-minibuffers t)
  (setq read-file-name-completion-ignore-case t)
  (setq read-buffer-completion-ignore-case t)

  ;; do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))

(use-package occur-mode
  :bind
  ("C-c C-q" . occur-edit-mode))

;;;;; minor modes

(use-package hi-lock :diminish hi-lock-mode)

(use-package xref
  :config
  (setq xref-search-program 'ripgrep))

(use-package flymake
  :config
  (setq flymake-fringe-indicator-position 'right-fringe))

(use-package outline
  :diminish outline-minor-mode
  :config
  (setq outline-blank-line t)
  (setq outline-minor-mode-highlight t)
  (setq outline-minor-mode-cycle t))

(use-package display-fill-column-indicator
  :hook ((text-mode prog-mode) . display-fill-column-indicator-mode))

(use-package so-long
  :demand t
  :config
  (setq so-long-threshold 500)

  (add-to-list* 'so-long-minor-modes
                '(highlight-thing-mode
                  rainbow-mode
                  highlight-numbers-mode
                  augment-mode
                  augment-prog-mode)
                'append)

  (add-to-list* 'so-long-variable-overrides
                '((global-augment-mode . nil))
                'append)

  (global-so-long-mode +1))

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
  :diminish whitespace-mode
  :config
  (setq whitespace-style '(face empty trailing))
  (setq-mode-local text-mode show-trailing-whitespace t)
  (setq-mode-local prog-mode show-trailing-whitespace t)

  (defun whitespace/collapse-newlines-in-buffer ()
    "Collapses multiple occurances of newlines in the current buffer."
    (interactive)
    (save-excursion
      (goto-char (point-max))
      (while (re-search-backward "\n\n\n" nil 'noerror)
        (replace-match "\n\n")))))

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
  (show-paren-mode +1))

(use-package abbrev ;; auto-replace common abbreviations
  :diminish abbrev-mode
  :hook ((text-mode
          circe-chat-mode
          git-commit-mode) . abbrev-mode)
  :config
  (read-abbrev-file)
  (abbrev-mode +1))

(use-package subword ;; easily navigate silly cased words
  :demand t
  :diminish subword-mode
  :config
  (global-subword-mode +1))

(use-package windmove
  ;; select windows with S-<arrow>
  :config
  (windmove-default-keybindings 'shift)

  (defun windmove/silent-select (orig &rest args)
    "Windmove uses user-errors to flood the echo area, silence them"
    (ignore-errors (apply orig args)))

  (advice-add #'windmove-do-window-select :around #'windmove/silent-select))

(use-package saveplace
  ;; save point position between sessions
  :demand t
  :config
  (save-place-mode +1))

(use-package savehist
  ;; persist some variables between sessions
  :demand t
  :config
  (setq savehist-save-minibuffer-history t)
  (setq savehist-autosave-interval 60)

  (add-to-list* 'savehist-additional-variables
                '(kill-ring
                  search-ring
                  regexp-search-ring))

  (savehist-mode +1))

(use-package autorevert
  ;; always show the version of a file as it appears on disk
  :demand t
  :config
  ;; auto revert applicable buffers
  (setq global-auto-revert-non-file-buffers t)
  (setq auto-revert-verbose nil)

  ;; just revert pdf files without asking
  (setq revert-without-query '("\\.pdf")))

(use-package smerge-mode ;; easily handle merge conflicts
  :bind
  (:map smerge-mode-map ("C-c ^" . transient-smerge))
  :config
  (transient-define-prefix transient-smerge ()
    "Most commonly used window commands"
    :transient-non-suffix 'transient--do-call
    ["Commands"
     [("n" "next" smerge-next)
      ("p" "previous" smerge-prev)
      ("u" "keep upper" smerge-keep-upper)
      ("l" "keep lower" smerge-keep-lower)]])

  (defun smerge/enable-if-diff-buffer ()
    "Enable Smerge-mode if the current buffer is showing a diff."
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^<<<<<<< " nil t)
        (smerge-mode +1))))

  (add-hook* '(find-file-hook after-revert-hook) #'smerge/enable-if-diff-buffer))

(use-package eldoc
  ;; show useful contextual information in the echo-area
  :diminish eldoc-mode
  :commands (easy-eldoc)
  :config
  (setq eldoc-echo-area-display-truncation-message nil)
  (setq eldoc-idle-delay 0.2)

  (defmacro easy-eldoc (hook fn)
    "Add FN to `eldoc-documentation-function' in HOOK."
    (let ((name (intern (format "easy-eldoc-%s-%s" (symbol-name hook) (symbol-name fn)))))
      `(progn
         (defun ,name ()
           (set (make-local-variable 'eldoc-documentation-function) #',fn))
         (add-hook ',hook #',name))))

  (defun eldoc/highlight-&s (doc)
    "Highlight &keywords in elisp eldoc arglists."
    (condition-case nil
        (with-temp-buffer
          (insert doc)
          (goto-char (point-min))

          (while (re-search-forward "&optional\\|&rest\\|&key" nil 't)
            (set-text-properties (match-beginning 0) (match-end 0) (list 'face 'font-lock-preprocessor-face)))

          (buffer-string))
      (error doc)))

  (advice-add #'elisp-get-fnsym-args-string :filter-return #'eldoc/highlight-&s)

  (defun eldoc/add-short-doc (orig &rest args)
    "Change the format of eldoc messages for functions to `(fn args)'."
    (shut-up
      (save-window-excursion
        (save-mark-and-excursion
          (let ((sym (car args))
                (eldoc-args (apply orig args)))
            (let* ((doc (and (fboundp sym) (documentation sym 'raw)))
                   (short-doc (when doc (substring doc 0 (string-match "\n" doc))))
                   (short-doc (when short-doc (propertize short-doc 'face font-lock-comment-face))))
              (format "%s\t\t%s" (or eldoc-args "") (or short-doc ""))))))))

  (eldoc/add-short-doc #'elisp-get-fnsym-args-string 'insert)

  (advice-add #'elisp-get-fnsym-args-string :around #'eldoc/add-short-doc)

  (global-eldoc-mode +1))

(use-package fringe
  :commands fringe-mode
  :config
  (define-fringe-bitmap 'empty-fringe-bitmap [0] 1 1 'center)
  (setf (alist-get 'truncation fringe-indicator-alist)
        '(empty-line empty-line))
  (setf (alist-get 'continuation fringe-indicator-alist)
        '(vertical-bar vertical-bar))

  (set-fringe-mode '(4 . 4)))

;;;;; misc packages

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

(use-package uniquify
  ;; give buffers unique names
  :demand t
  :config (setq uniquify-buffer-name-style 'forward))

(use-package tramp
  ;; easily access and edit files on remote machines
  :defer t
  :config
  (setq tramp-verbose 6)
  (setq tramp-default-method "ssh")

  (defun tramp/get-method-parameter (method param)
    "Return the method parameter PARAM.
If the `tramp-methods' entry does not exist, return NIL."
    (when-let ((entry (assoc param (assoc method tramp-methods))))
      (cadr entry)))

  (defun tramp/set-method-parameter (method param newvalue)
    "Set the method paramter PARAM to VALUE for METHOD.

If METHOD does not yet have PARAM, add it.
If METHOD does not exist, do nothing."
    (let ((method-params (assoc method tramp-methods)))
      (when method-params
        (if-let ((entry (assoc param method-params)))
            (setcar (cdr entry) newvalue)
          (setcdr (last method-params) '(param newvalue)))))))

(use-package recentf
  ;; save a list of recently visited files.
  :demand t
  :commands (recentf-mode recentf/with-colors)
  :bind (("C-x f" . recentf/with-colors))
  :config
  (add-to-list* 'recentf-exclude
                `("/tmp/*"
                  ,(regexp-quote no-littering-var-directory)
                  ,(f-join user-mail-directory "*")
                  ,(regexp-quote temporary-file-directory)
                  ))

  ;; save a bunch of recent items
  (setq recentf-max-saved-items 1000)

  ;; clean the list every 5 minutes
  (setq recentf-auto-cleanup 300)

  (defun recentf/silent-save ()
    (shut-up (recentf-save-list)))

  ;; save recentf file every 30s, but don't bother us about it
  (run-with-idle-timer 30 t #'recentf/silent-save)

  ;; TODO: package these colorizers?
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

  (defun recentf/with-colors ()
    "Show list of recently visited files, colorized by type."
    (interactive)
    (let* ((recent-files (mapcar #'substring-no-properties recentf-list))
           (colorized-files (-map #'path-colorize recent-files)))
      (find-file
       (completing-read "recent files: " colorized-files nil t))))

  (defun recentf/cleanup (orig-fun &rest args)
    "Silence `recentf-auto-cleanup'."
    (shut-up (apply orig-fun args)))

  (advice-add #'recentf-cleanup :around #'recentf/cleanup)
  (recentf-mode +1))

(use-package replace
  :defer t
  :bind
  (("C-c r" . replace/replace)
   ("C-c q" . replace/query-replace))
  :config
  (defun replace/--replace (fn)
    "Get replace arguments and delegate to replace FN."
    (let ((from (if (use-region-p)
                    (buffer-substring (region-beginning) (region-end))
                  (completing-read "Replace: " '())))
          (to (completing-read "Replace with: " '())))
      (deactivate-mark)
      (save-excursion
        (funcall fn from to nil (point-min) (point-max)))))

  (defun replace/replace ()
    "Replace occurrence of regexp in the entire buffer."
    (interactive)
    (replace/--replace #'replace-regexp))

  (defun replace/query-replace ()
    "Interactively replace occurrence of regexp in the entire buffer."
    (interactive)
    (replace/--replace #'query-replace-regexp)))

(use-package compile
  :bind
  (("M-g M-n" . compile/next-error)
   ("M-g M-p" . compile/previous-error)
   ("M-g M-e" . compile/transient-goto-error))
  :config
  ;; don't keep asking for the commands
  (setq compilation-read-command nil)
  ;; stop scrolling compilation buffer when encountering an error
  (setq compilation-scroll-output 'first-error)
  ;; wrap lines
  (add-hook 'compilation-mode-hook #'visual-line-mode)

  (require 'ansi-color)
  (add-hook* '(compilation-filter-hook
               shell-mode-hook)
             #'ansi-color-for-comint-mode-on)

  (transient-define-prefix compile/transient-goto-error ()
    "Hydra for navigating between errors."
    ["Commands"
     [("n" #'next-error "next error")
      ("p" #'previous-error "previous error")]])

  (defun compile/next-error ()
    "Go to next error in buffer, and start goto-error-hydra."
    (interactive)
    (next-error)
    (compile/transient-goto-error))

  (defun compile/previous-error ()
    "Go to previous error in buffer, and start goto-error-hydra."
    (interactive)
    (previous-error)
    (compile/transient-goto-error)))

(use-package mml
  :config
  (setq mml-secure-smime-sign-with-sender t)
  (setq mml-secure-openpgp-sign-with-sender t))

(use-package sendmail
  :after notmuch
  :config
  (setq mail-specify-envelope-from t)
  (setq mail-envelope-from 'header)
  (setq sendmail-program "msmtp")

  ;; this belongs in `message.el', but using (use-package message) breaks #'message.
  (setq message-sendmail-envelope-from 'header)
  (setq message-sendmail-extra-arguments '("--read-envelope-from"))
  (setq message-sendmail-f-is-evil t)
  (setq message-fill-column fill-column)
  (setq mail-signature nil)
  (setq message-citation-line-function #'message-insert-formatted-citation-line)
  (setq message-citation-line-format "On %b %d, %Y, at %H:%M, %f wrote:\n")

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

  ;; make sendmail errors buffer read-only and view-mode
  ;; FIXME: error: "failed with exit value 78", because gpg-key not cached?
  ;; (setq message-send-mail-function 'async-sendmail-send-it)
  (setq message-send-mail-function 'sendmail-send-it))

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
  :straight org
  :pin org
  :demand t
  :commands (org-indent-region
             org-indent-line
             org-babel-do-load-languages
             org-context)
  :bind
  (
   :map org-mode-map
   ("M-<next>" . org-move-subtree-down)
   ("M-<prior>" . org-move-subtree-up)
   ("M-<right>" . org-demote-subtree)
   ("M-<left>" . org-promote-subtree)
   ("C-c C-q" . org/add-tag)
   ;; unbind things that are used for other things
   ([(tab)] . nil)
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
   ("C-c C-o" . nil))
  :init
  (setq org-modules '(org-bibtex org-docview org-info))
  :config
  ;;;;;;;;;;;;;;;;;;;;;;
  ;; general settings ;;
  ;;;;;;;;;;;;;;;;;;;;;;

  (setq org-startup-folded t)

  (setq org-archive-save-context-info '(time file olpath itags))

  (setq org-catch-invisible-edits 'show-and-error)
  (setq org-log-done 'time)

  ;; fontify src blocks
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)

  ;; keep #+BEGIN_SRC blocks aligned with their contents
  (setq org-edit-src-content-indentation 0)

  (setq org-use-speed-commands t)
  (setq org-adapt-indentation nil) ; don't indent things
  (setq org-tags-column 0)
  (setq org-tags-sort-function #'org-string-collate-lessp)
  (setq org-ellipsis "――")
  (setq org-cycle-separator-lines 1)    ; show single spaces between entries

  ;; don't move the point when using speed-commands...
  (add-to-list 'org-speed-commands
               '("o" . (lambda () (save-excursion (org-open-at-point)))))

  ;; don't expand org drawers on cycling
  (add-to-list 'org-cycle-hook #'org-cycle-hide-drawers)

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

  (when-let* ((ditaa-dir "/usr/share/java/ditaa/")
              (_ (f-exists-p ditaa-dir))
              (ditaa-jar
               (-as-> (f-entries ditaa-dir) es
                      (-filter (lambda (e)
                                 (and
                                  (string-prefix-p "ditaa" (f-filename e))
                                  (f-ext-p e "jar")))
                               es)
                      (car es))))
    (setq org-ditaa-jar-path ditaa-jar))

  ;; try to get non-fuzzy latex fragments
  (plist-put org-format-latex-options :scale 1)
  (setq org-preview-latex-default-process 'dvisvgm)

  (setq org-html-head
        (s-join " "
                '("<style type=\"text/css\">"
                  "body {max-width: 800px; margin: auto;}"
                  "img {max-width: 100%;}"
                  "</style>")))

  (setq org-outline-path-complete-in-steps t)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-refile-use-outline-path t)
  (setq org-refile-targets '((nil . (:maxlevel . 1))))

  ;;;;;;;;;;;;;;;
  ;; exporting ;;
  ;;;;;;;;;;;;;;;

  (setq org-export-use-babel nil)

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

  (defun org/complete-todo-entries (_ n-not-done)
    "Switch entry to DONE when all sub-entries are done, to TODO otherwise."
    (let (org-log-done org-log-states)   ; turn off logging
      (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

  (add-hook 'org-after-todo-statistics-hook #'org/complete-todo-entries)

  (defun org/fold-completed-todo-entries ()
    "Hide an entry when it is marked DONE."
    (when (string= org-state "DONE")
      (hide-subtree)))

  (add-hook 'org-after-todo-state-change-hook #'org/fold-completed-todo-entries)

  ;; `$ $' is a pair for latex-math in org-mode
  (defvar org-extra-electric-pairs '((?\$ . ?\$)))

  (setq-mode-local org-mode
                   electric-pair-pairs
                   (append org-extra-electric-pairs electric-pair-pairs))

  ;; update images after evaluation
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images)

  ;;;;;;;;;;;;;;;;;;;;;;
  ;; helper functions ;;
  ;;;;;;;;;;;;;;;;;;;;;;

  (defvar org-global-tags
    '(
      "academia" "ai" "devops" "books" "backup" "career" "china" "crypto" "privacy"
      "investing" "life" "climate" "cloud" "cicd" "databases" "microsoft" "esoteric"
      "economics" "postmortem" "deep_learning" "data_structures" "eu" "foss"
      "functional" "fintech" "visualization" "hardware" "history" "industry" "investing"
      "libre" "linux" "machine_learning" "math" "netsec" "networks" "philosophy" "plt"
      "politics" "programming" "research" "science" "software" "statistics" "startup"
      "systems_design" "tech" "usa" "webdev" "work" "java" "cpp" "finance" "biology" "mooc"
      "compsci" "golang" "clojure" "altweb"
      ))

  (defun org/add-tag ()
    "Tag current headline with available tags from the buffer"
    (interactive)
    (let* ((completions (-uniq (-concat (-flatten (org-get-buffer-tags))
                                        org-global-tags)))
           (tags (org-get-tags))
           (pick (completing-read (format "%s: " tags) completions)))
      (if (member pick tags)
          (setq tags (delete pick tags))
        (push pick tags))
      (save-excursion
        (org-back-to-heading t)
        (org-set-tags tags))))

  (defun org/toggle-babel-safe ()
    "Toggle whether it is safe to eval babel code blocks in the current buffer."
    (interactive)
    (set (make-variable-buffer-local 'org-confirm-babel-evaluate)
         (not org-confirm-babel-evaluate)))

  (defun org/format-buffer ()
    (interactive)
    (whitespace/collapse-newlines-in-buffer)

    (org-map-entries
     (lambda ()
       ;; insert blank line after headline, before entry content
       (let ((end (org-entry-end-position)))
         (forward-line)

         ;; skip planning lines
         (while (and (org-at-planning-p)
                     (< (point) (point-max)))
           (forward-line))

         ;; skip drawers. You might think that `org-at-drawer-p' would suffice, but
         ;; for some reason it doesn't work correctly when operating on hidden text.
         ;; This works, taken from `org-agenda-get-some-entry-text'.
         (while (re-search-forward org-drawer-regexp end t)
           (re-search-forward "^[ \t]*:END:.*\n?" end t)
           (goto-char (match-end 0)))

         ;; only insert a newline if it makes sense
         (unless (or (= (point) (point-max))
                     (org-at-heading-p)
                     (looking-at-p "\n"))
           (insert "\n"))))))

  (defun org/indent ()
    "Indent line or region in org-mode."
    (interactive)
    (if (region-active-p)
        (org-indent-region (region-beginning) (region-end)))
    (org-indent-line)
    (message "indented"))

  ;; syntax highlight org-mode code blocks when exporting as pdf
  ;; (setq-default org-latex-listings 'minted
  ;;               org-latex-packages-alist '(("" "minted"))
  ;;               org-latex-pdf-process
  ;;               '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
  ;;                 "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
  )

(use-package org-contrib
  :straight t
  :after org-mode)

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

  (defun org-contacts/get-contacts ()
    "Return all contacts from `org-contacts', in 'NAME <EMAIL>' format."
    (let ((data (-map #'caddr (org-contacts-db))))
      (-map
       (lambda (d)
         (let ((email (cdr (assoc-string org-contacts-email-property d)))
               (name (cdr (assoc-string "ITEM" d))))
           (format "%s <%s>" (or name "") (or email ""))))
       data)))

  (add-to-list 'notmuch-mojn-candidate-functions #'org-contacts/get-contacts))

(use-package org-agenda
  :defer t
  ;; TODO: have a look at https://github.com/alphapapa/org-super-agenda
  )

(use-package org-ql :straight t :defer t)

;;; homemade
;;;; defuns

(log-info "Defining homemade defuns")

;;;;; convenience

(defun jens/clone-buffer ()
  "Open a clone of the current buffer."
  (interactive)
  (let ((newbuf (buf-new-scratch-buffer))
        (content (buffer-string))
        (p (point)))
    (with-current-buffer newbuf
      (insert content))
    (switch-to-buffer-other-window newbuf)
    (goto-char p)))

(defun jens/sudo-find-file (filename)
  "Open FILENAME with superuser permissions."
  (let ((remote-method (file-remote-p default-directory 'method))
        (remote-user (file-remote-p default-directory 'user))
        (remote-host (file-remote-p default-directory 'host))
        (remote-localname (file-remote-p filename 'localname))
        (auth-sources '())) ;; HACK: don't query auth-source
    (find-file (format "/%s:root@%s:%s"
                       (or remote-method "sudoedit")
                       (or remote-host "localhost")
                       (or remote-localname filename)))))

(defun jens/sudo-edit ()
  "Re-open current buffer file with superuser permissions.
With prefix ARG, ask for file to open."
  (interactive)
  (if (or current-prefix-arg
          (not buffer-file-name))
      (read-file-name "Find file:"))

  (let ((place (point))
        (mode major-mode))
    (jens/sudo-find-file buffer-file-name)
    (goto-char place)
    (funcall mode)))

(defun jens/sudo-save ()
  "Save the current buffer using sudo through TRAMP."
  (interactive)
  (let ((contents (buffer-string))
        (p (point)))
    (jens/sudo-edit)
    (delete-region (point-min) (point-max))
    (goto-char (point-min))
    (insert contents)
    (goto-char p)
    (save-buffer)))

(defun jens/visible-links ()
  "Return a list of all links visible in the current window."
  (let ((links)
        (link-regex (regexp-opt thing-at-point-uri-schemes))
        (end (window-end)))
    (save-excursion
      (goto-char (window-start))
      (while (re-search-forward link-regex end t)
        (when-let ((url (thing-at-point 'url)))
          (push url links))))
    (-uniq links)))

(defun jens/open-links ()
  "Interactively open a link visible in the current window."
  (interactive)
  (when-let* ((links (jens/visible-links))
              (pick (completing-read "urls: " links nil t)))
    (browse-url pick)))
(bind-key "C-x C-l" #'jens/open-links)

;;;;; files

(defun byte-compile-this-file ()
  "Byte compile the current buffer."
  (interactive)
  (if (f-exists? (concat (f-no-ext (buffer-file-name)) ".elc"))
      (byte-recompile-file (buffer-file-name))
    (byte-compile-file (buffer-file-name))))

(defun rename-this-file ()
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

(defun delete-this-file ()
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

(defun touch-this-file ()
  "Touches the current buffer, marking it as dirty."
  (interactive)
  (insert " ")
  (backward-delete-char 1)
  (save-buffer))
(bind-key* "C-x C-t" 'touch-this-file)

(defun reopen-this-file ()
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

(defun jens/eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))
(bind-key* "M-S-a" #'jens/eval-and-replace)

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
(bind-key* "C-a" 'jens/smart-beginning-of-line)

(defun jens/kill-to-beginning-of-line ()
  "Kill from <point> to the beginning of the current line."
  (interactive)
  (kill-region (save-excursion (beginning-of-line) (point))
               (point)))
(bind-key* "C-S-k" 'jens/kill-to-beginning-of-line)

(defun jens/save-region-or-current-line (_arg)
  "If a region is active then it is saved to the `kill-ring',
otherwise the current line is saved."
  (interactive "P")
  (save-mark-and-excursion
    (if (region-active-p)
        (kill-ring-save (region-beginning) (region-end))
      (kill-ring-save (line-beginning-position) (line-end-position)))))
(bind-key* "M-w" 'jens/save-region-or-current-line)

(defun jens/kill-region-or-current-line ()
  "If a region is active then it is killed, otherwise the current
line is killed."
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (save-excursion
      (kill-whole-line))))
(bind-key* "C-w" 'jens/kill-region-or-current-line)

(defun jens/clean-current-line ()
  "Delete the contents of the current line."
  (interactive)
  (delete-region (line-beginning-position) (line-end-position)))
(bind-key* "C-M-w" 'jens/clean-current-line)

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
(bind-key* "M-j" 'jens/join-region-or-line)

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
(bind-key* "M-J" 'jens/join-line-down)

(defun jens/comment-uncomment-region-or-line ()
  "If region is active, comment or uncomment it (based on what it
currently is), otherwise comment or uncomment the current line."
  (interactive)
  (if (region-active-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (line-beginning-position) (line-end-position))))
(bind-key* "C-c c" 'jens/comment-uncomment-region-or-line)

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
(bind-key* "s-n" 'jens/goto-next-line-with-same-indentation)

(defun jens/goto-prev-line-with-same-indentation ()
  "Jump to a previous line with the same indentation level as the
current line."
  (interactive)
  (back-to-indentation)
  (re-search-backward
   (s-concat "^" (s-repeat (current-column) " ") "[^ \t\r\n\v\f]"))
  (back-to-indentation))
(bind-key* "s-p" #'jens/goto-prev-line-with-same-indentation)

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
(bind-key* "M-r" #'jens/goto-repo)

(transient-define-prefix jens/shortcut ()
  :transient-non-suffix 'transient--do-call
  ["Shortcuts"
   [("m" "*messages*" jens/goto-msg-buffer)
    ("n" "notmuch" notmuch-mojn)
    ("e" "elfeed" elfeed)
    ("c" "new *scratch*" buf-jump-to-empty-scratch-buffer)]])
(bind-key "C-<escape>" #'jens/shortcut)

(defun jens/buffer-carousel-previous ()
  (interactive)
  (previous-buffer)
  (jens/buffer-carousel))
(bind-key "C-x <left>" #'jens/buffer-carousel-previous)

(defun jens/buffer-carousel-next ()
  (interactive)
  (next-buffer)
  (jens/buffer-carousel))
(bind-key "C-x <right>" #'jens/buffer-carousel-next)

(transient-define-prefix jens/buffer-carousel ()
  "Move between buffers."
  ["Commands"
   [("<left>" "previous" previous-buffer :transient t)
    ("<right>" "next" next-buffer :transient t)]])

;; TODO: make tail-buffer work for the buffer its called in, not just *Messages*
(defun jens/tail-message-buffer ()
  "Toggle tailing the *Message* buffer every time something is written to it."
  (interactive)
  (unless (fboundp 'tmb/message-buffer-goto-end)
    (defun tmb/message-buffer-goto-end (&rest args)
      (dolist (w (get-buffer-window-list "*Messages*"))
        (with-selected-window w
          (set-window-point w (point-max))))
      args))

  (unless (boundp 'tail-message-buffer-mode)
    (define-minor-mode tail-message-buffer-mode
      "Tail the *Messages* buffer every time something calls `message'."
      :lighter " tail"
      :keymap (make-keymap)
      (if (bound-and-true-p tail-message-buffer-mode)
          (advice-add #'message :after #'tmb/message-buffer-goto-end)
        (advice-remove #'message #'tmb/message-buffer-goto-end))))

  (with-current-buffer (get-buffer "*Messages*")
    (if (not (bound-and-true-p tail-message-buffer-mode))
        (tail-message-buffer-mode +1)
      (tail-message-buffer-mode -1))))
(bind-key "t" #'jens/tail-message-buffer messages-buffer-mode-map)

;; auto-tail the *Messages* buffer by default
(jens/tail-message-buffer)

(defun jens/emacs-init-loc ()
  "Total lines of emacs-lisp code in my emacs configuration.

Requires the system tools `tokei' and `jq'."
  (interactive)
  (let* ((locs '("init.el"
                 "early-init.el"
                 "experimental.el"
                 "lisp/*.el"
                 "modes/*.el"
                 "straight/repos/today.el/*.el"
                 "straight/repos/dokument.el/*.el"
                 "straight/repos/orgflow.el/*.el"
                 "straight/repos/org-proplines.el/*.el"
                 "straight/repos/augment.el/*.el"
                 "straight/repos/notmuch-mojn.el/*.el"
                 "straight/repos/lowkey-mode-line.el/*.el"
                 "straight/repos/zent-theme.el/*.el"
                 "straight/repos/venv.el/*.el"
                 "straight/repos/sane-windows.el/*.el"
                 "straight/repos/etmux.el/*.el"
                 "straight/repos/views.el/*.el"
                 "straight/repos/replace-at-point.el/*.el"))
         (full-paths (-map (lambda (l) (f-full (concat user-emacs-directory l))) locs))
         (all-files (-flatten (-map (lambda (p) (f-glob p)) full-paths)))
         (all-files (s-join " " all-files))
         (cloc-cmd "tokei -o json")
         (format-cmd "jq '.\"Emacs Lisp\".code'")
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

(defun jens/diff (a b)
  "Diff A with B, either should be a location of a document, local or remote."
  (interactive)
  (let* ((file_a a)
         (file_b b))
    (unless (f-exists-p a)
      (setq file_a (jens/download-file a (concat (f-filename a) "_" (uuid)) temporary-file-directory)))
    (unless (f-exists-p b)
      (setq file_b (jens/download-file b (concat (f-filename b) "_" (uuid)) temporary-file-directory)))

    (diff file_a file_b)))

(defun jens/diff-pick ()
  "Pick two files and diff them."
  (interactive)
  (let ((a (read-file-name "File A: " (f-dirname (buffer-file-name))))
        (b (read-file-name "File B: " (f-dirname (buffer-file-name)))))
    (jens/diff a b)))

(defun jens/read-emacs-news ()
  "Check what's new in NEWS."
  (interactive)
  (let* ((news-dir (f-join source-directory "etc"))
         (news-files (cons "UPSTREAM" (f-glob "NEWS*" news-dir)))
         (pick (completing-read "news file: " news-files nil t)))
    (view-file-other-window
     (if (string= pick "UPSTREAM")
         (jens/download-file
          "https://git.savannah.gnu.org/cgit/emacs.git/plain/etc/NEWS"
          (concat "NEWS" "_" (uuid))
          temporary-file-directory)
       (f-join news-dir pick)))))


(defun screenshot-frame-to-svg ()
  "Save a screenshot of the current frame as an SVG image.  Saves
to a temp file and puts the filename in the kill ring."
  (interactive)
  (let* ((filename (make-temp-file "emacs" nil ".svg"))
         (data (x-export-frames nil 'svg)))
    (with-temp-file filename
      (insert data))
    (kill-new filename)
    (message filename)))

(defun jens/render-control-chars ()
  "Render common control characters, such as ^L and ^M."
  (interactive)
  (when (not buffer-display-table)
    (setq buffer-display-table (make-display-table)))

  (aset buffer-display-table ?\^M [])
  (aset buffer-display-table ?\^L
        (vconcat (make-list fill-column (make-glyph-code ?― 'font-lock-builtin-face))))

  (redraw-frame))

(add-hook*
 '(special-mode-hook
   view-mode-hook
   diff-mode-hook
   compilation-mode-hook
   outline-mode-hook)
 #'jens/render-control-chars)

;;;; packages

(log-info "Loading homemade packages")

(use-package buf ;; buffer extentions
  :demand t)

(use-package edit-symbol
  :bind ("C-<f12>" . edit-symbol-at-point))

(use-package parenthetic
  :straight (parenthetic :type git :repo "git@github.com:jensecj/parenthetic.el")
  :bind (("M-<prior>" . parenthetic-up)
         ("M-<next>" . parenthetic-down)))

(use-package orgflow
  :straight (orgflow :type git :repo "git@github.com:jensecj/orgflow.el.git")
  :after org
  :bind
  (("M-o u" . #'orgflow-visit-linked-url)
   ("M-o f" . #'orgflow-visit-linked-file)
   ("M-o i" . #'orgflow-insert-link-to-nearby-file)
   ("M-o R" . #'orgflow-refile-to-nearby-file)
   ("M-o n" . #'orgflow-visit-nearby-file)
   ("M-o o" . #'orgflow-visit-open-file)
   ("M-o t" . #'orgflow-visit-tagged-heading)
   ("M-o h" . #'orgflow-visit-nearby-heading)
   ("M-o H" . #'orgflow-refile-here)
   ("M-o b" . #'orgflow-visit-backlinks))
  :config
  (setq orgflow-section-sizes '(40 40))
  (setq orgflow-directory (lambda () (or default-directory (project-root (project-current)))))

  (add-to-list* 'org-speed-commands
                '(("R" . orgflow-refile-to-nearby-file)
                  ("H" . orgflow-refile-here))))

(use-package org-proplines
  :straight (org-proplines :type git :repo "git@github.com:jensecj/org-proplines.el.git")
  :after org
  :hook (org-mode . org-proplines-mode)
  :config
  (setq org-proplines-entries
        '((:RATING . (lambda (d) (s-repeat (string-to-number d) "★")))
          (:LOC . (lambda (d) (format "(%s lines)" d)))
          (:DURATION . (lambda (d) (format "(%s)" d)))
          (:DATE . (lambda (d) (format "[%s]" d))))))

(use-package org-extra
  :after org
  :bind
  (("M-o *" . #'org-extra-rate)
   ("M-o r" . #'org-extra-refile-here)
   ("M-o a" . #'org-extra-agenda-here)
   ("M-o c" . #'org-extra-copy-url-at-point)
   ("M-o T" . #'org-extra-move-subtree-at-point-to-top)
   ("M-o B" . #'org-extra-move-subtree-at-point-to-bottom))
  :config
  (add-to-list 'org-speed-commands '("C" . org-extra-copy-url-at-point)))

(use-package dev-extra :demand t)

(use-package indset
  :demand t
  :bind (("M-i" . indset)))

(use-package notmuch-mojn
  :straight (notmuch-mojn :type git :repo "git@github.com:jensecj/notmuch-mojn.el.git")
  :defer t
  :requires epa-file
  :bind*
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
  (require 'notmuch nil 'noerror)

  (setq notmuch-mojn-accounts-alist
        (get-secret 'notmuch-mojn-accounts))

  ;; TODO: action for adding contact to org-contacts?
  ;; TODO: action for viewing a mail in org-mode

  (setq notmuch-mojn-really-delete-mail t)

  (remove-hook 'notmuch-mojn-post-refresh-hook #'notmuch-mojn-mute-retag-messages)

  (defun notmuch-mojn/cache-mail-key ()
    (let ((counter 0)
          (gpg-key (get-secret 'user-gpg-key)))
      (while (and (< counter 3)
                  (not (gpg/key-in-cache-p gpg-key)))
        (cl-incf counter)
        (gpg/cache-key gpg-key))))

  (add-hook 'notmuch-mojn-pre-fetch-hook #'notmuch-mojn/cache-mail-key)
  (add-hook 'message-send-hook #'notmuch-mojn/cache-mail-key)

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

(use-package cleanup
  :defer t
  :bind (("C-c n" . cleanup-buffer))
  :config
  (cleanup-add 'org-mode '(whitespace/collapse-newlines-in-buffer org/format-buffer))
  (cleanup-add 'python-mode #'blacken-buffer))

(use-package augment
  :straight (augment :type git :repo "git@github.com:jensecj/augment.el")
  :defer t
  :hook (emacs-lisp-mode . augment-prog-mode)
  :config
  (require 'augment-git)
  (add-to-list 'augment-entries augment-entry-github-commits)
  (add-to-list 'augment-entries augment-entry-github-issues)
  (augment-sort-entries))

(use-package views
  :straight (views :type git :repo "git@github.com:jensecj/views.el.git")
  :defer t
  :bind
  (("M-v" . transient-views))
  :config
  (transient-define-prefix transient-views ()
    :transient-non-suffix 'transient--do-call
    ["Commands"
     [("s" "switch" views-switch)
      ("p" "push" views-push)
      ("k" "pop" views-pop)]]))

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
  :commands (today-capture-elfeed-at-point)
  :bind
  (("C-x t" . today-transient))
  :config
  (with-eval-after-load 'elfeed
    (bind-key "t" #'today-capture-elfeed-at-point elfeed-search-mode-map))

  (setq today-directory "~/vault/journal/")
  (setq today-file "~/vault/inbox/today.org")
  (setq today-inbox-file "~/vault/inbox/inbox.org")

  (add-to-list* 'today-capture-url-transforms
                `((,(rx (or "subscriptions.gir.st" "invidious.snopyta.org")) . "youtube.com")))

  (add-to-list* 'today-capture-auto-handlers
                `(("emacswiki\\.org" . read)
                  ("github\\.com" . plain)
                  ("github\\.io" . read)
                  ("lwn\\.net" . read)
                  ("towardsdatascience\\.com" . read)
                  ("wikipedia\\.org" . read)
                  ("teddit\\.net" . read)
                  (,(rx (or "news.ycombinator.com" "lobste.rs")) . read)
                  (,(rx (or "twitch.tv"
                            "vimeo.com"
                            "subscriptions.gir.st"
                            "invidio.us"
                            "invidious.snopyta.org")) . watch)))

  (add-to-list* 'today-capture-auto-tags
                `((,(rx (or "emacs" "elisp")) . "emacs")
                  ("guix" . "guix")
                  (,(rx (or "linux" "GNU" "ubuntu" "manjaro" "debian" "openbsd")) . "linux")
                  (,(rx (or "gopher" "gemini")) . "gopher")
                  (,(rx (or space punct) (or "AI" "A.I" "artificial intelligence")) . "ai")
                  (,(rx (or "netsec" "secops"
                            (seq "linux" (0+ any) "security")
                            (seq "security" (0+ any) "linux"))) . "netsec")
                  ("systemd" . "systemd")
                  ("devops" . "devops")
                  (,(rx (or "docker" "podman" "kubernetes" "OCI" "LXC" "LXD")) . "containers")
                  ("database" . "database")
                  ("graphql" . "graphql")
                  (,(rx (or "lisp" "clojure" "guile" "scheme")) . "lisp")
                  ("python" . "python")
                  ("java" . "java")
                  ("jvm" . "jvm")
                  ("rust" . "rust")
                  ("haskell" . "haskell")
                  (,(rx "c++" "cpp") . "cpp")
                  (,(rx (or space punct) "git") . "git")
                  ("math" . "math")
                  ("programming" . "programming")
                  ("software" . "software")
                  ("redis" . "redis")
                  ("backup" . "backup")
                  (,(rx (or "wireguard" "vpn" "tinc")) . "network")
                  ("china" . "china")
                  (,(rx (or "U.S.A.?" "U.S.?" "america")) . "usa")
                  (,(rx (or space punct) (or (seq "E" (opt ".") "U" (opt "."))
                                             (seq "europe" (opt "an") (opt "union")))) . "eu")))

  (transient-define-prefix today-transient ()
    :transient-non-suffix 'transient--do-call
    [[("a" "Archive completed todos" today-archive-done-todos)
      ("t" "Go to todays file" today-visit-todays-file)
      ("l" "List all archived files" today-list)]
     [("c" "Capture here from clipboard" today-capture-here-from-clipboard :transient t)]
     [("T" "Update title" today-capture-update-title-at-point :transient t)
      ("L" "Update LOC property" today-capture-add-property-loc-at-point :transient t)
      ("D" "Update publish date property" today-capture-add-property-archive-date-at-point :transient t)
      ("V" "Update video duration property" today-capture-add-property-video-duration-at-point :transient t)]
     [("g" "Move entry to today-file" today-move-to-today :transient t)]])
  )

(use-package dokument
  :straight (dokument :repo "git@github.com:jensecj/dokument.el.git")
  :demand t
  :bind
  (("C-+" . dokument)
   :map emacs-lisp-mode-map
   ("<f12>" . dokument/inspect-symbol-at-point))
  :commands (dokument
             dokument-company-menu-selection-quickhelp
             dokument-use-defaults)
  :config
  (with-eval-after-load 'company
    (bind-key "C-+" #'dokument-company-menu-selection-quickhelp company-active-map))

  (defun dokument/inspect-symbol-at-point (&optional arg)
    "Inspect symbol at point."
    (interactive "P")
    (require 'dokument)
    (require 'helpful)
    (let* ((sym (symbol-at-point))
           (value (cond
                   ((fboundp sym)
                    ;; TODO: fix functions defined in C.
                    (let ((def (helpful--definition sym t)))
                      (read (helpful--source sym t (car def) (cadr def)))))
                   ((boundp sym) (symbol-value sym)))))
      (if arg
          (with-current-buffer (get-buffer-create "*Inspect*")
            (let ((inhibit-read-only t))
              (erase-buffer)
              (pp value (current-buffer))
              (emacs-lisp-mode)
              (goto-char (point-min)))
            (view-buffer-other-window (current-buffer)))

        ;; TODO: set max width on popup-frame
        (funcall dokument-display-fn
                 (dokument-elisp--fontify-as-code
                  (with-output-to-string
                    (pp value)))))))

  (dokument-use-defaults))

(use-package replace-at-point
  :straight (replace-at-point :repo "git@github.com:jensecj/replace-at-point.el.git")
  :defer t
  :bind ("C-M-SPC" . replace-at-point)
  :config
  (replace-at-point-setup-defaults))

(use-package lowkey-mode-line
  :straight (lowkey-mode-line :repo "git@github.com:jensecj/lowkey-mode-line.el.git")
  :demand t
  :commands lowkey-mode-line-enable
  :config
  (lowkey-mode-line-enable))

;;; third-party packages

(log-info "Loading third-party packages")

(use-package epdh
  :straight (emacs-package-dev-handbook
             :host github :repo "alphapapa/emacs-package-dev-handbook"
             :fork (:host github :repo "jensecj/emacs-package-dev-handbook")))

;;;; major modes and extentions

(use-package yaml-mode :straight t :defer t :mode ("\\.yaml\\'" "\\.yml\\'"))
(use-package toml-mode :straight t :defer t :mode ("\\.toml\\'"))
(use-package dockerfile-mode :straight t :defer t :mode "\\Dockerfile\\'")
(use-package gitconfig-mode :straight t :defer t :mode "\\.gitconfig\\'")
(use-package gitignore-mode :straight t :defer t :mode "\\.gitignore\\'")
(use-package lua-mode :straight t :defer t :mode "\\.lua\\'")
(use-package markdown-mode :straight t :defer t :mode ("\\.md\\'" "\\.card\\'"))
(use-package scss-mode :straight t :defer t :mode "\\.scss\\'")
(use-package tuareg :straight t :defer t :mode ("\\.ml\\'" "\\.mli\\'" "\\.mli\\'" "\\.mll\\'" "\\.mly\\'"))
(use-package ini-mode :straight t :defer t)
(use-package systemd :straight t :defer t)
(use-package nginx-mode :straight t :defer t)

(use-package json-mode
  :straight t
  :defer t
  :hook ((json-mode . flycheck-mode))
  :config
  (setq js-indent-level tab-width))

(use-package elpher
  :straight t
  :bind (("M-s M-e" . #'elpher/open-url-at-point))
  :config
  (defun elpher/open-url-at-point ()
    (interactive)
    (let ((url (or
                (if (region-active-p) (buffer-substring-no-properties (region-beginning) (region-end)))
                (browse-url/get-url-at-point))))
      (view-buffer-other-window "*elpher*")
      (elpher-go url))))

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
  :bind
  :config
  (unbind-key "M-," clojure-mode-map)
  (unbind-key "M-." clojure-mode-map)
  (unbind-key "M--" clojure-mode-map)

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
  :defer t
  :bind
  (("C-x m" . magit-status)
   :map magit-mode-map
   ("C-c C-a" . magit-commit-amend)
   ("<tab>" . magit-section-cycle))
  :config
  (setq magit-auto-revert-mode nil)
  (put 'magit-log-mode 'magit-log-default-arguments '("--graph" "--color" "-n256" "--decorate"))
  (setq magit-merge-arguments '("--no-ff"))
  (setq magit-section-visibility-indicator '("…", t))
  (setq git-commit-summary-max-length fill-column)
  (setq git-commit-style-convention-checks nil) ; don't warn about long or multiline messages
  (setq magit-diff-refine-hunk t)

  ;; disable hl-line-mode in magit, it messes with diffs
  (add-hook 'magit-mode-hook
            (lambda ()
              (make-variable-buffer-local 'global-hl-line-mode)
              (setq global-hl-line-mode nil)))

  (add-hook 'git-commit-mode-hook #'flyspell-prog-mode))

(use-package magit-section :straight t)

(use-package forge
  :straight t
  :after (magit transient)
  :config
  (add-to-list 'forge-owned-accounts (list (list (get-secret 'user-github-account))))
  ;; setup augment.el, see (forge-bug-reference-setup)
  (transient-append-suffix 'magit-status-jump '(0 0 -1)
    '("I " "Issues" forge-jump-to-issues))

  (transient-append-suffix 'magit-status-jump '(0 0 -1)
    '("P " "Pull-requests" forge-jump-to-pullreqs)))

(use-package magithub
  ;; https://github.com/vermiculus/magithub/blob/9fb9c653d0dad3da7ccff3ae321fa6e54c08f41b/magithub.el#L223
  ;; https://github.com/vermiculus/ghub-plus
  ;; https://github.com/vermiculus/apiwrap.el
  ;; https://github.com/magit/ghub/issues/84
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
  (transient-append-suffix 'magit-status-jump '(0 0 -1)
    '("T " "Todos" magit-todos-jump-to-todos))

  (setq magit-todos-exclude-globs '("var/*" "venv/*" "vendor/*")))

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
  :defer t
  :defines elfeed-search-mode-map
  :commands (elfeed elfeed-search-selected elfeed/load-feeds)
  :functions elfeed/copy-link-at-point
  :bind
  (:map elfeed-search-mode-map
        ("c" . elfeed/copy-link-at-point)
        ("V" . elfeed/play-video-at-point))
  :config
  (require 'today)

  (setq elfeed-search-filter "@100-month-ago +unread")

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

  (add-hook 'elfeed-new-entry-hook
            (elfeed-make-tagger :feed-url "sachachua\\.com"
                                :entry-title (list "Weekly review:")
                                :add 'junk
                                :remove 'unread))

  (defun elfeed/load-feeds ()
    (interactive)
    (let* ((feeds-file (locate-user-emacs-file "elfeeds.el"))
           (feeds (jens/load-from-file feeds-file)))
      (setq elfeed-feeds feeds)))

  (elfeed/load-feeds)

  (defun elfeed/-focus-emacs-after-browse-url (fn &rest args)
    "Activate the emacs-window"
    (let* ((emacs-window (shell-command-to-string "xdo id"))
           (active-window "")
           (counter 0))
      (apply fn args)
      (sleep-for 0.7)
      (while (and (not (string= active-window emacs-window))
                  (< counter 15))
        (sleep-for 0.2)
        (cl-incf counter)
        (setq active-window (shell-command-to-string "xdo id"))
        (shell-command-to-string (format "xdo activate %s" emacs-window)))))

  (advice-add #'elfeed-search-browse-url :around #'elfeed/-focus-emacs-after-browse-url)

  (defun elfeed/play-video-at-point ()
    "Attempt to play the video link of the elfeed entry at point."
    (interactive)
    (letrec ((entry (car (elfeed-search-selected)))
             (link (elfeed-entry-link entry))
             (mpv-buf (get-buffer-create "*mpv*")))
      (start-process "mpv-ytdl" mpv-buf "mpv" "--ytdl-format=bestvideo[width<=1920][height<=1080]" "--ytdl" link)
      (view-buffer-other-window mpv-buf)))

  (defun elfeed/copy-link-at-point ()
    "Copy the link of the elfeed entry at point to the
clipboard."
    (interactive)
    (letrec ((entry (car (elfeed-search-selected)))
             (link (elfeed-entry-link entry)))
      (with-temp-buffer
        (insert link)
        (clipboard-kill-ring-save (point-min) (point-max))
        (message (format "copied %s to clipboard" link))))))

(use-package pdf-tools
  :straight t
  :defer t
  :hook (pdf-view-mode . auto-revert-mode)
  :bind
  ;; need to use plain isearch, pdf-tools hooks into it to handle searching
  (:map pdf-view-mode-map ("C-s" . isearch-forward))
  :config
  (pdf-tools-install 'no-query 'skip-dependencies))

(use-package pdf-continuous-scroll-mode
  :straight (pdf-continuous-scroll-mode
             :host github
             :repo "dalanicolai/pdf-continuous-scroll-mode.el")
  :hook (pdf-view-mode . pdf-continuous-scroll-mode))

(use-package helpful
  ;; replacement for *help* buffers that provides more contextual information.
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
        ("M-a" . helpful-at-point)
        ("l" . find-library))
  :config
  (defalias #'describe-key #'helpful-key)
  (defalias #'describe-function #'helpful-callable)
  (defalias #'describe-variable #'helpful-variable)
  (defalias #'describe-symbol #'helpful-symbol)
  (defalias #'describe-key #'helpful-key))

(use-package elisp-demos
  :straight t
  :after helpful
  :config
  (advice-add #'helpful-update :after #'elisp-demos-advice-helpful-update))

(use-package inform
  ;; hyperlinks of symbols (functions, variables, etc.) in Info to their documentation.
  :straight t)

(use-package vdiff
  :straight t
  :defer t
  :commands (vdiff-files vdiff-files3)
  :bind
  (:map diff-mode-map
        ("V" . #'vdiff/from-diff))
  :config
  (setq vdiff-auto-refine t)
  (setq vdiff-only-highlight-refinements t)

  (bind-key "C-c" vdiff-mode-prefix-map vdiff-mode-map)
  (bind-key "C-c" vdiff-mode-prefix-map vdiff-3way-mode-map)

  (defun vdiff/from-diff ()
    (interactive)
    (when (eq major-mode 'diff-mode)
      (let ((files (diff/get-files)))
        (vdiff-files (nth 0 files) (nth 1 files))))))

(use-package vdiff-magit
  :straight t
  :after magit
  :bind
  (:map magit-mode-map
        ("e" . #'vdiff-magit-dwim)
        ("E" . #'vdiff-magit))
  :config
  (transient-suffix-put 'magit-dispatch "e" :description "vdiff (dwim)")
  (transient-suffix-put 'magit-dispatch "e" :command 'vdiff-magit-dwim)
  (transient-suffix-put 'magit-dispatch "E" :description "vdiff")
  (transient-suffix-put 'magit-dispatch "E" :command 'vdiff-magit))

(use-package circe
  :straight t
  :defer t
  :bind (:map circe-mode-map
              ("C-t" . #'circe/switch-buffer)
              ("C-S-t" . #'tracking-next-buffer))
  :config
  (setq circe-default-quit-message "")
  (setq circe-default-part-message "")
  (setq circe-reduce-lurker-spam t)

  (setq circe-server-auto-join-default-type :after-auth)

  (setq circe-highlight-nick-type 'occurrence)

  (setq circe-format-say "{nick}: {body}")
  (setq circe-format-self-say "{nick}: {body}")

  (setq circe-chat-buffer-name "{target}")
  (setq circe-server-buffer-name "*{host}*")

  (setq lui-time-stamp-position 'left)
  (setq lui-time-stamp-format "%H:%M ")
  (setq lui-time-stamp-only-when-changed-p nil)
  (setq lui-fill-column 80)
  (setq lui-flyspell-p t)

  (require 'circe-color-nicks)
  (setq circe-color-nicks-everywhere t)
  (add-to-list* 'circe-color-nicks-message-blacklist
                '("the" "The" "NSA" "nsa" "another" "nevermind"))

  (enable-circe-color-nicks)
  (enable-lui-track)

  ;; don't list names when joining channels
  (circe-set-display-handler "353" 'circe-display-ignore)
  (circe-set-display-handler "366" 'circe-display-ignore)

  ;; don't display when lurkers become active
  (advice-add #'circe-lurker-display-active :override #'ignore)

  (defun circe/buffers ()
    "Return a list of Circe buffers"
    (-filter
	   (lambda (buffer)
       (provided-mode-derived-p
        (buffer-local-value 'major-mode buffer)
        'circe-mode))
     (buffer-list)))

  (defun circe/switch-buffer ()
    "Switch to a Circe buffer interactively."
    (interactive)
    (let* ((buffers (mapcar #'buffer-name (circe/buffers)))
           (pick (completing-read "buffer: " buffers nil t)))
      (switch-to-buffer pick)))

  (defun circe/switch-buffer-xf (s)
    "Cleanup names of CIRCE buffers."
    (let* ((buf (get-buffer s))
           (buf-name (->> buf
                       (buffer-name)
                       (string-replace "*" "")
                       (string-trim))))
      buf-name))

  (defun circe/rename-chat-buffers ()
    "Rename Circe chat buffers.
Prefix buffers with server-name, append @ to query buffers."
    (let ((channel (buffer-name))
          (server (with-circe-server-buffer circe-server-name)))
      (when (eq major-mode 'circe-query-mode)
        (setq channel (concat "@" channel)))

      (rename-buffer (format "*%s/%s*" server channel) t)))

  (add-hook 'circe-chat-mode-hook #'circe/rename-chat-buffers)

  ;; when /join'ing, just switch to the buffer, don't open a new window
  (patch-add
   #'circe-command-JOIN
   "5996ffd7bc7eb3639c0739b51074096a"
   '(pop-to-buffer
     (circe-server-get-or-create-chat-buffer channel
                                             'circe-channel-mode))
   '(switch-to-buffer
     (circe-server-get-or-create-chat-buffer channel
                                             'circe-channel-mode)))

  ;; don't highlight the nicknames of lurkers
  ;; lots of people use everyday words as nicknames
  ;; e.g "the", "nevermind", or "always"
  (patch-add
   #'circe-color-nicks
   "1582d0fb0f85ce1924faa0a9aeffd0be"
   '(not (member nick blacklist))
   '(and (not (member nick blacklist))
         (not (circe-lurker-p nick))))

  (defun circe/duration-string (seconds)
    "Return a description of SECONDS as days,hours,minutes,seconds."
    (let* ((days (floor (/ seconds 60 60 24)))
           (seconds (- seconds (* 60 60 24 days)))
           (hours (floor (/ seconds 60 60)))
           (seconds (- seconds (* 60 60 hours)))
           (minutes (floor (/ seconds 60)))
           (seconds (round (- seconds (* 60 minutes)))))
      (concat
       (when (> days 0)    (format "%sd" days))
       (when (> hours 0)   (format "%sh" hours))
       (when (> minutes 0) (format "%sm" minutes))
       (when (> seconds 0) (format "%ss" seconds)))))

  (advice-add #'circe-duration-string :override #'circe/duration-string)

  (comment
   (circe "Libera Chat"
          :tls t
          :nick "jensecj"
          :sasl-username "jensecj"
          :sasl-password (lambda (&rest _) (get-secret "irc/libera/jensecj"))
          :channels '("#emacs"))
   )
  )

(use-package notmuch
  ;; TODO: setup notmuch for multiple mail-profiles
  ;; see https://www.djcbsoftware.nl/code/mu/mu4e/Contexts-example.html
  :straight t
  :defer t
  :bind
  (:map notmuch-show-mode-map
        ("B" . #'notmuch/show-list-links)
        ("N" . #'notmuch-show/goto-next-unread-message)
        ("P" . #'notmuch-show/goto-previous-unread-message)
        :map notmuch-search-mode-map
        ("N" . #'notmuch-search/goto-next-unread-thread)
        ("P" . #'notmuch-search/goto-previous-unread-thread)
        ("U" . #'notmuch-search/filter-unread)
        ("<C-return>" . #'notmuch-search/show-thread-first-unread)
        ("s" . #'notmuch/search)
        :map notmuch-message-mode-map
        ("C-c C-a" . mail-add-attachment)
        ("M-i" . #'notmuch/change-identity)
        ("M-t" . #'notmuch/set-recipient)
        :map notmuch-show-part-map
        ("V" . #'notmuch-show/view-mime-part-at-point-in-mode))
  :config
  (setq notmuch-identities (get-secret 'user-mail-identities))
  (setq notmuch-fcc-dirs "sent +sent +new -unread")
  (setq notmuch-column-control 1.0)
  (setq notmuch-wash-wrap-lines-length fill-column)
  (setq notmuch-wash-citation-lines-prefix 10)
  (setq notmuch-wash-citation-lines-suffix 0)
  (setq notmuch-wash-citation-regexp (rx bol (0+ space) (optional (1+ alpha)) ">" (0+ any) eol))
  (setq notmuch-wash-button-signature-hidden-format "\n[ signature -- click to show ]")
  (setq notmuch-wash-button-signature-visible-format "\n[ signature -- click to hide ]")
  (setq notmuch-wash-button-citation-hidden-format "[ %d more lines -- click to show ]")
  (setq notmuch-wash-button-citation-visible-format "[ %d more lines -- click to hide ]")
  (setq notmuch-wash-button-original-hidden-format "[ %d-line hidden original message. click to show ]")
  (setq notmuch-wash-button-original-visible-format "[ %d-line hidden original message. click to hide ]")

  (add-to-list 'notmuch-show-insert-text/plain-hook
               #'notmuch-wash-convert-inline-patch-to-part)

  (defun notmuch/regexify (&rest lines)
    (let ((sep "[ \n]*"))
      (cl-flet ((line-to-regex (line)
                               (->> line
                                 (s-collapse-whitespace)
                                 (string-replace " " sep)
                                 (s-prepend sep)
                                 (s-append sep))))
        (let ((regex-lines (-map #'line-to-regex lines)))
          (s-join sep regex-lines)))))

  (setq notmuch-common-preambles
        `((rms . ,(notmuch/regexify "[> ]* [[[ To any NSA and FBI agents reading my email: please consider    ]]]"
                                    "[> ]* [[[ whether defending the US Constitution against all enemies,     ]]]"
                                    "[> ]* [[[ foreign or domestic, requires you to follow Snowden's example. ]]]"))

          (fsf . ,(notmuch/regexify "*Please consider adding <info@fsf.org> to your address book,"
                                    "which will ensure that our messages reach you and not your spam box.*"))))

  (defun notmuch/wash-common-preambles (_msg _depth)
    (dolist (pre notmuch-common-preambles)
      (goto-char (point-min))
      (when (re-search-forward (cdr pre) nil t)
        (replace-match ""))))

  (add-hook 'notmuch-show-insert-text/plain-hook #'notmuch/wash-common-preambles)

  (defun notmuch/set-recipient ()
    (interactive)
    (message-goto-to))

  (defun notmuch/change-identity ()
    (interactive)
    (let ((id (completing-read "identity: " notmuch-identities)))
      (message-replace-header "From" id)))

  ;; notmuch-message-forwarded-tags
  ;; notmuch-draft-tags
  ;; notmuch-message-replied-tags
  (add-to-list* 'notmuch-archive-tags '("+archived" "-deleted"))

  (defface notmuch-search-muted-face `((t (:foreground ,zent-grey-1)))
    "Face used in search modes for muted threads.")

  (add-to-list 'notmuch-search-line-faces '("muted" . notmuch-search-muted-face))

  (defun notmuch/excl (query &rest tags)
    "Exclude TAGS from QUERY."
    (let* ((default-excludes '("archived" "lists" "builds" "draft" "sent" "deleted"))
           (all-tags (-concat default-excludes tags))
           (excludes (s-join " " (-map (lambda (tag) (format "and not tag:%s" tag)) all-tags))))
      (format "%s %s" query excludes)))

  (setq notmuch-saved-searches
        `((:name "subst"   :key "ps" :query "to:jens@subst.net" :sort-order newest-first)
          (:name "posteo"  :key "pp" :query "to:jensecj@posteo.net" :sort-order newest-first)
          (:name "gmail"   :key "pg" :query "to:jensecj@gmail.com" :sort-order newest-first)
          (:blank t)
          (:name "unread"  :key "u" :query ,(notmuch/excl "tag:unread") :sort-order newest-first)
          (:name "today"   :key "t" :query ,(notmuch/excl "date:today..") :sort-order newest-first)
          (:name "7 days"  :key "W" :query ,(notmuch/excl "date:7d..") :sort-order newest-first)
          (:name "30 days" :key "M" :query ,(notmuch/excl "date:30d..") :sort-order newest-first)
          (:name "inbox"   :key "i" :query "tag:inbox" :sort-order newest-first)
          (:blank t)
          (:name "emacs-devel" :key "ld" :query "tag:lists/emacs-devel" :sort-order newest-first)
          (:name "emacs-help"  :key "lh" :query "tag:lists/emacs-help" :sort-order newest-first)
          (:name "emacs-bugs"  :key "lb" :query "tag:lists/emacs-bugs" :sort-order newest-first)
          (:name "emacs-misc"  :key "lm" :query "tag:lists/emacs-misc" :sort-order newest-first)
          (:blank t)
          (:name "work"     :key "w" :query "tag:work" :sort-order newest-first)
          (:name "builds"   :key "B" :query "tag:builds" :sort-order newest-first)
          (:name "personal" :key "p" :query "tag:personal" :sort-order newest-first)
          (:name "bills"    :key "b" :query "tag:bills" :sort-order newest-first)
          (:blank t)
          (:name "all mail" :key "a" :query "not tag:lists and not tag:draft" :sort-order newest-first)
          (:name "archive"  :key "r" :query "tag:archived" :sort-order newest-first)
          (:name "sent"     :key "s" :query "tag:sent" :sort-order newest-first)
          (:name "drafts"   :key "d" :query "tag:draft" :sort-order newest-first)
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
          (bind-key key (lambda () (interactive) (funcall fn tags)) mode-map))
      (error "%s is not a proper notmuch mode" mode)))

  ;; delete mail in all modes with "d"
  (apply* #'notmuch/quicktag '(show search tree) "d" '(("-inbox" "-archived" "+deleted")))
  ;; mute mail in all modes with "M"
  (apply* #'notmuch/quicktag '(show search tree) "M" '(("-unread" "+muted")))

  (defun notmuch/search ()
    "Search notmuch interactively, using the current query as initial input."
    (interactive)
    (let ((query (notmuch-search-get-query)))
      (notmuch-search (completing-read "Notmuch search: " nil nil nil query))))

  ;;;;;;;;;;;;;;;;;;;;;;;
  ;; notmuch-show mode ;;
  ;;;;;;;;;;;;;;;;;;;;;;;

  (setq notmuch-show-logo nil)
  (setq notmuch-show-indent-messages-width 1)
  (setq notmuch-show-relative-dates nil)
  (setq notmuch-message-headers-visible nil)
  (remove-hook 'notmuch-show-hook #'notmuch-show-turn-on-visual-line-mode)

  (defun notmuch-show/update-header-line ()
    "Set the `header-line' to the subject of the current message."
    (setq header-line-format (map-elt* (notmuch-show-get-message-properties) :headers :Subject)))

  (advice-add 'notmuch-show-command-hook :after #'notmuch-show/update-header-line)

  (setq notmuch-show-max-text-part-size 10000) ; collapse text-parts over 10000 characters

  (defun notmuch-show/format-headerline-date (&rest args)
    (let* ((args (car args))
           (date (nth 1 args))
           (new-date (format-time-string
                      "%Y-%m-%d %H:%M %z"
                      (encode-time (parse-time-string date)))))
      (list (nth 0 args) new-date (nth 2 args) (nth 3 args))))

  (advice-add #'notmuch-show-insert-headerline
              :filter-args
              #'notmuch-show/format-headerline-date)

  (defun notmuch/show-list-links ()
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

  (add-hook 'notmuch-show-mode-hook #'augment-mode)

  ;; TODO: dont just show in mode, allow handlers to open mime parts
  (defun notmuch-show/view-mime-part-at-point-in-mode ()
    "Open MIME-part at point in a specific major-mode."
    (interactive)
    (let* ((modes '(org-mode emacs-lisp-mode text-mode markdown-mode diff-mode))
           (mode (intern (completing-read "mode: " modes nil t))))
      (notmuch-show-apply-to-current-part-handle
       (lambda (handle)
         (let ((buf (generate-new-buffer " *notmuch-internal-part*")))
           (with-current-buffer buf
             (if (eq (mm-display-part handle) 'external)
	               (kill-buffer buf)
               (goto-char (point-min))
               (set-buffer-modified-p nil)
               (funcall mode)

               (if (eq major-mode 'org-mode)
                   (org-show-all))

               (font-lock-ensure)))
           (view-buffer-other-window buf #'kill-buffer-if-not-modified)))
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

  ;; make sure that messages end in a newline, just like the newline after the header.
  (patch-add
   #'notmuch-show-insert-msg
   "a4034680fd0b23eb0c464c7fc632e0d7"
   '(unless (bolp)
      (insert "\n"))
   '(progn
      (unless (bolp)
        (insert "\n"))
      (insert "\n")))

  ;; only show text/plain part by default
  (patch-add
   #'notmuch-show-insert-bodypart
   "0a5f4201858462717c33186d2579e9b1"
   '(not (or (equal hide t)
			       (and long button)))
   '(or (notmuch-match-content-type mime-type "multipart/*")
        (notmuch-match-content-type mime-type "text/plain")))

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

  (defun notmuch-search/filter-unread ()
    "Filter the current search-query to only show unread messages."
    (interactive)
    (let ((query (notmuch-search-get-query)))
      (notmuch-search (concat query " and tag:unread"))))

  (defun notmuch-search/show-thread-first-unread ()
    "Show the thread-at-point, but jump to the first unread message."
    (interactive)
    (notmuch-search-show-thread)
    (goto-char (point-min))
    (unless (notmuch-show/message-has-tag-p "unread")
      (notmuch-show/goto-next-unread-message)))

  ;;;;;;;;;;;;;
  ;; message ;;
  ;;;;;;;;;;;;;

  (defun notmuch-message-mode-setup ()
    (auto-fill-mode -1)
    (visual-line-mode +1)
    (visual-fill-column-mode +1))

  (add-hook 'notmuch-message-mode-hook #'notmuch-message-mode-setup)

  (defun notmuch-cite-function (&rest _args)
    (let ((from (mail-header-from message-reply-headers))
	        (date (mail-header-date message-reply-headers))
          (cc (message-fetch-field "Cc")))
      (insert
       (concat
        (when from (format "> From: %s\n" from))
        (when date (format "> Date: %s\n" date))
        (when cc (format "> Cc: %s\n" cc))))
      (newline)))

  (setq message-citation-line-function #'notmuch-cite-function)

  ;; When a message is sent to one of my ID's, i want to reply from that ID,
  ;; otherwise force choosing an identity
  (defun notmuch-show/reply-setup (f &rest args)
    (let ((from (notmuch-show-get-from))
          (to (notmuch-show-get-to)))

      ;; here, we are in a notmuch-show buffer
      (apply f args)
      ;; here, we are in a new message-buffer

      (message-remove-header "From")

      (dolist (id notmuch-identities)
        (if (string-match (regexp-quote to) id)
            (message-replace-header "From" id)))

      (set-buffer-modified-p nil)))

  (advice-add #'notmuch-mua-reply :around #'notmuch-show/reply-setup)

  (defun notmuch-show/new-mail-setup (&rest args)
    (message-remove-header "From")
    (set-buffer-modified-p nil))

  (advice-add #'notmuch-mua-mail :after #'notmuch-show/new-mail-setup)

  (defun notmuch-message/ensure-sender ()
    (when (not (message-field-value "From"))
      (error "Message has no sender!")))

  (add-hook 'notmuch-mua-send-hook #'notmuch-message/ensure-sender)

  ;;;;;;;;;;;;
  ;; extras ;;
  ;;;;;;;;;;;;

  ;; TODO: fix these so org-store-link stores link to the message at point in search mode.
  (require 'ol-notmuch)
  (bind-key "C-c C-l" #'org-store-link notmuch-show-mode-map)
  (bind-key "C-c C-l" #'org-store-link notmuch-search-mode-map))

;;;; extensions to built-in packages

(use-package flycheck
  :straight t
  :defer t
  :hook ((python-mode) . flycheck-mode)
  :config
  (setq flycheck-display-errors-delay 0.4)
  (setq flycheck-indication-mode 'right-fringe)

  (defun flycheck/error-list-make-last-column (args)
    "Transform messages from pycheckers to report correct checker."
    (cl-destructuring-bind (msg checker) args
      (when (eq checker 'python-pycheckers)
        (let ((idx (regexp-search-in-string ": " msg)))
          (setq checker (intern (substring msg 0 idx)))
          (setq msg (substring msg (+ idx 2)))))
      (list msg checker)))

  (advice-add #'flycheck-error-list-make-last-column
              :filter-args
              #'flycheck/error-list-make-last-column)

  (setq flycheck-error-list-format
        `[("File" 8)
          ("Line" 3 flycheck-error-list-entry-< :right-align t)
          ("Col" 2 nil :right-align t)
          ("Level" 8 flycheck-error-list-entry-level-<)
          ("ID" 6 t)
          ("Message" 0 t)])

  (defun flycheck/adjust-flycheck-automatic-syntax-eagerness ()
    "Adjust how often we check for errors based on if there are any.
  This lets us fix any errors as quickly as possible, but in a
  clean buffer we're an order of magnitude laxer about checking."
    (setq flycheck-idle-change-delay
          (if flycheck-current-errors 0.3 1.5)))

  ;; Each buffer gets its own idle-change-delay because of the
  ;; buffer-sensitive adjustment above.
  (make-variable-buffer-local 'flycheck-idle-change-delay)

  (add-hook 'flycheck-after-syntax-check-hook
            #'flycheck/adjust-flycheck-automatic-syntax-eagerness)

  ;; Remove newline checks, since they would trigger an immediate check
  ;; when we want the idle-change-delay to be in effect while editing.
  ;; (setq-default flycheck-check-syntax-automatically '(save idle-change new-line mode-enabled))

  (define-fringe-bitmap 'flycheck--fringe-indicator
    (vector #b11000000
            #b11000000
            #b11000000
            #b11000000
            #b11000000
            #b11000000
            #b11000000
            #b11000000
            #b11000000
            #b11000000
            #b11000000
            #b11000000
            #b11000000
            #b11000000
            #b11000000
            #b11000000))

  (let ((bitmap 'flycheck--fringe-indicator))
    (flycheck-define-error-level 'info
      :severity 0
      :overlay-category 'flycheck-info-overlay
      :fringe-bitmap bitmap
      :error-list-face 'flycheck-error-list-info
      :fringe-face 'flycheck-fringe-info)

    (flycheck-define-error-level 'warning
      :severity 1
      :overlay-category 'flycheck-warning-overlay
      :fringe-bitmap bitmap
      :error-list-face 'flycheck-error-list-warning
      :fringe-face 'flycheck-fringe-warning)

    (flycheck-define-error-level 'error
      :severity 2
      :overlay-category 'flycheck-error-overlay
      :fringe-bitmap bitmap
      :error-list-face 'flycheck-error-list-error
      :fringe-face 'flycheck-fringe-error)))

(use-package flycheck-package
  :straight t
  :after flycheck
  :config (flycheck-package-setup))

(use-package flycheck-checkbashisms
  :straight t
  :after flycheck
  :config (flycheck-checkbashisms-setup))

(use-package flycheck-bashate
  :straight t
  :after flycheck
  :config (flycheck-bashate-setup))

(use-package flyspell-correct
  :straight t
  :after flyspell
  :bind
  (:map flyspell-mode-map
        ("C-," . flyspell-correct-at-point))
  :commands flyspell-correct-at-point)

(use-package frog-menu
  :straight t
  :after flyspell-correct
  :config
  (setq frog-menu-avy-padding t)
  (setq frog-menu-min-col-padding 4)
  (setq frog-menu-format 'vertical)

  (defun frog-menu/flyspell-correct (candidates word)
    (interactive)
    (let* ((actions `(("C-s" "Save word" (save . ,word))
                      ("C-a" "Accept (session)" (session . ,word))
                      ("C-b" "Accept (buffer)" (buffer . ,word))
                      ("C-c" "Skip" (skip . ,word))))
           (prompt (format "Dictionary: [%s]" (or ispell-local-dictionary
                                                  ispell-dictionary
                                                  "default"))))
      (frog-menu-read prompt candidates actions)))

  (setq flyspell-correct-interface #'frog-menu/flyspell-correct))

(use-package diredfl
  :straight t
  :after dired
  :hook (dired-mode . diredfl-mode)
  :config
  (setq diredfl-ignore-compressed-flag nil))

(use-package dired-filter
  :straight dired-hacks
  :after dired)

(use-package dired-subtree
  :straight dired-hacks
  :after dired
  :bind
  (:map dired-mode-map
        ("<tab>" . dired-subtree-toggle))
  :config
  (setq dired-subtree-use-backgrounds nil))

(use-package dired-rainbow
  :straight dired-hacks
  :after (dired zent-theme)
  :config
  (defmacro dired-rainbow/zent (fn sym face exts)
    "Wrapper to inject colors from `zent-theme'."
    (when (and (symbolp face)
               (string-prefix-p "zent-" (symbol-name face)))
      (setq face (symbol-value face)))

    `(,fn ,sym ,face ,exts))

  (defmacro dired-rainbow/def (sym face exts)
    `(dired-rainbow/zent dired-rainbow-define ,sym ,face ,exts))

  (defmacro dired-rainbow/def-chmod (sym face exts)
    `(dired-rainbow/zent dired-rainbow-define-chmod ,sym ,face ,exts))

  (dired-rainbow/def-chmod directory zent-yellow   "d.*")
  (dired-rainbow/def-chmod exec      zent-green+1  "-.*x.*")
  (dired-rainbow/def-chmod symlink   zent-yellow-2 "l.*")
  (dired-rainbow/def partition       zent-blue-4    ("bak" "bin" "dmg" "iso" "vmdk"))
  (dired-rainbow/def web             zent-red+2    ("css" "htm" "html" "less" "mustache" "sass" "scss"))
  (dired-rainbow/def document        zent-magenta  ("PDF" "djvu" "doc" "docm" "docx" "epub" "mobi" "odb" "odp" "odt" "pdf" "ppt" "pptx" "ps" "rtf"))
  (dired-rainbow/def media           zent-blue-1   ("MP3" "MP4" "avi" "flac" "flv" "mid" "midi" "mkv" "mov" "mp3" "mp4" "mpeg" "mpg" "ogg" "wav"))
  (dired-rainbow/def image           zent-blue+3   ("JPEG" "gif" "ico" "jpeg" "jpg" "png" "svg"))
  (dired-rainbow/def log             zent-grey-1   ("log"))
  (dired-rainbow/def encrypted       zent-tan-1    ("age" "asc" "enc" "gpg" "minisig" "pem" "pgp" "sig" "signature"))
  (dired-rainbow/def packaged        zent-blue-6   ("apk" "bsp" "deb" "jar" "rpm"))
  (dired-rainbow/def xml             zent-yellow-4 ("bib" "edn" "json" "msg" "rss" "xml" "xsd" "xsl" "xslt" "yaml" "yml"))
  (dired-rainbow/def markdown        zent-orange   ("info" "markdown" "md" "org" "rst" "tex" "txt"))
  (dired-rainbow/def database        zent-green+4  ("csv" "db" "mdb" "nc" "sqlite" "xls" "xlsx"))
  (dired-rainbow/def compressed      zent-red-2    ("7z" "Z" "apk" "bz2" "ear" "gz" "jar" "rar" "tar" "tgz" "txz" "war" "xpi" "xz" "xz" "z" "zip" "zst"))
  (dired-rainbow/def compiled        zent-blue-1   ("asm" "c" "c++" "cc" "cl" "cp" "cpp" "cs" "el" "f" "go" "h" "h++" "hi" "hpp" "hs" "hxx" "java" "lisp" "m" "rs" "s"))
  (dired-rainbow/def interpreted     zent-blue-1   ("clj" "cljc" "cljs" "ipynb" "js" "mysql" "pgsql" "py" "r" "rb" "scala" "sql"))
  (dired-rainbow/def artifact        zent-cyan     ("class" "elc" "eln" "o" "out" "pyc"))
  )

(use-package geiser
  :straight t
  :defer t
  :hook (scheme-mode . geiser-mode)
  :config
  (setq geiser-chicken-binary "chicken-csi")
  (setq geiser-active-implementations '(chicken)))

(use-package chicken
  :download "https://code.call-cc.org/cgi-bin/gitweb.cgi?p=chicken-core.git;a=blob_plain;f=misc/chicken.el"
  :after scheme-mode
  :config
  (setq scheme-program-name "chicken-csi -:c")
  (setq scheme-compiler-name "chicken-csc")

  (defun chicken/compile-this-file ()
    (interactive)
    (shell-command-to-string (format "%s %s" scheme-compiler-name (buffer-file-name)))))

(use-package elpy
  :straight t
  :after python-mode
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
(use-package flycheck-pycheckers
  :straight t
  :config
  (setq flycheck-pycheckers-checkers '(pylint flake8 mypy3 bandit))
  (add-to-list* 'flycheck-pycheckers-ignore-codes
                '("W1203" ;; dont use f-strings in logging
                  "W1201" ;; use lazy % in logging functions
                  "E501" ;; line too long
                  "C0301" ;; line too long
                  "W0611" ;; DUPLICATE: unused import
                  "C0410" ;; DUPLICATE: multiple import on one line
                  "E999" ;; DUPLICATE: invalid syntax
                  "F821" ;; DUPLICATE: undefined variable
                  "C0303" ;; DUPLICATE: trailing whitespace
                  "B101" ;; asserts are compiled out
                  "E0602" ;; DUPLICATE: undefined variable
                  "E203" ;; whitespace before :
                  "C0321" ;; DUPLICATE: multiple statements on one line
                  "E711" ;; DUPLICATE: comparison to none should be "is none"
                  "F841" ;; DUPLICATE: local variable never used
                  ))
  ;; TODO: update flycheck-pycheckers-venv-root using project.el?
  ;; (pop flycheck-pycheckers-ignore-codes)

  (with-eval-after-load 'flycheck
    (add-hook 'flycheck-mode-hook #'flycheck-pycheckers-setup)))

(use-package blacken
  :straight t
  :after python-mode
  :config
  (setq blacken-line-length 'fill))

(use-package realgud
  :defer t
  :straight t)

(use-package highlight-defined
  :straight t
  :diminish highlight-defined-mode
  :hook (emacs-lisp-mode . highlight-defined-mode))

(use-package highlight-numbers
  :straight t
  :hook (prog-mode . highlight-numbers-mode))

(use-package highlight-thing
  :straight t
  :diminish highlight-thing-mode
  :config
  (add-to-list* 'highlight-thing-ignore-list '("nil" "t" "*" "**" "***" "****" "-" "_"))
  (setq highlight-thing-delay-seconds 0.4)
  (setq highlight-thing-case-sensitive-p nil)
  (setq highlight-thing-exclude-thing-under-point nil)
  (setq highlight-thing-limit-to-region-in-large-buffers-p t)
  (setq highlight-thing-narrow-region-lines 30)
  (add-to-list* 'highlight-thing-excluded-major-modes '(pdf-view-mode doc-view-mode notmuch-show-mode notmuch-search-mode))
  (global-highlight-thing-mode +1))

(use-package fontify-face
  :straight t
  :defer t
  :diminish fontify-face-mode
  :hook (emacs-lisp-mode . fontify-face-mode))

(use-package package-lint :straight t :defer t :commands (package-lint-current-buffer))
(use-package relint :straight t :defer t :commands (relint-current-buffer relint-file relint-directory))
(use-package clang-format :straight t :defer t)
(use-package rmsbolt :straight t :defer t)

(use-package flymake-shellcheck
  :straight t
  :commands flymake-shellcheck-load
  :init
  (add-hook 'sh-mode-hook #'flymake-shellcheck-load))

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

;;;; minor modes

(use-package git-timemachine :straight t :defer t)

(use-package tree-sitter
  :straight t
  :hook (after-init . global-tree-sitter-mode))

(use-package tree-sitter-langs
  :straight t
  :after tree-sitter)

(use-package xterm-color
  :straight t)

(use-package rainbow-mode
  ;; highlight color-strings (hex, etc.)
  :straight t
  :defer t
  :diminish rainbow-mode)

(use-package visual-fill-column
  :straight t
  :config
  (add-hook 'visual-fill-column-mode-hook #'visual-line-mode))

(use-package outline-minor-faces
  :straight t
  :after outline
  :hook (outline-minor-mode . outline-minor-faces-add-font-lock-keywords))

(use-package outshine
  :straight t
  :diminish outshine-mode
  :after outline
  :hook ((prog-mode outline-minor-mode) . outshine-mode)
  :bind
  (:map outshine-mode-map
        ("<tab>" . nil)
        ("<backtab>" . nil)
        ("M-<up>" . nil)
        ("M-<down>" . nil))
  :config
  (setq outshine-fontify-whole-heading-line t)
  (setq outshine-use-speed-commands t)
  (setq outshine-preserve-delimiter-whitespace nil)

  (setq outshine-speed-commands-user '(("g" . consult-outline)))

  ;; fontify the entire outshine-heading, including the comment characters (;;;)
  (patch-add
   #'outshine-fontify-headlines
   "f07111ba85e2f076788ee39af3805516"
   '(font-lock-new-keywords
     `((,heading-1-regexp 1 'outshine-level-1 t)
       (,heading-2-regexp 1 'outshine-level-2 t)
       (,heading-3-regexp 1 'outshine-level-3 t)
       (,heading-4-regexp 1 'outshine-level-4 t)
       (,heading-5-regexp 1 'outshine-level-5 t)
       (,heading-6-regexp 1 'outshine-level-6 t)
       (,heading-7-regexp 1 'outshine-level-7 t)
       (,heading-8-regexp 1 'outshine-level-8 t)))
   '(font-lock-new-keywords
     `((,heading-1-regexp 0 'outshine-level-1 t)
       (,heading-2-regexp 0 'outshine-level-2 t)
       (,heading-3-regexp 0 'outshine-level-3 t)
       (,heading-4-regexp 0 'outshine-level-4 t)
       (,heading-5-regexp 0 'outshine-level-5 t)
       (,heading-6-regexp 0 'outshine-level-6 t)
       (,heading-7-regexp 0 'outshine-level-7 t)
       (,heading-8-regexp 0 'outshine-level-8 t)))))

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
  :straight t
  :defer t
  :bind (("M-<up>" .  sp-backward-barf-sexp)
         ("M-<down>" . sp-forward-barf-sexp)
         ;; ("M-<prior>" . sp-backward-up-sexp)
         ;; ("M-<next>" . sp-up-sexp)
         ("M-<left>" . sp-backward-slurp-sexp)
         ("M-<right>" . sp-forward-slurp-sexp)
         ("M-k" . sp-kill-sexp)
         ("C-M-k" . sp-copy-sexp)
         ("C-S-a" . sp-beginning-of-sexp)
         ("C-S-e" . sp-end-of-sexp)
         ("S-<next>" . sp-split-sexp)
         ("S-<prior>" . sp-join-sexp))
  :config
  (require 'smartparens-config))

(use-package paren-face
  :straight t
  :config
  (setq paren-face-regexp "[][(){}]")
  (global-paren-face-mode +1))

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
  :bind ("C-c C-v" . diff-hl/transient)
  :hook
  ((magit-post-refresh . diff-hl-magit-post-refresh)
   (prog-mode . diff-hl-mode)
   (org-mode . diff-hl-mode))
  :config
  (setq diff-hl-dired-extra-indicators t)
  (setq diff-hl-draw-borders nil)

  (defun diff-hl/first-hunk ()
    (interactive)
    (goto-char (point-min))
    (diff-hl-next-hunk))

  (defun diff-hl/last-hunk ()
    (interactive)
    (goto-char (point-max))
    (diff-hl-previous-hunk))

  (transient-define-prefix diff-hl/transient ()
    "Move to changed VC hunks."
    :transient-non-suffix 'transient--do-call
    ["Commands"
     [("n" "next" diff-hl-next-hunk :transient t)
      ("p" "previous" diff-hl-previous-hunk :transient t)]
     [("f" "first" diff-hl/first-hunk :transient t)
      ("l" "last" diff-hl/last-hunk :transient t)]])

  (global-diff-hl-mode +1))

(use-package diff-hl-dired
  ;; git highlighting for dired-mode, part of `diff-hl'
  :after (dired diff-hl)
  :hook (dired-mode . diff-hl-dired-mode-unless-remote)
  :config
  (defun create-empty-fringe-bitmap (&rest _args)
    "Always use the clean empty BMP for fringe display"
    'empty-fringe-bitmap)

  ;; Use clean fringe style for highlighting
  (setq diff-hl-fringe-bmp-function #'create-empty-fringe-bitmap)

  (patch-add
   #'diff-hl-dired-highlight-items
   "38a4236f8be21aeeb4ab18debf51673c"
   'diff-hl-fringe-bmp-from-type
   'create-empty-fringe-bitmap))

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
        '(((rx "*helpful*") :regexp t :select t :same t :inhibit-window-quit t)
          ((rx "-clojuredocs*") :select t :same t :inhibit-window-quit t)
          ((rx (or "*Apropos*" "*Info*" "*Help*")) :select t :same t :inhibit-window-quit t)
          ((rx "*undo-tree*") :regexp t :select t :align right :inhibit-window-quit t)
          ((rx "*xref*") :regexp t :same t :inhibit-window-quit t)))

  (shackle-mode +1))

(use-package projectile
  :straight t
  :defer t
  :diminish projectile-mode
  :bind
  (("M-p c" . projectile-compile-project)
   ("M-p t" . projectile-test-project)
   ("M-p r" . projectile-run-project)
   ("M-p f" . projectile-find-file)
   ("M-p d" . projectile-dired-other-window)
   ("M-p T" . projectile-toggle-between-implementation-and-test))
  :config
  (setq projectile-enable-caching t)

  (projectile-mode +1))

(use-package keyfreq
  :straight t
  :commands (keyfreq-mode keyfreq-autosave-mode)
  :config
  (keyfreq-mode +1)
  (keyfreq-autosave-mode +1))

;;;; misc packages

(use-package rg :straight t :defer t) ;; ripgrep in emacs
(use-package gist :straight t :defer t) ;; work with github gists
(use-package edit-indirect :straight t)
(use-package spinner :straight t)
(use-package browse-at-remote :straight t)
(use-package org-web-tools :straight t :after org)

(use-package auto-compile
  :straight t
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

  :straight t
  :config

(use-package epithet
  :straight t
  :config
  ;; TODO: rename circe buffers
  (add-hook*
   '(Info-selection-hook
     eww-after-render-hook
     help-mode-hook
     occur-mode-hook
     notmuch-show-mode-hook)
   #'epithet-rename-buffer)

  (with-eval-after-load 'notmuch
    (defun epithet/rename-notuch-show ()
      (when (derived-mode-p 'notmuch-show-mode)
        (format "notmuch: %s" (buffer-name))))

    (add-to-list 'epithet-suggesters #'epithet/rename-notuch-show)
    (add-to-list 'notmuch-show-mode-hook #'epithet-rename-buffer))
  )

(use-package prescient
  ;; functionality to sort candidates
  :straight t
  :demand t)

(use-package company-prescient
  :straight t
  :after (prescient company)
  :config
  (company-prescient-mode +1))

(use-package dumb-jump
  :straight t
  :defer t
  :config
  (setq dumb-jump-selector 'completing-read)
  (setq dumb-jump-prefer-searcher 'rg))

(use-package smart-jump
  :straight t
  :defer t
  :commands (smart-jump-register
             smart-jump-simple-find-references)
  :functions (smart-jump/find-references-with-rg
              smart-jump-refs-search-rg
              smart-jump/select-rg-window)
  :bind*
  (("M-." . smart-jump-go)
   ("M-," . smart-jump-back)
   ("M--" . smart-jump-references))
  :config
  (defun smart-jump/find-references-with-rg ()
    "Use `rg' to find references."
    (interactive)
    (unless (fboundp 'rg)
      (message "Install `rg' to use `smart-jump/find-references-with-rg'."))

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

  (defun smart-jump/select-rg-window nil
    "Select the `rg' buffer, if visible."
    (select-window (get-buffer-window (get-buffer "*rg*"))))

  (setq smart-jump-find-references-fallback-function #'smart-jump/find-references-with-rg)

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
   :heuristic #'smart-jump/select-rg-window)

  (smart-jump-register
   :modes 'c++-mode
   :jump-fn 'dumb-jump-go
   :pop-fn 'pop-tag-mark
   :refs-fn #'smart-jump-simple-find-references
   :should-jump t
   :heuristic #'smart-jump/select-rg-window
   :order 4))

(use-package iedit
  :straight t
  :bind*
  (("C-;" . iedit-mode)
   :map iedit-mode-keymap
   ("<return>" . iedit-quit)))

(use-package multiple-cursors
  :straight t
  :bind*
  (("C-d" . mc/mark-next-like-this)
   ("C-S-d" . mc/mark-previous-like-this)
   ("C-M-a" . mc/mark-all-like-this)))

(use-package ace-mc
  :straight t
  :bind ("C-M-<return>" . ace-mc-add-multiple-cursors))

(use-package browse-kill-ring
  :straight t
  :defer t
  :bind ("C-x C-y" . browse-kill-ring)
  :config (setq browse-kill-ring-quit-action 'save-and-restore))

(use-package avy
  :straight t
  :defer t
  :bind*
  (("C-ø" . avy-goto-char)
   ("C-'" . avy-goto-line))
  :config
  (setq avy-background t)
  (setq avy-single-candidate-jump nil)

  (defun avy/disable-highlight-thing (fn &rest args)
    "Disable `highlight-thing-mode' when avy-goto mode is active,
re-enable afterwards."
    (let ((toggle (bound-and-true-p highlight-thing-mode)))
      (when toggle (highlight-thing-mode -1))
      (unwind-protect
          (apply fn args)
        (when toggle (highlight-thing-mode +1)))))

  (advice-add #'avy-goto-char :around #'avy/disable-highlight-thing))

(use-package avy-zap
  :straight t
  :bind ("C-å" . avy-zap-to-char))

(use-package expand-region
  :straight t
  :bind
  (("M-e" . er/expand-region)
   ("C-M-e" . er/contract-region)))

(use-package move-text
  :straight t
  :bind
  (("C-S-<up>" . move-text-up)
   ("C-S-<down>" . move-text-down)))

(use-package fullframe
  :straight t
  :config
  (fullframe magit-status magit-mode-quit-window))

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
  (global-undo-tree-mode +1))

(use-package goto-chg
  :straight t
  :defer t
  :bind ("M-ø" . goto-last-change))

(use-package beginend
  :straight t
  :demand t
  :diminish beginend-global-mode
  :bind (("M-<" . beginning-of-buffer)
         ("M->" . end-of-buffer))
  :config
  ;; diminish all the `beginend' modes
  (mapc (lambda (s) (diminish (cdr s))) beginend-modes)
  (beginend-global-mode +1))

(use-package which-key
  :straight t
  :diminish which-key-mode
  :config
  (setq which-key-separator "  ")
  (setq which-key-max-description-length 30)
  (setq which-key-show-early-on-C-h t)
  (setq which-key-idle-delay 1)
  (setq which-key-idle-secondary-delay 0.05)
  (setq which-key-show-transient-maps t)

  (which-key-setup-side-window-bottom)
  (which-key-mode +1))

(use-package wgrep
  :straight t
  :defer t
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
              ("-" . grep-context-less-around-point)))

(use-package exec-path-from-shell
  :straight t
  :demand t
  :init
  (setq exec-path-from-shell-check-startup-files nil)
  :config
  (exec-path-from-shell-initialize)
  ;; try to grab the ssh-agent if it is running
  (exec-path-from-shell-copy-env "SSH_AGENT_PID")
  (exec-path-from-shell-copy-env "SSH_AUTH_SOCK"))

(use-package vterm
  :defer t
  :straight (vterm :type git :host github :repo "akermu/emacs-libvterm")
  :bind ("C-z" . vterm)
  :init
  (add-to-list 'load-path (expand-file-name "~/software/emacs-libvterm"))
  :config
  (setq vterm-max-scrollback 10000))

(use-package multi-vterm
  :straight t
  :after vterm
  :bind (("C-z" . multi-vterm)))

(use-package orderless
  :straight t
  :demand t
  :config
  (setq completion-styles '(orderless))
  (setq completion-category-defaults nil)
  (setq completion-category-overrides '((file (styles . (partial-completion))))))

(use-package vertico
  :straight t
  :demand t
  :bind (:map vertico-map
              ("<next>" . vertico-scroll-up)
              ("<prior>" . vertico-scroll-down))
  :config
  (vertico-mode +1))

(use-package marginalia
  :straight t
  :demand t
  :config
  (marginalia-mode +1))

(use-package consult
  :straight t
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :bind
  (("C-s" . consult/search)
   ("C-S-s" . consult/ripgrep)
   ("C-S-f" . consult/fd)
   ("C-x i" . consult-imenu)
   ("C-x I" . consult-project-imenu)
   ("C-x C-i" . consult-outline)
   ("C-x C-b" . consult-buffer)
   ("M-y" . consult-yank-pop)
   ("M-æ" . consult-mark)
   ("M-S-æ" . consult-global-mark)
   ("M-g M-g" . consult-goto-line)
   ("M-y" . consult-yank-pop)
   ("M-b" . consult-bookmark))
  :config
  (setq consult-narrow-key (kbd "C-<"))

  (setq xref-show-xrefs-function #'consult-xref)
  (setq xref-show-definitions-function #'consult-xref)

  (setq completion-in-region-function #'consult-completion-in-region)

  (defun consult/search ()
    (interactive)
    (if (region-active-p)
        (let ((query (buffer-substring-no-properties
                      (region-beginning) (region-end))))
          (deactivate-mark)
          (funcall #'consult-line query))
      (call-interactively #'consult-line)))

  (defun consult/ripgrep ()
    (interactive)
    (if (region-active-p)
        (let ((query (buffer-substring-no-properties
                      (region-beginning) (region-end))))
          (deactivate-mark)
          (funcall #'consult-ripgrep nil query))
      (call-interactively #'consult-ripgrep)))

  (defun consult/fd (&optional dir initial)
    (interactive "P")
    (let ((consult-find-command
           (string-join
            '("fd"
              " --type f --hidden --no-ignore --no-ignore-vcs"
              "--exclude '.git' --exclude 'var/backup/' --exclude 'var/auto-save/' --exclude 'var/undo-tree/'"
              "--exclude 'eln-cache/' --exclude '.python-environments/'"
              "--color never --full-path ARG OPTS")
            " ")))
      (consult-find dir initial)))
  )

(use-package embark
  :straight t
  :bind ("M-a" . embark-act))

(use-package embark-consult
  :straight t
  :after (embark consult))

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

(use-package lsp-mode
  :straight t
  :defer t
  :hook (lsp-mode . lsp-enable-which-key-integration)
  :config
  (setq lsp-eldoc-render-all t)
  (setq lsp-completion-enable-additional-text-edit nil)
  (setq lsp-headerline-breadcrumb-enable nil))

(use-package lsp-ui
  :disabled t
  :straight t
  :after lsp-mode
  :config
  (setq lsp-ui-sideline-show-hover nil)
  (setq lsp-ui-sideline-show-symbol nil)
  (setq lsp-ui-sideline-show-diagnostics nil)
  (setq lsp-ui-sideline-show-code-actions nil)
  (setq lsp-ui-sideline-code-actions-prefix "> "))

(use-package lsp-java
  :straight t
  :after lsp-mode)

(use-package company
  :straight t
  :defer t
  :diminish company-mode
  :bind ("M-<tab>" . #'company/complete)
  :config
  (setq company-search-regexp-function 'company-search-flex-regexp)
  (setq company-require-match nil)
  (setq company-abort-on-unique-match nil)


  (setq company-frontends '())
  (setq company-backends
        '(company-capf
          company-files
          (company-dabbrev-code company-keywords)
          company-dabbrev))

  ;; don't show the company menu automatically
  (setq company-begin-commands nil)
  (setq company-idle-delay nil)
  (setq company-tooltip-idle-delay nil)

  (defun company/complete ()
    "Show company completions using the minibuffer."
    (interactive)
    (company-manual-begin) ;; calculates company-prefix and company-candidates

    (when-let ((pick (completing-read "complete: " company-candidates nil nil company-prefix)))
      (delete-region (- company-point (length company-prefix)) company-point)
      (insert pick)))

  (defun type-of-sym (sym)
    ;; see [[file:~/emacs/build/share/emacs/28.0.50/lisp/help-fns.el.gz::(defun%20help--symbol-completion-table-affixation%20(completions)]]
    (cond
     ((commandp sym) 'command)
     ((macrop sym) 'macro)
     ((functionp sym) 'function)
     ((booleanp sym) 'boolean)
     ((keywordp sym) 'keyword)
     ((keymapp (symbol-value sym)) 'keymap)
     ((facep sym) 'face)
     ((boundp sym) 'variable)
     ((numberp sym) 'number)
     ((symbolp sym) 'symbol)))

  (defun company/complete-xf (s)
    (ignore-errors
      (when (eq major-mode 'emacs-lisp-mode)
        (when-let* ((typ (intern-soft s))
                    (typ (type-of-sym typ))
                    (typ (symbol-name typ))
                    (typ (propertize typ 'face font-lock-comment-face))
                    (padding (s-repeat (max 5 (- 50 (length s))) " ")))
          (setq s (concat s padding typ))))
      s))

  (global-company-mode +1))

  :straight t
  :config


;;; keybindings

(log-info "Setting keybindings")

;; keys for quickly going to common files
(bind-key* "\e\ei" (xi (find-file "~/vault/inbox/inbox.org")))
(bind-key* "\e\ek" (xi (find-file "~/vault/org/tracking.org")))
(bind-key* "\e\em" (xi (find-file "~/vault/org/roadmap.org")))
(bind-key* "\e\et" (xi (find-file "~/vault/inbox/today.org")))
(bind-key* "\e\er" (xi (find-file "~/vault/org/read.org")))
(bind-key* "\e\ew" (xi (find-file "~/vault/org/watch.org")))
(bind-key* "\e\ec" (xi (find-file "~/.emacs.d/init.el")))

;;;; for built-in things

;; Easily mark the entire buffer
(bind-key* "C-x a" #'mark-whole-buffer)

;; Quit emacs, mnemonic is C-x REALLY QUIT
(bind-key* "C-x r q" #'save-buffers-kill-terminal)
;; Kill emacs, mnemonic is C-x REALLY KILL
(bind-key* "C-x r k" #'save-buffers-kill-emacs)

(bind-key* "C-c C-SPC" #'pop-to-mark-command)

(bind-key "M-f" #'find-function help-map)
(bind-key "M-v" #'find-variable help-map)
(bind-key "b" #'describe-bindings help-map)

;; Evaluate the current buffer/region
(bind-key "C-c C-k" #'eval-buffer)
(bind-key "C-c k" #'eval-region)

;; Casing words/regions
(bind-key* "M-u" #'upcase-dwim)
(bind-key* "M-l" #'downcase-dwim)
(bind-key* "M-c" #'capitalize-dwim)

(bind-key* "C-M-n" #'forward-paragraph)
(bind-key* "C-M-p" #'backward-paragraph)

(bind-key* "C-c g" #'revert-buffer)

;; Scroll the buffer without moving the point (unless we over-shoot)
(bind-key* "C-<up>" (xi (scroll-down 5)))
(bind-key* "C-<down>" (xi (scroll-up 5)))

;; don't close emacs
(unbind-key "C-x C-c")

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

;; don't suspend-frame
(unbind-key "C-x C-z")

;; Make Home and End go to the top and bottom of the buffer, we have C-a/e for lines
(bind-key* "<home>" 'beginning-of-buffer)
(bind-key* "<end>" 'end-of-buffer)

;;; epilogue

(log-success "Configuration is %s lines of emacs-lisp. (excluding 3rd-parties)"
             (jens/emacs-init-loc))
(log-success "Initialized in %s, with %s garbage collections.\n"
             (emacs-init-time) gcs-done)

(defun jens/show-initial-important-messages ()
  "Show all lines in *Messages* matching a regex for important messages."
  (let* ((regex
          (rx (or
               ;; show info about loaded files with auto-save data
               "recover-this-file"
               ;; show warnings and errors that occurred during init
               (seq bol "! ")
               (seq bol "~ ")
               ;; show all lines containing the word `warning' or `error'
               "warning"
               "error")))
         (important
          (with-temp-buffer
            (insert-buffer-substring "*Messages*")
            (delete-non-matching-lines regex (point-min) (point-max))
            (buffer-string))))
    (when (not (string-empty-p important))
      (with-current-buffer (get-buffer-create "*Important Messages*")
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert important))
        (view-buffer-other-window (current-buffer)))))
  ;; only show the messages buffer in the first frame created
  (remove-hook #'server-after-make-frame-hook #'jens/show-initial-important-messages))

(add-hook #'server-after-make-frame-hook #'jens/show-initial-important-messages)

;; TODO: get buffer string without `with-current-buffer', to speed things up?
;; TODO: colorize message/compile buffer
;; TODO: add separator (^L / newline?) in compile mode
;; TODO: test with ert?
;; TODO: maybe autoinsert? https://www.gnu.org/software/emacs/manual/html_node/autotype/Autoinserting.html
;; TODO: look at doom-emacs
;; TODO: notify someone about the auto-revert + tramp hangs
;; TODO: figure out how to default username on tramp ssh access using .ssh/config User entry

;; TODO: using (if-let ((_ ...))) gives the warning "variable ‘_’ not left unused"
(provide 'init)
;;; init.el ends here
