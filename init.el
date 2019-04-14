;;; -*- lexical-binding: t -*-
;; use lexical binding for initialization code
(setq-default lexical-binding t)

;; some functions for logging
(defun msg-info (txt) (message "# %s" txt))
(defun msg-warning (txt) (message "! %s" txt))
(defun msg-success (txt) (message "@ %s" txt))

(msg-info "Started initializing emacs!")

;; turn off excess interface early in startup to avoid momentary display
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)

;; directories for elisp things
(defconst user-emacs-elpa-dir (locate-user-emacs-file "elpa/"))
(defconst user-emacs-lisp-dir (locate-user-emacs-file "lisp/"))

;; add user directories to the load-path
(add-to-list 'load-path user-emacs-lisp-dir)
(add-to-list 'load-path user-emacs-elpa-dir)

;; setup package archives
(setq-default package-archives
              '(("gnu" . "https://elpa.gnu.org/packages/")
                ("melpa-stable" . "https://stable.melpa.org/packages/")
                ("melpa" . "https://melpa.org/packages/")
                ("org" . "https://orgmode.org/elpa/")))

;; make sure use-package is installed
(unless (package-installed-p 'use-package)
  (msg-warning "use-package.el was not found. installing...")
  (package-refresh-contents)
  (package-install 'use-package))

;; need to enable imenu support before requiring `use-package'
(setq use-package-enable-imenu-support t)
;; make use-package tell us what its doing
(setq use-package-verbose t)
(setq use-package-compute-statistics t)

(require 'use-package)

;;; make sure straight.el is installed
(defvar bootstrap-version)
(let ((bootstrap-file
       (locate-user-emacs-file "straight/repos/straight.el/bootstrap.el"))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (msg-warning "straight.el was not found, installing.")
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; functional things, -map, -concat, etc
(use-package dash
  :straight (dash :host github :repo "magnars/dash.el"
                  :fork (:host github :repo "jensecj/dash.el")))

;; string things, s-trim, s-replace, etc.
(use-package s
  :straight (s :host github :repo "magnars/s.el"
               :fork (:host github :repo "jensecj/s.el")))

;; file-system things, f-exists-p, f-base, etc.
(use-package f
  :straight (f :host github :repo "rejeep/f.el"
               :fork (:host github :repo "jensecj/f.el")))

;; a great hash-table wrapper.
(use-package ht
  :straight (ht :host github :repo "Wilfred/ht.el"
                :fork (:host github :repo "jensecj/ht.el")))

(defun jens/init-fonts ()
  "Setup font configuration for new frames."
  (let ((my-font "Source Code Pro Semibold 10"))
    (if (not (find-font (font-spec :name my-font)))
        (msg-warning (format "could not find font: %s" my-font))
      (add-to-list 'default-frame-alist `(font . ,my-font))
      (set-frame-font my-font)

      ;; only setup fonts once
      (remove-hook 'server-after-make-frame-hook #'jens/init-fonts))))

(add-hook 'server-after-make-frame-hook #'jens/init-fonts)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cache, temp files, etc. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; contain extra files in etc/ and var/.
;; load early, and overwrite locations in configs if needed.
(use-package no-littering :ensure t :demand t)

(setq temporary-file-directory (no-littering-expand-var-file-name "temp/"))
(setq bookmark-default-file (no-littering-expand-etc-file-name "bookmarks.el"))

(let ((auto-save-dir (no-littering-expand-var-file-name "auto-save/")))
  (setq auto-save-file-name-transforms `((".*" ,auto-save-dir t)))
  (setq auto-save-list-file-prefix auto-save-dir))

;; keep emacs custom settings in a separate file, and load it if it exists.
(setq custom-file (locate-user-emacs-file "custom.el"))
(if (file-exists-p custom-file)
    (load custom-file))

;;;;;;;;;;;;;;
;; settings ;;
;;;;;;;;;;;;;;

;; location of emacs source files
(let ((src-dir "/home/jens/.aur/emacs-git-src/"))
  (if (f-exists-p src-dir)
      (setq source-directory src-dir)
    (msg-warning "Unable to locate emacs source directory.")))

;; hide the splash screen
(setq inhibit-startup-message t)

;; set the paranoia level to medium, warns if connections are insecure
(setq network-security-level 'medium)

;; don't disable function because they're confusing to beginners
(setq disabled-command-function nil)

;; never use dialog boxes
(setq use-dialog-box nil)

;; load newer files, even if they have outdated byte-compiled counterparts
(setq load-prefer-newer t)

;; don't blink the cursor
(blink-cursor-mode -1)

;; always highlight current line
(global-hl-line-mode 1)

;; allow pasting selection outside of Emacs
(setq select-enable-clipboard t)

;; show keystrokes in progress
(setq echo-keystrokes 0.1)

;; move files to trash when deleting
(setq delete-by-moving-to-trash t)

;; don't use shift to mark things
(setq shift-select-mode nil)

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

;; lines should be 80 characters wide
(setq-default fill-column 80)

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
(setq-default indicate-empty-lines t)

;; don't automatically break lines
(setq-default truncate-lines t)

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
(setq undo-limit (* 5 1024 1024))
(setq undo-strong-limit (* 20 1024 1024))

;; remember a lot of messages
(setq message-log-max 10000)

(setq auto-window-vscroll nil)

;; if moving the point more than 10 lines away,
;; center point in the middle of the window, otherwise be conservative.
(setq scroll-conservatively 10)

;; only scroll the current line when moving outside window-bounds
(setq auto-hscroll-mode 'current-line)

;; save clipboard from other programs to kill-ring
(setq save-interprogram-paste-before-kill t)

;; always follow symlinks
(setq vc-follow-symlinks t)

;; just give me a clean scratch buffer
(setq initial-scratch-message "")

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

(setq auth-sources '("~/vault/authinfo.gpg" "~/.netrc"))

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

(defun jens/kill-idle-gpg-buffers ()
  "Kill .gpg buffers after they have not been used for 60
seconds."
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

(defun jens/create-scratch-buffer ()
  "Create a new scratch buffer to work in.  (named *scratch* -
*scratch<n>*)."
  (interactive)
  (let ((scratch-buf (jens/new-scratch-buffer))
        (initial-content (if (use-region-p)
                             (buffer-substring (region-beginning) (region-end)))))
    (xref-push-marker-stack)
    (switch-to-buffer scratch-buf)
    (when initial-content (insert initial-content))
    (goto-char (point-min))
    (emacs-lisp-mode)))

(defun jens/clean-view ()
  "Create a scratch buffer, and make it the only buffer visible."
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

;; TODO: use reformatter.el?
(defun jens/cleanup-buffer ()
  "Perform a bunch of operations on the white space content of a buffer.
Including indent-buffer, which should not be called automatically
on save."
  (interactive)
  (indent-region (point-min) (point-max))
  (whitespace-cleanup)
  (message "cleaned up"))

(defun jens/sudo-find-file (filename)
  "Open FILENAME with superuser permissions."
  (let ((remote-method (file-remote-p default-directory 'method))
        (remote-user (file-remote-p default-directory 'user))
        (remote-host (file-remote-p default-directory 'host))
        (remote-localname (file-remote-p filename 'localname)))
    (find-file (format "/%s:root@%s:%s"
                       (or remote-method "sudo")
                       (or remote-host "localhost")
                       (or remote-localname filename)))))

(defun jens/sudo-edit (&optional arg)
  "Re-open current buffer file with superuser permissions.
With prefix ARG, ask for file to open."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (read-file-name "Find file:"))

  (let ((place (point)))
    (jens/sudo-find-file buffer-file-name)
    (goto-char place)))

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
line. Otherwise join the line below the current line, with the
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
      (previous-line)
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
  "Get the file name and extension of the file belonging to the
current buffer."
  (file-name-nondirectory buffer-file-name))

(defun jens/get-buffer-file-name ()
  "Get the file name of the file belonging to the current
buffer."
  (file-name-sans-extension (jens/get-buffer-file-name+ext)))

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

(defun jens/new-package ()
  "Create a skeleton for a elisp package."
  (interactive)
  (let* ((path (read-file-name "package name: "))
         (file (f-filename path))
         (name (f-no-ext file))
         (p))
    (with-current-buffer (find-file-noselect path)
      (insert (format ";;; %s. --- -*- lexical-binding: t; -*-\n\n" file))
      (insert (format ";; Copyright (C) %s %s\n\n"
                      (format-time-string "%Y")
                      user-full-name))

      (insert (format ";; Author: %s <%s>\n" user-full-name user-mail-address))
      (insert ";; Keywords:\n")
      (insert (format ";; Package-Version: %s\n" (format-time-string "%Y%m%d")))
      (insert ";; Version: 0.1\n\n")

      (insert ";; This file is NOT part of GNU Emacs.\n\n")

      (insert ";;; Commentary:\n\n")
      (insert ";;; Code:\n\n")

      (setq p (point))

      (insert "\n\n")

      (insert (format "(provide '%s)" name))

      (goto-char p)
      (switch-to-buffer (current-buffer)))))

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

(defun jens/inspect-variable-at-point (&optional arg)
  "Inspect variable at point."
  (interactive "P")
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

      ;; TODO: create a posframe for all-purpose emacs things

      (funcall doc-at-point-display-fn
               (doc-at-point-elisp--fontify-as-code
                (with-output-to-string
                  (pp value)))))))

(defun jens/one-shot-keybinding (key command)
  "Set a keybinding that disappear once you press a key that is
not in the overlay-map"
  (set-transient-map
   (let ((map (make-sparse-keymap)))
     (define-key map (kbd key) command)
     map) t))
;; example
;; (jens/one-shot-keybinding "a" (xi (previous-line)))

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

(defun jens/remove-text-properties-region (beg end)
  "Remove text properties from text in region between BEG and END."
  (set-text-properties beg end nil))

(defun jens/remove-text-propertiex-in-region ()
  "Remove text propertiex from all text in active region."
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
  (let ((beg (or beg (region-beginning)))x
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

;;;;;;;;;;;;;;;;;
;; misc defuns ;;
;;;;;;;;;;;;;;;;;

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
  "Return function definitoin of FNSYM as an org code-block."
  (concat "#+begin_src emacs-lisp\n"
          (jens/function-def-string fnsym)
          "#+end_src"))

(defun jens/copy-symbol-at-point ()
  "Save the `symbol-at-point' to the kill-ring."
  (interactive)
  (let ((sym (symbol-at-point)))
    (kill-new (symbol-name sym))))

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
         (pick (completing-read "Repo:" repos-propped nil t)))
    (cond
     ((f-directory? pick) (dired pick))
     ((f-file? pick) (find-file pick))
     (t (message "unknown repo: %s" pick)))))

(defun jens/load-secrets ()
  "Eval everything in secrets.el.gpg."
  (interactive)
  (with-current-buffer
      (find-file-noselect (locate-user-emacs-file "secrets.el.gpg"))
    (eval-buffer)
    (kill-current-buffer)))

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

;;;;;;;;;;;;;;
;; packages ;;
;;;;;;;;;;;;;;

;; We are going to use the bind-key (`:bind') and diminish (`:diminish')
;; extensions of `use-package', so we need to have those packages.
(use-package bind-key :ensure t)
(use-package diminish :ensure t :commands diminish)
(use-package delight :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; some built-in packages ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;; insert parens-type things in pairs
(use-package elec-pair
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

;; show matching parens
(use-package paren
  :config
  (setq show-paren-delay 0.1)
  (setq show-paren-when-point-inside-paren t)

  (show-paren-mode +1))

;; auto-replace common abbreviations
(use-package abbrev
  :demand t
  :diminish abbrev-mode
  :hook (org-mode . abbrev-mode)
  :commands read-abbrev-file
  :config
  (setq abbrev-file-name (no-littering-expand-etc-file-name "abbreviations.el"))
  (read-abbrev-file)
  (abbrev-mode +1))

;; easily navigate silly cased words
(use-package subword
  :diminish subword-mode
  :commands global-subword-mode
  :config (global-subword-mode 1))

;; give buffers unique names
(use-package uniquify
  :config (setq uniquify-buffer-name-style 'forward))

;; easily access and edit files on remote machines
(use-package tramp
  :defer t
  :config
  (setq tramp-persistency-file-name (no-littering-expand-var-file-name "tramp"))
  (setq tramp-terminal-type "tramp")
  (setq tramp-verbose 6))

;; save point position between sessions
(use-package saveplace
  :config
  (setq-default save-place t)
  (setq save-place-file (no-littering-expand-var-file-name "saveplaces")))

;; persist some variables between sessions
(use-package savehist
  :defer 2
  :commands savehist-mode
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

;; save a list of recently visited files.
(use-package recentf
  :commands recentf-mode
  :config
  ;; TODO: maybe move to var directory?
  (setq recentf-save-file
        (recentf-expand-file-name (no-littering-expand-etc-file-name "recentf.el")))
  (setq recentf-exclude
        `(,(regexp-quote
            (locate-user-emacs-file no-littering-var-directory))
          "COMMIT_EDITMSG"))

  ;; save a bunch of recent items
  (setq recentf-max-saved-items 500)

  ;; clean the list every 5 minutes
  (setq recentf-auto-cleanup 300)

  ;; save recentf file every 30s, but don't bother us about it
  (setq recentf-auto-save-timer
        (run-with-idle-timer 30 t (lambda ()
                                    (shut-up (recentf-save-list)))))

  (defun jens/recentf-cleanup (orig-fun &rest args)
    "Silence `recentf-auto-cleanup'."
    (shut-up (apply orig-fun args)))
  (advice-add 'recentf-cleanup :around #'jens/recentf-cleanup)

  (recentf-mode +1))

;; always show the version of a file as it appears on disk
(use-package autorevert
  :diminish auto-revert-mode
  :commands global-auto-revert-mode
  :config
  ;; also auto refresh dired, but be quiet about it
  (setq global-auto-revert-non-file-buffers t)
  (setq auto-revert-verbose nil)

  ;; auto refresh buffers
  (global-auto-revert-mode 1))

(use-package replace
  :bind
  (("C-c r" . jens/replace)
   ("C-c q" . jens/query-replace))
  :config
  (defun jens--replace (fn)
    "Get replace arguments and delegate to replace FN."
    (let ((from (if (use-region-p)
                    (buffer-substring (region-beginning) (region-end))
                  (completing-read "Replace: " '())))
          (to (completing-read "Replace with: " '())))
      (deactivate-mark)
      (save-excursion
        (funcall fn from to nil (point-min) (point-max)))))

  (defun jens/replace ()
    "Replace occurance of regexp in the entire buffer."
    (interactive)
    (jens--replace #'replace-regexp))

  (defun jens/query-replace ()
    "Interactively replace occurance of regexp in the entire buffer."
    (interactive)
    (jens--replace #'query-replace-regexp)))

;; semantic analysis in supported modes (cpp, java, etc.)
(use-package semantic
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
  (add-hook 'compilation-mode-hook 'visual-line-mode)

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

(use-package hi-lock
  :diminish hi-lock-mode)

;; easily handle merge conflicts
(use-package smerge-mode
  :bind (:map smerge-mode-map ("C-c ^" . jens/smerge/body))
  :config
  (defhydra jens/smerge ()
    "Move between buffers."
    ("n" #'smerge-next "next")
    ("p" #'smerge-prev "previous")
    ("u" #'smerge-keep-upper "keep upper")
    ("l" #'smerge-keep-lower "keep lower"))

  (defun jens/enable-smerge-if-diff-buffer ()
    "Enable SMerge-mode if the current buffer is showing a diff."
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^<<<<<<< " nil t)
        (smerge-mode 1))))

  (add-hook 'find-file-hook 'jens/enable-smerge-if-diff-buffer t)
  (add-hook 'after-revert-hook 'jens/enable-smerge-if-diff-buffer t))

(use-package elisp-mode
  :delight (emacs-lisp-mode "Elisp" :major))

;; show useful contextual information in the minibuffer
(use-package eldoc
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

  (require 'smartparens)

  (defun jens/lispify-eldoc-message (eldoc-msg)
    "Change the format of eldoc messages for functions to `(fn args)'."
    (if (and eldoc-msg
             (member major-mode sp-lisp-modes))
        (let* ((parts (s-split ": " eldoc-msg))
               (sym (car parts))
               (args (cadr parts)))
          (cond
           ((string= args "()") (format "(%s)" sym))
           (t (format "(%s %s)" sym (substring args 1 (- (length args) 1))))))
      eldoc-msg))
  (advice-add #' elisp-get-fnsym-args-string :filter-return #'jens/lispify-eldoc-message)

  (global-eldoc-mode +1)
  :custom-face
  (eldoc-highlight-function-argument ((t (:inherit font-lock-warning-face))))
  (eldoc-highlight-&s-face ((t (:inherit font-lock-preprocessor-face)))))

(use-package dired+
  :straight (dired+ :type git :host github :repo "emacsmirror/dired-plus")
  :after dired
  :defer t
  :commands (toggle-diredp-find-file-reuse-dir
             diredp-up-directory-reuse-dir-buffer)
  :bind
  (:map dired-mode-map
        ("<backspace>" . diredp-up-directory-reuse-dir-buffer))
  :config
  (unbind-key "C-<up>" dired-mode-map)
  (unbind-key "C-<down>" dired-mode-map)
  (toggle-diredp-find-file-reuse-dir +1)
  :custom-face
  (diredp-dir-priv ((t (:foreground "#8CD0D3"))))
  (diredp-file-name ((t (:foreground "#DCDCCC"))))
  (diredp-dir-name ((t (:foreground "#8CD0D3")))))

(use-package dired-ranger
  :ensure t
  :bind
  (:map dired-mode-map
        ("c" . dired-ranger-copy)
        ("p" . dired-ranger-paste)))

(use-package dired
  :defer t
  :commands dired
  :bind
  (("C-x C-d" . dired-jump)
   :map dired-mode-map
   ("C-." . dired-omit-mode)
   ("SPC" . jens/dired-toggle-mark)
   ("T" . dired-create-empty-file))
  :config
  ;; pull in extra functionality for dired
  (load-library "dired-x")
  (load-library "dired-aux")
  (require 'dired+)

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

  (advice-add 'dired-readin :after #'jens/dired-sort)

  (defun jens/dired-toggle-mark (arg)
    "Toggle mark on the current line."
    (interactive "P")
    (let ((this-line (buffer-substring (line-beginning-position) (line-end-position))))
      (if (s-matches-p (dired-marker-regexp) this-line)
          (dired-unmark arg)
        (dired-mark arg)))))

(use-package browse-url
  :defer t
  :config (setq browse-url-firefox-program "firefox"))

(use-package fringe
  :commands fringe-mode
  :config
  (fringe-mode '(5 . 0))

  (defun jens/toggle-window-margins ()
    "Toggle left and right window margins, centering `fill-column' lines."
    (interactive)
    (let ((margins (window-margins)))
      (if (and (null (car margins)) (null (cdr margins)))
          (let* ((win-width (window-width (selected-window)))
                 (margin (/ (- win-width fill-column) 2)))
            (set-window-margins (selected-window) margin margin))
        (set-window-margins (selected-window) 0 0))))
  :custom-face
  (fringe ((t (:background "#3f3f3f")))))

;;;;;;;;;
;; Org ;;
;;;;;;;;;

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
   ("M-<prior>" . org-move-subtree-up)
   ("C-c n" . jens/org-indent))
  :config
  (setq org-extra-electric-pairs '((?\$ . ?\$)))

  (setq org-log-done 'time)

  (defun jens/org-summary-todo (n-done n-not-done)
    "Switch entry to DONE when all subentries are done, to TODO otherwise."
    (let (org-log-done org-log-states)   ; turn off logging
      (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

  (add-hook 'org-after-todo-statistics-hook 'jens/org-summary-todo)

  (defun jens/org-add-electric-pairs ()
    (setq-local electric-pair-pairs (-concat org-extra-electric-pairs electric-pair-pairs)))

  (add-hook 'org-mode-hook 'jens/org-add-electric-pairs)

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

  (org-babel-do-load-languages 'org-babel-load-languages
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

  (setq org-html-head "<style type=\"text/css\">body {max-width: 800px; margin: 0 auto;} img {max-width: 100%;}</style>")

  ;;;;;;;;;;;;
  ;; filing ;;
  ;;;;;;;;;;;;

  (setq org-outline-path-complete-in-steps t)
  (setq org-refile-allow-creating-parent-nodes (quote confirm))
  (setq org-refile-use-outline-path t)
  (setq org-refile-targets '( (nil . (:maxlevel . 1))))

  (defhydra jens/org-today-refile-pl (:foreign-keys run)
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

    ("x" jens/org-today-refile/body "Back to refiling hydra" :exit t)
    ("z" org-refile-goto-last-stored "Jump to last refile")
    ("q" nil "quit"))

  (defhydra jens/org-today-refile (:foreign-keys run)
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

    ("x" jens/org-today-refile-pl/body "Refile programming languages" :exit t)
    ("z" org-refile-goto-last-stored "Jump to last refile")
    ("q" nil "quit")))

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
         ("\\.PKGBUILD\\'" . shell-script-mode))
  :config
  (add-hook 'sh-mode-hook #'flymake-mode)
  (add-hook 'sh-mode-hook #'flycheck-mode))

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

(use-package cc-mode
  :bind*
  (:map java-mode-map
        ("C-c C-c" . projectile-compile-project))
  (:map c++-mode-map
        ("C-c C-c" . projectile-compile-project)
        ("C-c n" . clang-format-buffer))
  :config
  (dolist (k '("\M-," "\M-." "\M--"))
    (bind-key k nil c-mode-base-map)))

(use-package make-mode
  :config
  ;; TODO: fix indentation
  )

;; from repos
(use-package lsp-mode :defer t :ensure t)
(use-package cmake-mode :ensure t :mode "\\CmakeLists.txt\\'")
(use-package dockerfile-mode :ensure t :mode "\\Dockerfile\\'")
(use-package gitconfig-mode :ensure t :mode "\\.gitconfig\\'")
(use-package gitignore-mode :ensure t :mode "\\.gitignore\\'")
(use-package haskell-mode :ensure t :mode "\\.hs\\'")
(use-package lua-mode :ensure t :mode "\\.lua\\'")
(use-package markdown-mode :ensure t :mode ("\\.md\\'" "\\.card\\'"))
(use-package scss-mode :ensure t :mode "\\.scss\\'")
(use-package tuareg :ensure t :mode ("\\.ml\\'" "\\.mli\\'" "\\.mli\\'" "\\.mll\\'" "\\.mly\\'"))

(use-package rust-mode
  :ensure t
  :defer t
  :bind
  (:map rust-mode-map
        ("C-c n" . rust-format-buffer))
  :mode "\\.rs\\'"
  :config
  (unbind-key "M-," rust-mode-map)
  (unbind-key "M-." rust-mode-map)
  (unbind-key "M--" rust-mode-map))

(use-package clojure-mode
  :ensure t
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; extensions to major modes ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package slime
  :defer t
  :ensure t
  :commands (jens/qlot-slime slime-start)
  :config
  (setq inferior-lisp-program "sbcl")
  (setq slime-contribs '(slime-fancy))

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
  ;; don't warn on refactor evals
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
  :hook ((rust-mode . racer-mode)
         (racer-mode . eldoc-mode)))

(use-package elpy
  :ensure t
  :defer t
  :delight " elpy "
  :commands (elpy-goto-definition)
  :hook (python-mode . elpy-mode)
  :custom
  (elpy-modules
   '(elpy-module-sane-defaults
     elpy-module-company
     elpy-module-eldoc
     elpy-module-pyvenv)))

(use-package highlight-defined
  :ensure t
  :diminish highlight-defined-mode
  :hook (emacs-lisp-mode . highlight-defined-mode)
  :custom-face
  (highlight-defined-function-name-face ((t (:foreground "#BDE0F3"))))
  (highlight-defined-builtin-function-name-face ((t (:foreground "#BFBFBF")))))

(use-package highlight-thing
  :ensure t
  :diminish highlight-thing-mode
  :config
  (setq highlight-thing-ignore-list '("nil" "t"))
  (setq highlight-thing-delay-seconds 0.5)
  (setq highlight-thing-case-sensitive-p nil)
  (setq highlight-thing-exclude-thing-under-point nil)
  (global-highlight-thing-mode +1)
  :custom-face
  (highlight-thing ((t (:background "#4f4f4f" :weight bold)))))

(use-package emr
  :ensure t
  :bind
  (:map emacs-lisp-mode-map
        ("M-<return>" . emr-show-refactor-menu)))

(use-package package-lint :ensure t :defer t :commands (package-lint-current-buffer))
(use-package flycheck-package :ensure t :defer t :commands (flycheck-package-setup))

(use-package ggtags
  :ensure t
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

(use-package rtags :ensure t :defer t :disabled t)
(use-package clang-format :ensure t :defer t)

(use-package flymake-shellcheck
  :ensure t
  :commands flymake-shellcheck-load
  :init
  (add-hook 'sh-mode-hook 'flymake-shellcheck-load))

;;;;;;;;;;;;;;;;;;;;;
;; auto completion ;;
;;;;;;;;;;;;;;;;;;;;;

(use-package company
  :ensure t
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

(use-package company-box
  :ensure t
  :demand t
  :after company
  :config
  (setq company-box-enable-icon t)
  (setq company-box-show-single-candidate t)

  (defun jens/company-box-icon-elisp (sym)
    (when (derived-mode-p 'emacs-lisp-mode)
      (let ((sym (if (stringp sym) (intern sym))))
        (cond
         ((functionp sym) (propertize "f" 'face font-lock-function-name-face))
         ((macrop sym) (propertize "m" 'face font-lock-keyword-face))
         ((boundp sym) (propertize "v" 'face font-lock-variable-name-face))
         (t "")))))

  (delete 'company-box-icons--elisp company-box-icons-functions)
  (add-to-list 'company-box-icons-functions #'jens/company-box-icon-elisp)

  (setf (map-elt company-box-frame-parameters 'side) 0.2)
  (setf (map-elt company-box-frame-parameters 'min-width) 40)

  (company-box-mode +1)

  :custom-face
  (company-box-selection ((t (:foreground nil :background "black")))))

(use-package company-lsp
  :ensure t
  :config
  (push 'company-lsp company-backends))

(use-package company-flx
  :ensure t
  :hook (company-mode . company-flx-mode))

(use-package company-c-headers
  :ensure t
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

;;;;;;;;;;;;;;;;;;;
;; misc packages ;;
;;;;;;;;;;;;;;;;;;;

(use-package flx :ensure t) ;; fuzzy searching for ivy, etc.
(use-package fzf :ensure t) ;; fuzzy file finder
(use-package rg :ensure t :commands (rg-read-pattern rg-project-root rg-default-alias rg-run)) ;; ripgrep in emacs
(use-package flycheck :ensure t :defer t)
(use-package git-timemachine :ensure t :defer t)
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

(use-package spinner
  :ensure t
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

(use-package yasnippet
  :ensure t
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
  :init
  (let ((help-fns-plus-file (concat user-emacs-elpa-dir "/help-fns+.el"))
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
  ;; TODO: figure out how to disable epdf asking to rebuild when starting
  ;; emacsclient, it does not work.

  ;; (pdf-tools-install)
  )

(use-package amx
  :ensure t
  :commands amx-mode
  :config
  (amx-mode))

(use-package smartparens
  :ensure t
  :defer t
  :bind (("M-<up>" .  sp-backward-barf-sexp)
         ("M-<down>" . sp-forward-barf-sexp)
         ("M-<left>" . sp-backward-slurp-sexp)
         ("M-<right>" . sp-forward-slurp-sexp)
         ("C-S-a" . sp-beginning-of-sexp)
         ("C-S-e" . sp-end-of-sexp)
         ("S-<next>" . sp-split-sexp)
         ("S-<prior>" . sp-join-sexp)))

(use-package smart-jump
  :straight t
  :demand t
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
  :demand t
  :diminish hl-todo-mode
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
  (defalias #'describe-key #'helpful-key))

(use-package elisp-demos
  :straight t
  :after helpful
  :config
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))

(use-package iedit
  :straight t
  :bind ("C-;" . iedit-mode))

(use-package smartscan
  :straight t
  :bind (("C->" . smartscan-symbol-go-forward)
         ("C-<" . smartscan-symbol-go-backward)))

(use-package multiple-cursors
  :ensure t
  :bind
  (("C-d" . mc/mark-next-like-this)
   ("C-S-d" . mc/mark-previous-like-this)
   ("C-M-a" . mc/mark-all-like-this)
   ("C-M-<return>" . mc-hydra/body))
  :init
  (setq mc/list-file (no-littering-expand-etc-file-name "mc-lists.el"))
  :config
  (defhydra mc-hydra ()
    "
^Next^                ^Previous^              ^Lines^
^^^^^^^^----------------------------------------------------
_n_: Mark next        _p_: Mark previous      _b_: Edit beginning of lines
_N_: Skip to next     _P_: Skip to previous   _e_: Edit ends of lines
_M-n_: Unmark next    _M-p_: Unmark previous  ^ ^
"
    ("n" #'mc/mark-next-like-this)
    ("N" #'mc/skip-to-next-like-this)
    ("M-n" #'mc/unmark-next-like-this)
    ("p" #'mc/mark-previous-like-this)
    ("P" #'mc/skip-to-previous-like-this)
    ("M-p" #'mc/unmark-previous-like-this)

    ("b" #'mc/edit-beginnings-of-lines)
    ("e" #'mc/edit-ends-of-lines)))

(use-package browse-kill-ring
  :ensure t
  :defer t
  :bind ("C-x C-y" . browse-kill-ring)
  :config (setq browse-kill-ring-quit-action 'save-and-restore))

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

(use-package fullframe
  :ensure t
  :commands fullframe/maybe-restore-configuration
  :config
  (fullframe magit-status magit-mode-quit-window)
  (fullframe list-packages quit-window))

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
  ;; TODO: replace with forge.el?
  :ensure t
  :after magit
  :config
  (magithub-feature-autoinject t))

(use-package magit-todos
  :ensure t
  :after magit
  :commands magit-todos-mode
  :config
  (magit-todos-mode))

(use-package undo-tree
  :ensure t
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

(use-package fold-this
  :ensure t
  :bind
  (([(meta shift h)] . fold-this-unfold-all)
   ("M-h" . fold-this)
   (:map emacs-lisp-mode-map ("M-h" . fold-this-sexp)))
  :custom-face
  (fold-this-overlay ((t (:foreground nil :background "#5f5f5f")))))

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

(use-package elfeed
  :ensure t
  :defer t
  :after today
  :commands (elfeed elfeed-search-selected)
  :functions jens/elfeed-copy-link-at-point
  :bind
  (:map elfeed-search-mode-map
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

  (setq elfeed-feeds (jens/load-from-file (locate-user-emacs-file "elfeeds.el")))

  (defun jens/elfeed-select-emacs-after-browse-url (fn &rest args)
    "Activate the emacs-window"
    (let* ((emacs-window (shell-command-to-string "xdotool getactivewindow"))
           (active-window "")
           (counter 0))
      (apply fn args)
      (sleep-for 0.5)
      (while (and (not (string= active-window emacs-window))
                  (< counter 5))
        (sleep-for 0.2)
        (incf counter)
        (setq active-window (shell-command-to-string "xdotool getactivewindow"))
        (shell-command-to-string (format "xdotool windowactivate %s" emacs-window)))))

  (advice-add #'elfeed-search-browse-url :around #'jens/elfeed-select-emacs-after-browse-url)

  (defun jens/elfeed-copy-link-at-point ()
    "Copy the link of the elfeed entry at point to the
clipboard."
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

  (setq erc-user-full-name user-full-name)
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
  :defer t
  :commands (multi-term-get-buffer
             multi-term-internal)
  :bind ("C-z" . jens/multi-term)
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
  :ensure t
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
   ("C-d" . (lambda () (interactive) (ivy-quit-and-run (dired ivy--directory))))
   ("C-S-<return>" . ivy-immediate-done)
   :map ivy-occur-grep-mode-map
   ("d" . ivy-occur-delete-candidate))
  :config
  (setq ivy-height 15)
  (setq ivy-count-format "")
  (setq ivy-use-virtual-buffers t)
  (setq ivy-re-builders-alist '((t . ivy--regex-plus)))

  (ivy-mode)
  :custom-face
  (ivy-current-match ((t (:foreground nil :background "#313131" :box t :underline nil)))))

(use-package ivy-rich
  :ensure t
  :commands ivy-rich-mode
  :config
  (defun ivy-rich-bookmark-context-string (candidate)
    "Transformer for pretty bookmarks in `counsel-bookmark'.n"
    (let ((front (cdr (assoc 'front-context-string (cdr (assoc candidate bookmark-alist))))))
      (s-replace "\n" ""  (concat  "" front))))

  (add-to-list 'ivy-rich--display-transformers-list
               '(:columns
                 ((ivy-rich-candidate (:width 35 :face font-lock-builtin-face))
                  (ivy-rich-bookmark-context-string (:width 20 :face font-lock-string-face))
                  (ivy-rich-bookmark-filename (:face font-lock-doc-face)))))

  (add-to-list 'ivy-rich--display-transformers-list 'counsel-bookmark)

  (ivy-rich-mode -1)
  (ivy-rich-mode +1))


(use-package counsel
  :ensure t
  :after (ivy fzf)
  :defer 1
  :diminish counsel-mode
  :functions jens/counsel-read-file-name
  :commands (counsel-mode counsel--find-file-matcher)
  :bind
  (("C-S-s" . jens/ripgrep)
   ("C-x f" . counsel-recentf)
   ("C-x C-f" . counsel-find-file)
   ("C-x C-S-f" . counsel-fzf)
   ("C-x C-i" . counsel-imenu)
   ("M-æ" . counsel-mark-ring)
   ("M-x" . counsel-M-x)
   ("M-b" . counsel-bookmark)
   :map help-map
   ("l" . counsel-find-library))
  :config
  (setq
   counsel-grep-base-command
   "rg -i -M 250 --no-heading --line-number --color never '%s' %s")

  (defun jens/ripgrep ()
    "Interactively search the current directory. Jump to result using ivy."
    (interactive)
    (let ((counsel-ag-base-command counsel-rg-base-command)
          (initial-input (if (use-region-p)
                             (buffer-substring-no-properties
                              (region-beginning) (region-end)))))
      (deactivate-mark)
      (counsel-ag initial-input default-directory)))

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
  :straight (bookmark+ :type git :host github :repo "emacsmirror/bookmark-plus")
  :config
  (advice-add #'bookmark-jump :around
              (lambda (fn &rest args)
                "Push point to the marker-stack before jumping to bookmark."
                (let ((pm (point-marker)))
                  (apply fn args)
                  (xref-push-marker-stack pm)))))

(use-package projectile
  :ensure t
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
  :commands (enable-spellchecking
             flyspell-mode
             flyspell-prog-mode
             flyspell-buffer)
  :config
  (ispell-change-dictionary "english")

  (defun enable-spellchecking ()
    "Enable spellchecking in the current buffer."
    (interactive)
    (ispell-change-dictionary "english")
    (flyspell-prog-mode)
    (flyspell-buffer)))

(use-package flyspell-correct
  :ensure t
  :bind (("C-," . flyspell-correct-at-point))
  :commands flyspell-correct-at-point)

(use-package so-long
  :straight (so-long :type git :repo "https://git.savannah.gnu.org/git/so-long.git/")
  :demand t
  :commands so-long-enable
  :config
  (setq so-long-threshold 500)
  (so-long-enable))

(use-package treemacs
  :straight t)

(use-package posframe
  :straight t
  :config
  (setq posframe-mouse-banish nil))

(use-package eros
  :ensure t
  :bind (:map slime-mode-map
              ("C-x C-e" . jens/slime-eval-last-sexp))
  :config
  (eros-mode +1)

  (defun jens/slime-eval-last-sexp ()
    "Show the result of evaluating the last-sexp in an overlay."
    (interactive)
    (slime-eval-async `(swank:eval-and-grab-output ,(slime-last-expression))
                      (lambda (result)
                        (cl-destructuring-bind (output value) result
                          (let ((string (s-replace "\n" " " (concat output value))))
                            (message string)
                            (eros--eval-overlay string (point))))))
    (slime-sync)))

(use-package zenburn-theme
  :ensure t
  :demand t
  :config
  (load-theme 'zenburn t)
  :custom-face
  (mode-line ((t (:box nil))))
  (mode-line-inactive ((t (:box nil))))
  (hl-line ((t (:background "gray30"))))
  (highlight ((t (:background nil :foreground nil))))
  (popup-tip-face ((t (:background "#cbcbbb" :foreground "#2b2b2b")))))

;;;;;;;;;;;;;;;;;;;;;;
;; homemade things ;;
;;;;;;;;;;;;;;;;;;;;;;

(use-package blog
  :defer t
  :commands (blog-publish
             blog-find-posts-file)
  :load-path "~/vault/blog/src/")

(use-package views
  :straight (views :type git :repo "git@github.com:jensecj/views.el.git")
  :bind
  (("M-p p" . views-push)
   ("M-p k" . views-pop)
   ("M-v" . views-switch)))

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
  :defer t
  :commands (jens/etmux-jackin)
  :config
  (defun jens/etmux-jackin ()
    (interactive)
    (let* ((history-file (no-littering-expand-etc-file-name "etmux-history.el"))
           (etmux-command-history (jens/load-from-file history-file))
           (pane (etmux-pick-pane))
           (command (completing-read "command: " etmux-command-history)))

      (unless (member command etmux-command-history)
        (jens/save-to-file (cons command etmux-command-history) history-file))

      (dir-locals-set-class-variables
       'etmux
       `((nil . ((tmux-cmd . ,command)
                 (tmux-pane . ,pane)))))

      (dir-locals-set-directory-class
       (projectile-project-root) 'etmux)

      (defun jackin-etmux-do nil
        (interactive)
        (etmux-C-c tmux-pane)
        (etmux-run-command tmux-pane tmux-cmd))

      (global-set-key (kbd "C-x C-c") #'jackin-etmux-do)
      (hack-local-variables))))

(use-package highlight-bookmarks
  :demand t
  :commands highlight-bookmarks-in-this-buffer
  :config
  (add-hook 'find-file-hook #'highlight-bookmarks-in-this-buffer)
  (add-hook 'after-save-hook #'highlight-bookmarks-in-this-buffer)
  (advice-add #'bookmark-jump :after #'highlight-bookmarks-in-this-buffer)
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
  (("C-x t" . today-hydra/body)
   :map org-mode-map
   ("M-o" . today-set-hydra/body))
  :config
  (setq today-directory "~/vault/git/org/today/")

  (defhydra today-hydra (:foreign-keys run)
    "
^Capture^                                 ^Actions^                          ^Find^
^^^^^^^^----------------------------------------------------------------------------------------------
_r_: capture read task                    _a_: archive completed tasks      _t_: go to todays file
_R_: capture read task from clipboard     ^ ^                               _l_: list all date files
_w_: capture watch task                   ^ ^                               ^ ^
_W_: capture watch task from clipboard    ^ ^                               ^ ^
_c_: capture with prompt                  ^ ^                               ^ ^
"
    ("r" (lambda () (interactive) (today-capture-link-with-task 'read)))
    ("R" (lambda () (interactive) (today-capture-link-with-task-from-clipboard 'read)))
    ("w" (lambda () (interactive) (today-capture-link-with-task 'watch)))
    ("W" (lambda () (interactive) (today-capture-link-with-task-from-clipboard 'watch)))
    ("c" #'today-capture-prompt)

    ("a" #'today-archive-done-todos :exit t)

    ("t" #'today :exit t)
    ("T" #'today-visit-todays-file :exit t)
    ("l" #'today-list :exit t)

    ("f" #'jens/org-today-refile/body :exit t)

    ("q" nil "quit")))

(use-package doc-at-point
  :straight (doc-at-point :repo "git@github.com:jensecj/doc-at-point.el.git")
  :defer t
  :bind
  (("C-+" . doc-at-point)
   :map company-active-map
   ("C-+" . doc-at-point-company-menu-selection-quickhelp))
  :commands (doc-at-point
             doc-at-point-company-menu-selection-quickhelp
             doc-at-point-setup-defaults)
  :config
  (setq doc-at-point--posframe-font "Source Code Pro Semibold")
  (doc-at-point-setup-defaults)
  :custom-face
  (internal-border ((t (:background "#777777")))))

(use-package replace-at-point
  :straight (replace-at-point :repo "git@github.com:jensecj/replace-at-point.el.git")
  :bind ("C-M-SPC" . replace-at-point)
  :config
  (replace-at-point-setup-defaults))

(use-package lowkey-mode-line
  :straight
  (lowkey-mode-line
   :repo "git@github.com:jensecj/lowkey-mode-line.el.git")
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
  "Call ORIG-FUN until the cursor moves.  Try popping up to 10
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
(bind-key* "C-x a" 'mark-whole-buffer)

;; Quit emacs, mnemonic is C-x REALLY QUIT
(bind-key* "C-x r q" 'save-buffers-kill-terminal)
;; Kill emacs, mnemonic is C-x REALLY KILL
(bind-key* "C-x r k" 'save-buffers-kill-emacs)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; keybindings for defuns ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Better C-a
(bind-key* "C-a" 'jens/smart-beginning-of-line)

(bind-key* "M-<prior>" #'jens/sexp-up)
(bind-key* "M-<next>" #'jens/sexp-down)

;; Join lines (pull the below line up to this one, or the above one down)
(bind-key* "M-j" 'jens/join-region-or-line)
(bind-key* "M-J" 'jens/join-line-down)

;; Comment/uncomment block
(bind-key* "C-c c" 'jens/comment-uncomment-region-or-line)

;; Fix spaces / tabs
(bind-key "C-c n" 'jens/cleanup-buffer)

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

(bind-key* "C-M-k" #'jens/copy-symbol-at-point)

(bind-key* "M-r" #'jens/goto-repo)

(global-set-key (kbd "<f12>") #'jens/inspect-variable-at-point)

;; Completion that uses many different methods to find options.
(global-set-key (kbd "C-.") 'hippie-expand)

(msg-success (format "Emacs initialized in %s, with %s garbage collections." (emacs-init-time) gcs-done))

(defun jens/show-initial-important-messages ()
  "Show all lines in *Messages* matching a regex for important messages."
  (let* ((regex
          (rx (or
               ;; show info about loaded files with auto-save data
               "recover-this-file"
               ;; show warning messages that occured during init
               (group bol "!")
               ;; lines containing the word `warning'
               (group bol (0+ any) "warning" (0+ any) eol))))
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

(provide 'init)
;;; init.el ends here
