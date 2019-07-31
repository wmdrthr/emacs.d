;; -*- mode: Emacs-Lisp -*-
;; .emacs
;; Time-stamp: <2019-08-15 15:13:20 weemadarthur>

;;    ___ _ __ ___   __ _  ___ ___
;;   / _ \ '_ ` _ \ / _` |/ __/ __|
;;  |  __/ | | | | | (_| | (__\__ \
;;   \___|_| |_| |_|\__,_|\___|___/
;;

(set-language-environment "UTF-8")
(setenv "LANG" "en_US.UTF-8")

(require 'cl)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Add timestamps to 'message' output.
(defadvice message (before when-was-that activate)
  (when (> (length (ad-get-arg 0)) 0)
    (ad-set-arg 0 (concat (format-time-string "[%a %T] ")
                          (ad-get-arg 0)))))

(setq emacs-load-start-time (current-time))
(message "» Loading .emacs file")

;; Avoid garbage collection during emacs startup
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'after-init-hook (lambda ()
                             ;; Make it 20 MB after initialization
                             (setq gc-cons-threshold (* 20 1024 1024))))

;; trim whitespace from end of string
(defun chomp (str)
  "trim trailing whitespace (tabs and spaces) from str"
  (if (string-match "[ \t\n]*$" str)
      (replace-match "" nil nil str)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Personalization

(setq hostname (chomp (shell-command-to-string "hostname -s")))

(setq default-directory "~/")

(setq user-full-name "Shantanu Joshi"
      user-mail-address (if (equal hostname "baelrog")
                            "shantanu@helpshift.com"
                          "weemadarthur@yggdrasil.in"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages
(require 'package)

(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
;(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; bootstrap use-package
(package-initialize)
(when (not (package-installed-p 'use-package))
  (progn (package-refresh-contents)
         (package-install 'use-package)))

(add-to-list 'exec-path "/usr/local/bin")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Useful packages

;; Set the file where emacs will write its own customizations
(setq custom-file (concat user-emacs-directory "custom.el"))
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
(load custom-file)

;; Some modes have trouble with ligatures; for those buffers, we
;; need to disable auto composition.
(defun disable-auto-composition ()
  (auto-composition-mode 0))

;; hide minor modes
(use-package diminish
  :ensure t)

;; kill-ring-search
(use-package kill-ring-search
  :ensure t
  :bind ("M-C-y" . kill-ring-search))

;; Enable ido
(use-package ido
  :config
  (ido-mode t)
  :custom
  (ido-case-fold t)
  (ido-use-faces t)
  (ido-enable-flex-mathing t)
  (ido-everywhere t))

;; rainbow delimiters
(use-package rainbow-delimiters
  :ensure t
  :hook ((lisp-mode . rainbow-delimiters-mode)
         (prog-mode . rainbow-delimiters-mode)))

;; unique buffer names
(use-package uniquify
  :custom
  (uniquify-buffer-name-style 'reverse)
  (uniquify-separator "/")
  (uniquify-after-kill-buffer-p t)
  (uniquify-ignore-buffers-re "^\\*"))

;; expands selection to enclosing block
(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

;; translate ANSI escape sequence
(use-package ansi-color
  :config (ansi-color-for-comint-mode-on))

;; eshell
(use-package eshell
  :defer t
  :bind ("C-M-e" . eshell)
  :hook (eshell-mode . disable-auto-composition)
  :custom
  (eshell-history-size 5000)
  (eshell-save-history-on-exit t))

;; File Backups
(defun backup-each-save-filter (filename)
  (let ((ignored-filenames '("^/tmp" "\\.recentf$"))
        (matched-ignored-filename nil))
    (mapc
     (lambda (x)
       (when (string-match x filename)
         (setq matched-ignored-filename t)))
     ignored-filenames)
    (not matched-ignored-filename)))

(use-package backup-each-save
  :load-path "site-local/"
  :init (setq make-backup-files nil)    ; disable standard backups
  :hook after-save
  :config ;; these are not custom variables
  (setq backup-each-save-mirror-location "~/.emacs.d/backups")
  (setq backup-each-save-time-format "%Y_%m_%d_%H_%M")
  (setq backup-each-save-filter-function 'backup-each-save-filter))

;; Recent Files
(use-package recentf
  :custom
  (recentf-save-file "~/.emacs.d/.recentf")
  (recentf-max-menu-items 0)
  (recentf-max-saved-items 50)
  (recentf-auto-cleanup 300)
  (recentf-filename-handlers '(abbreviate-file-name))
  :config (recentf-mode 1)
  :bind ("C-c r" . recentf-open-files))

(use-package avy
  :ensure t
  :custom
  (avy-all-windows nil)
  (avy-background t)
  :bind (("C-;" . avy-goto-char-timer)
         ("M-g M-g" . avy-goto-line)))

;; The Silver Searcher
(use-package ag
  :ensure t
  :custom (ag-executable "/usr/local/bin/ag"))

;; Dumb Jump
(use-package dumb-jump
  :ensure t
  :config
  (dumb-jump-mode)
  :init (setq dumb-jump-prefer-searcher 'rg)
  (unbind-key "C-M-q" dumb-jump-mode-map)
  :bind (("C-M-g" . dumb-jump-go)
         ("C-s-q" . dumb-jump-quick-look)))

;; Persistent scratch buffer
(use-package persistent-scratch
  :ensure t
  :config (persistent-scratch-setup-default))

;; Git integration
(use-package magit
  :ensure t
  :defer t
  :diminish auto-revert-mode
  :config (use-package magit-blame)
  :custom (git-commit-summary-max-length 72)
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch-popup)
         ("C-x s-g" . magit-blame-popup)))

;; Projectile
(use-package projectile
  :ensure t
  :config (projectile-mode +1)
  :diminish projectile-mode
  :bind (("s-p" . 'projectile-command-map)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org Mode customization
(when (equal hostname "baelrog")
  (global-set-key (kbd "C-c a") 'org-agenda)

  (setq org-agenda-files '("/Users/sjoshi/src/helpshift/data/helpshift.org"))

  ;;set priority range from A to C with default A
  (setq org-highest-priority ?A)
  (setq org-lowest-priority ?C)
  (setq org-default-priority ?A)

  (setq org-log-done t)

  ;;open agenda in current window
  (setq org-agenda-window-setup (quote current-window))

  ;;capture todo items using C-c c t
  (define-key global-map (kbd "C-c c") 'org-capture)
  (setq org-capture-templates
        '(("t" "todo" entry (file+headline "/Users/sjoshi/src/helpshift/data/helpshift.org" "Tasks")
           "* TODO [#A] %?")))

  (add-hook 'org-mode-hook 'disable-auto-composition)

  (use-package org-bullets
    :ensure t
    :hook (org-mode . org-bullets-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Appearance

;; Disable toolbar, menu-bar and scroll bar
(mapc (lambda (mode)
        (when (fboundp mode)
          (funcall mode -1)))
      '(menu-bar-mode tool-bar-mode scroll-bar-mode))

;; Theme
(use-package doom-themes :ensure t)
(use-package dracula-theme :ensure t)

(load-theme 'doom-one t)
(defvar current-theme :doom)

(defun switch-theme ()
  (interactive)
  (if (eq :doom current-theme)
      (progn (load-theme 'dracula t)
             (setq current-theme :dracula))
    (progn (load-theme 'doom-one t)
           (setq current-theme :doom))))

(global-set-key '[(f5)] 'switch-theme)


;; Fonts

(use-package unicode-fonts
  :ensure t
  :config (unicode-fonts-setup))

;; Default font
(if (equal hostname "baelrog")
    (progn
      (set-face-attribute 'default nil :font "Fira Code Retina" :height 140)
      (let ((alist '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
                     (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
                     (36 . ".\\(?:>\\)")
                     (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
                     (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
                     (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
                     (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
                     (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
                     (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")
                     (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
                     (48 . ".\\(?:x[a-zA-Z]\\)")
                     (58 . ".\\(?:::\\|[:=]\\)")
                     (59 . ".\\(?:;;\\|;\\)")
                     (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
                     (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
                     (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
                     (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
                     (91 . ".\\(?:]\\)")
                     (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
                     (94 . ".\\(?:=\\)")
                     (119 . ".\\(?:ww\\)")
                     (123 . ".\\(?:-\\)")
                     (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
                     (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)"))))
        (dolist (char-regexp alist)
          (set-char-table-range composition-function-table (car char-regexp)
                                `([,(cdr char-regexp) 0 font-shape-gstring])))
        (set-frame-font "Fira Code Retina" nil t)))
  (set-face-attribute 'default nil :font "Iosevka" :height 140))


;; Smart modeline
(use-package smart-mode-line
  :ensure t
  :config (sml/setup))

;; Native Line Numbers
(global-display-line-numbers-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OSX specific customizations

(when (eq system-type 'darwin)

  (setq ns-use-srgb-colorspace t)

  ;; Use the Command key as Meta
  (setq mac-option-key-is-meta nil)
  (setq mac-command-key-is-meta t)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super)

  ;; Menu bar is not annoying in OSX
  (menu-bar-mode 1)

  ;; Make the browser the OS X default
  (setq browse-url-browser-function 'browse-url-default-macosx-browser)

  ;; In dired, move deletions to trash
  (setq delete-by-moving-to-trash t)

  ;; Make cut and paste work with the OS X clipboard
  (defun paste-from-osx-clipboard ()
    (shell-command-to-string "pbpaste"))

  (defun copy-to-osx-clipboard (text &optional push)
    (let ((process-connection-type nil))
      (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
        (process-send-string proc text)
        (process-send-eof proc))))

  (setq interprogram-cut-function 'copy-to-osx-clipboard)
  (setq interprogram-paste-function 'paste-from-osx-clipboard))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Programming Modes and Related Packages

;; Company mode - autocompletion
(use-package company
  :defer t
  :ensure t
  :diminish company-mode
  :config (global-company-mode))

;; Python mode
(use-package anaconda-mode
  :ensure t
  :diminish anaconda-mode
  :hook python-mode
  :custom (python-indent-offset 4))

;; completion for Python mode using anaconda
(use-package company-anaconda
  :ensure t
  :after (company anaconda-mode)
  :config (add-hook 'python-mode-hook
                    (lambda () (add-to-list 'company-backends 'company-anaconda))))

;; Clojure
(use-package clojure-mode
  :ensure t)

;; Clojure CIDER mode
(use-package cider
  :ensure t
  :after clojure-mode
  :custom
  (nrepl-hide-special-buffers t)
  (cider-prefer-local-resources t)
  (cider-stacktrace-default-filters '(tooling dup))
  (cider-show-error-buffer 'only-in-repl)
  (cider-stacktrace-fill-column 80)
  (cider-repl-display-in-current-window t)
  (cider-repl-display-help-banner nil)
  (cider-repl-use-pretty-printing t)
  (cider-repl-history-file (concat user-emacs-directory "cider-history.dat"))
  :bind (:map cider-repl-mode-map
              ("C-c C-l" . cider-repl-clear-buffer)
              :map cider-mode-map
              ("C-C M-n" . cider-repl-set-ns)))

;; paredit
(use-package paredit
  :ensure t
  :diminish paredit-mode
  :hook ((clojure-mode . enable-paredit-mode)
         (cider-repl-mode . enable-paredit-mode)
         (lisp-mode . enable-paredit-mode)
         (emacs-lisp-mode . enable-paredit-mode)
         (lisp-interaction-mode . enable-paredit-mode)
         (ielm-mode . enable-paredit-mode)
         (json-mode . enable-paredit-mode))
  :bind (:map paredit-mode-map
              ("M-)" . paredit-forward-slurp-sexp)
              ("M-(" . paredit-wrap-round)
              ("M-[" . paredit-wrap-square)
              ("M-{" . paredit-wrap-curly)))

;; Common Lisp
(when (file-exists-p (expand-file-name "~/.quicklisp/slime-helper.el"))
  (load (expand-file-name "~/.quicklisp/slime-helper.el"))
  (setq inferior-lisp-program "/usr/local/bin/sbcl --no-linedit")
  (eval-after-load "slime"
    '(progn
       (slime-setup '(slime-fancy
                      slime-fancy-inspector
                      slime-asdf
                      slime-indentation
                      slime-fontifying-fu))
       (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol
             slime-fuzzy-completion-in-place nil
             slime-complete-symbol*-fancy t
             slime-net-coding-system 'utf-8-unix)
       (slime-autodoc-mode)
       (add-hook 'slime-mode-hook
                 (lambda ()
                   (set-variable lisp-indent-function 'common-lisp-indent-function)))
       ;; Point slime to local copy of HyperSpec
       (load (expand-file-name "~/.quicklisp/clhs-use-local.el") t))))


;; Haskell mode
(when (file-exists-p (expand-file-name "~/.ghc"))

  (use-package haskell-mode
    :ensure t
    :bind (:map haskell-mode-map
                ("C-c C-l" . haskell-process-load-or-reload-promp)
                ("C-c C-z" . haskell-interactive-switch))
    :config
    (require 'haskell-interactive-mode)
    (require 'haskell-process)
    :init ;; these are not custom variables
    (setq haskell-process-suggest-remove-import-lines t
          haskell-process-auto-import-loaded-modules t
          haskell-process-log t
          haskell-indentation-electric-flag t
          haskell-process-args-stack-ghci '("--ghci-options=-ferror-spans -fshow-loaded-modules")))

  (use-package company-ghc
    :ensure t
    :custom (company-ghc-show-info t)
    :config (add-to-list 'company-backends 'company-ghc))

  (use-package intero
    :ensure t
    :hook (haskell-mode . intero-mode)))


;; YAML mode
(use-package yaml-mode
  :ensure t
  :defer t
  :bind (:map yaml-mode-map ("C-m" . newline-and-indent)))

(add-hook 'c-mode-common-hook 'disable-auto-composition)

;; Rust Mode
(use-package rust-mode
  :ensure t
  :config (add-to-list 'exec-path (expand-file-name "~/.cargo/bin")))

(use-package cargo
  :after rust-mode
  :ensure t
  :hook (rust-mode . cargo-minor-mode))

(defun racer-setup ()
  (let ((rust-root (chomp (with-temp-buffer
                            (call-process "rustc" nil t nil "--print" "sysroot")
                            (buffer-string)))))
    (setq racer-rust-src-path (concat rust-root "/lib/rustlib/src/rust/src"))))

(use-package racer
  :after rust-mode
  :ensure t
  :bind (:map rust-mode-map ("TAB" . company-indent-or-complete-common))
  :hook ((rust-mode . racer-mode)
         (racer-mode . eldoc-mode))
  :config (racer-setup))

;; Golang Mode
(use-package go-mode
  :ensure t)

;; CSV mode
(use-package csv-mode
  :ensure t
  :hook (csv-mode . disable-auto-composition))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customization

;; Prefer UTF-8
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(setq-default indent-tabs-mode nil ;; Indent with spaces instead of tabs
              c-basic-offset 4
              tab-width 4
              show-trailing-whitespace t
              visible-bell nil
              ring-bell-function 'ignore
              redisplay-dont-pause t ;; don't pause display on input
              ;; disable bidirectional tetx support for slight performance bonus
              bidi-display-reordering nil)

;; Do not ask for confirmation
(setq confirm-nonexistent-file-or-buffer nil)

;; Show trailing whitespace
(setq-default show-trailing-whitespace t)
;; Do not show annying menu-bar tips
(setq suggest-key-bindings nil)

;; Make default mode Emacs lisp mode
(setq default-major-mode 'emacs-lisp-mode)

;; Show column number in mode line
(column-number-mode 1)

;; automatically update timestamps when saving files
(add-hook 'before-save-hook 'time-stamp)

;; start the server
(if (eq system-type 'gnu/linux)
    (setq server-socket-dir (format "/run/user/%d" (user-uid)))
  (setq server-socket-dir user-emacs-directory))
(server-start)

;; Highlight matching parens
(show-paren-mode 1)
(set-face-attribute 'show-paren-match nil :weight 'extra-bold)

;; Personal dictionary file for ispell
(setq ispell-personal-dictionary (concat user-emacs-directory "ispell_personal"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keybindings and useful functions

(bind-key "M-o"   'other-window)
(bind-key "M-1"   'delete-other-windows)
(bind-key "M-0"   'delete-window)
(bind-key "M-]"   'ns-next-frame)
(bind-key "M-'"   'pop-to-mark-command)

(defalias 'yes-or-no-p 'y-or-n-p)

(defun swap-windows ()
  "If you have 2 windows, it swaps them."
  (interactive)
  (cond ((/= (count-windows) 2)
         (message "You need exactly 2 windows to do this."))
        (t
         (let* ((w1 (first (window-list)))
                (w2 (second (window-list)))
                (b1 (window-buffer w1))
                (b2 (window-buffer w2))
                (s1 (window-start w1))
                (s2 (window-start w2)))
           (set-window-buffer w1 b2)
           (set-window-buffer w2 b1)
           (set-window-start w1 s2)
           (set-window-start w2 s1))))
  (other-window 1))
(bind-key "C-c s" 'swap-windows)

(defun split-window-vertically-and-switch (&optional size)
  "same as split-window-vertically, but switches to the new window"
  (interactive "P")
  (select-window (split-window-below size)))
(bind-key "M-2" 'split-window-vertically-and-switch)

(defun split-window-horizontally-and-switch (&optional size)
  "same as split-window-horizontally, but switches to the new window"
  (interactive "P")
  (select-window (split-window-right size)))
(bind-key "M-3" 'split-window-horizontally-and-switch)

(defun my-toggle-window-split ()
  "Toggle between vertical and horizontal split (only works with exactly 2 windows)"
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
    (message "Only works with exactly 2 windows.")))
(bind-key "C-c w" 'my-toggle-window-split)

(defun kill-whitespace ()
  "Kill all whitespace between the point and the next non-whitespace character"
  (interactive "*")
  (save-excursion
    (save-restriction
      (save-match-data
        (progn
          (re-search-backward "[^ \t\r\n]" nil t)
          (re-search-forward "[ t\r\n]+" nil t)
          (replace-match " " nil nil))))))
(bind-key "M-z" 'kill-whitespace)

(bind-key
 "C-x C-c"
 (lambda ()
   (interactive)
   (if (y-or-n-p "Quit Emacs? ")
       (save-buffers-kill-emacs))))

(bind-key
 "C-0"
 (lambda ()
   (interactive)
   (find-file user-init-file)))

(defun cider-connect-helper (iport)
  "Helper for connecting to an already running Clojure repl."
  (interactive "P")
  (cl-flet ((read-nrepl-port (project-dir)
                             (ignore-errors
                               (car (read-from-string
                                     (with-temp-buffer
                                       (insert-file-contents (concat (file-name-as-directory project-dir)
                                                                     ".nrepl-port"))
                                       (buffer-string)))))))
    (let* ((project-dir (read-directory-name "Project: " "~/src/helpshift/moby" nil nil nil))
           (port (or iport
                     (read-nrepl-port project-dir)
                     (read-number "Port: "))))
      (cider-connect (list :host "localhost" :port port :project-dir project-dir)))))

(bind-key "C-9" 'cider-connect-helper)

;; macro recording
(defun toggle-kbd-macro-recording-on ()
  "One-key keyboard macros: toggle recording on"
  (interactive)
  (define-key global-map (this-command-keys)
    'toggle-kbd-macro-recording-off)
  (start-kbd-macro nil))

(defun toggle-kbd-macro-recording-off ()
  "One-key keyboard macros: toggle recording off"
  (interactive)
  (define-key global-map (this-command-keys)
    'toggle-kbd-macro-recording-on)
  (end-kbd-macro))

(global-set-key '[(f3)] 'toggle-kbd-macro-recording-on)

;; use "C-x K" to mark buffers opened using emacsclient as done
(add-hook 'server-switch-hook
          (lambda ()
            (when (current-local-map)
              (use-local-map (copy-keymap (current-local-map))))
	        (when server-buffer-clients
		      (local-set-key (kbd "C-x k") 'server-edit))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; emacs uptime

;; original code by Thien-Thi Nguyen, modified to return either broken down list or formatted string
(defun uptime-details (&optional format-as-string)
  "return uptime either as a list of total uptime in seconds, days, hours, minutes and seconds,
or as a formatted string containing the non-zero components of above list eg. 2d 3h 45s"
  (let* ((cur (current-time))
         (hi-diff (- (car cur) (car emacs-load-start-time)))
         (tot-sec (+ (ash hi-diff 16) (- (cadr cur) (cadr emacs-load-start-time))))
         (days (/ tot-sec (* 60 60 24)))
         (hrs  (/ (- tot-sec (* days 60 60 24)) (* 60 60)))
         (mins (/ (- tot-sec (* days 60 60 24) (* hrs 60 60)) 60))
         (secs (/ (- tot-sec (* days 60 60 24) (* hrs 60 60) (* mins 60)) 1)))
    (if format-as-string
        (chomp (concat
                (if (> days 0) (format "%dd " days) nil)
                (if (> hrs 0) (format "%dh " hrs) nil)
                (if (> mins 0) (format "%dm " mins) nil)
                (if (> secs 0) (format "%ds" secs) nil)))
      (list tot-sec days hrs mins secs))))

(defun uptime ()
  "print uptime, as well as emacs version, number of buffers, and number of open files"
  (interactive)
  (message "Emacs %s: up %s, %d buffers, %d files"
           emacs-version (uptime-details t)
           (length (buffer-list))
           (count t (buffer-list)
                  :test-not
                  (lambda (ignore buf)
                    (null (cdr (assoc 'buffer-file-truename
                                      (buffer-local-variables buf))))))))
(bind-key "C-x u" 'uptime)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customize the scratch buffer, add a welcome message

(defvar welcome-messages
  `("Let the hacking commence!"
    "Hacks and glory await!"
    "Hack and be merry!"
    "Your hacking starts... NOW!"
    "May the source be with you!"
    "Take this REPL, brother, and may it serve you well."
    "Lemonodor-fame is but a hack away!"
    ,(format "%s, this could be the start of a beautiful program."
             (car (split-string user-full-name)))
    "Scientifically-proven optimal words of hackerish encouragement."))

;; Show a message in the echo area
(defun display-startup-echo-area-message ()
  (message
   (concat "» " (nth (random (length welcome-messages)) welcome-messages))))

;; Get a fortune cookie
(defun fortune-cookie (&optional print-message)
  (interactive "p")
  (let ((cookie (chomp (with-temp-buffer
                         (call-process "fortune" nil t nil
                                       (expand-file-name (concat user-emacs-directory
                                                                 "site-local/fortune-lambda")))
                         (buffer-string)))))
    (if print-message
        (message cookie)
      (format ";; %s\n\n"
              (replace-regexp-in-string
               "\n" "\n;; " ; comment each line
               (replace-regexp-in-string
                "\n$" ""
                cookie))))))

;; show an ascii art of emacs
(load (concat user-emacs-directory "site-local/ascii.el"))
(defun welcome-buffer ()
  (let ((welcome-buffer (generate-new-buffer "*startup*")))
    (set-buffer welcome-buffer)
    (insert
     ";; " (replace-regexp-in-string "\n" "" (emacs-version)) "
;;
"
     (replace-regexp-in-string "//" ";;" (ascii-art-title)) "
;;
;; "
     (format "Loaded in %.05f seconds."
             (time-to-seconds (time-since emacs-load-start-time))) "
;;
"
             (fortune-cookie) "

")
    (read-only-mode)
    welcome-buffer))

(setq initial-buffer-choice 'welcome-buffer)
(setq initial-scratch-message nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun donuts ()
  (interactive)
  (message "» Mmm, donuts."))

(message "» Emacs startup time: %0.5f seconds."
         (time-to-seconds (time-since emacs-load-start-time)))
