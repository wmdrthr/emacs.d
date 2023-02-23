;; -*- mode: Emacs-Lisp -*-
;; .emacs
;; Time-stamp: <2023-02-23 15:59:44 shantanu>
;;    ___ _ __ ___   __ _  ___ ___
;;   / _ \ '_ ` _ \ / _` |/ __/ __|
;;  |  __/ | | | | | (_| | (__\__ \
;;   \___|_| |_| |_|\__,_|\___|___/
;;

(set-language-environment "UTF-8")
(setenv "LANG" "en_US.UTF-8")

(require 'cl-lib)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Add timestamps to 'message' output.
(defadvice message (before when-was-that activate)
  (when (> (length (ad-get-arg 0)) 0)
    (ad-set-arg 0 (concat (format-time-string "[%a %T.%3N] ")
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

(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path (expand-file-name "~/bin"))
(add-to-list 'exec-path (expand-file-name "~/.local/bin"))
(setq default-directory "~/")

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

(defvar w/features '()
  "List of features/packages to be enabled/installed/configured.")

(defmacro w/featurep (feature &rest body)
  `(when (member ,feature w/features)
     ,@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Personalization

(setq user-full-name "Shantanu Joshi")

(let ((local-init-file (expand-file-name "local.el"
                                         (concat user-emacs-directory "site-local/"))))
  (when (file-exists-p local-init-file)
    (load local-init-file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Useful packages

;; Set the file where emacs will write its own customizations
(setq custom-file (concat user-emacs-directory "custom.el"))
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
(load custom-file)

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
  (ido-enable-flex-matching t)
  (ido-everywhere t))

;; Use ido with imenu
(use-package idomenu
  :load-path "site-local/"
  :bind ("C-." . idomenu))

;; rainbow delimiters
(use-package rainbow-mode
  :ensure t
  :diminish rainbow-mode
  :hook css-mode)

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
  :hook (after-save . backup-each-save)
  :config ;; these are not custom variables
  (setq backup-each-save-mirror-location "~/.emacs.d/backups")
  (setq backup-each-save-time-format "%Y_%m_%d_%H_%M")
  (setq backup-each-save-filter-function 'backup-each-save-filter))

;; Recent Files
(use-package recentf
  :custom
  (recentf-save-file "~/.emacs.d/.recentf")
  (recentf-max-menu-items 15)
  (recentf-max-saved-items 50)
  (recentf-auto-cleanup 300) ;; every 5 minutes
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

;; Searching - use both ripgrep and the silver searcher
(use-package ag :ensure t)
(use-package rg :ensure t)

;; Smart scan
(use-package smartscan
  :ensure t
  :config
  (global-smartscan-mode t)
  (unbind-key "M-n" smartscan-map)
  (unbind-key "M-p" smartscan-map)
  :bind (("s-n" . smartscan-symbol-go-forward)
         ("s-p" . smartscan-symbol-go-backward)))

;; Persistent scratch buffer
(use-package persistent-scratch
  :ensure t
  :init (persistent-scratch-setup-default)
  :config
  (defun persistent-scratch-buffer-p ()
    (string-match "^*scratch" (buffer-name)))
  (setq persistent-scratch-scratch-buffer-p-function 'persistent-scratch-buffer-p))

;; Git integration
(use-package magit
  :ensure t
  :defer t
  :diminish auto-revert-mode
  :config (use-package magit-blame)
  :custom (git-commit-summary-max-length 72)
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch)
         ("C-x s-g" . magit-blame-popup)))


;; Enhancements to dired
(use-package all-the-icons
  :ensure t)

(use-package all-the-icons-dired
  :ensure t
  :defer t
  :diminish all-the-icons-dired-mode
  :config
  :hook (dired-mode . (lambda ()
                        (interactive)
                        (unless (file-remote-p default-directory)
                          (all-the-icons-dired-mode)))))

;; Projectile
(use-package projectile
  :ensure t
  :config (projectile-mode +1)
  :diminish projectile-mode
  :bind (("C-s-p" . 'projectile-command-map)))

(use-package multiple-cursors
  :ensure t
  :bind
   (("C-c m t" . mc/mark-all-like-this)
    ("C-c m m" . mc/mark-all-like-this-dwim)
    ("C-c m l" . mc/edit-lines)
    ("C-c m e" . mc/edit-ends-of-lines)
    ("C-c m a" . mc/edit-beginnings-of-lines)
    ("C-c m n" . mc/mark-next-like-this)
    ("C-c m p" . mc/mark-previous-like-this)
    ("C-c m s" . mc/mark-sgml-tag-pair)
    ("C-c m d" . mc/mark-all-like-this-in-defun)))

;; Completion at point
(use-package corfu
  :ensure t
  :custom
  (corfu-auto t)
  (corfu-auto-prefix 3)
  (corfu-auto-delay 0)
  (corfu-echo-documentation 0)
  (corfu-quit-no-match 'separator)
  (corfu-preview-current nil)
  :bind (:map corfu-map
         ("<escape>" . corfu-quit)
         ("C-h" . corfu-show-documentation))
  :init (global-corfu-mode))

;; Vertical ompletion in minibuffer
(use-package vertico
  :ensure t
  :bind (:map vertico-map
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous)
              ("M-j" . vertico-next)
              ("M-k" . vertico-previous)
              ("C-f" . vertico-exit)
              :map minibuffer-local-map
              ("M-h" . backward-kill-word))
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode)
  ;; Grow and shrink the Vertico minibuffer
  (setq vertico-resize t)
  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  (setq vertico-cycle t)
  :config
  (vertico-mouse-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org Mode customization
(w/featurep
 'org-mode

 (require 'org)

 (global-set-key (kbd "C-c a") 'org-agenda)
 (setq org-agenda-files (list
                         (expand-file-name "~/src/helpshift/data/helpshift.org")))

 ;; Set priority range from A to C with default A
 (setq org-highest-priority ?A)
 (setq org-lowest-priority ?C)
 (setq org-default-priority ?A)

 (setq org-log-done t)

 ;;open agenda in current window
 (setq org-agenda-window-setup (quote current-window))

 ;;capture todo items using C-c c t
 (define-key global-map (kbd "C-c c") 'org-capture)
 (setq org-capture-templates
       '(("t" "todo" entry (file+headline (expand-file-name "~/src/helpshift/data/helpshift.org")
                                          "Tasks")
          "* TODO [#A] %?")))
 (setq org-todo-keywords '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))

 ;; Disable ligatures in org mode - fonts with ligatures crash emacs
 ;; when an org file is opened
 (add-hook 'org-mode-hook 'disable-auto-composition)

 (use-package org-bullets
   :ensure t
   :custom (org-bullets-bullet-list '("◉" "◆" "✜" "▶"))
   :hook (org-mode . org-bullets-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Appearance

;; Disable toolbar, menu-bar and scroll bar
(mapc (lambda (mode)
        (when (fboundp mode)
          (funcall mode -1)))
      '(menu-bar-mode tool-bar-mode scroll-bar-mode))

;; Theme - the Doom One theme is the default, but ensure some other
;; themes are available

(use-package nord-theme
  :ensure t)

(use-package dracula-theme
  :ensure t)

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-one t)
  (doom-themes-org-config)
  (custom-theme-set-faces
   'doom-one
   '(outline-1 ((t (:height 1.1 :foreground "#51afef" :weight semi-bold))))))


;; Display time in the toolbar
(display-time-mode 1)

;; Fonts

;; default font size - overridden in local.el
(defvar font-size 140)

(use-package unicode-fonts
  :ensure t
  :config (unicode-fonts-setup))

;; Default font
(w/featurep
 'fira-code

 (set-face-attribute 'default nil :font "Fira Code Retina" :height font-size)
 (set-frame-font "Fira Code Retina" nil t))

(w/featurep
 'iosevka

 (set-face-attribute 'default nil :font "Iosevka" :height font-size)
 (set-frame-font "Iosevka" nil t))

(w/featurep
 'ligatures

 (if (eq system-type 'darwin)
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
       (add-hook 'c-mode-common-hook 'disable-auto-composition))

   (progn
     (use-package ligature
       :load-path "site-local/"
       :config
       (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                            ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                            "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                            "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                            "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                            "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                            "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                            "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                            ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                            "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                            "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                            "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                            "\\\\" "://"))
       :hook (prog-mode . ligature-mode)))))


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

(use-package flycheck
  :ensure t
  :hook (prog-mode . flycheck-mode))

(defun w/lsp-deferred-if-supported ()
  "Run lsp-deferred only if it's a supported mode"
  (unless (derived-mode-p 'emacs-lisp-mode 'clojure-mode 'sql-mode)
    (lsp-deferred)))

(use-package lsp-mode
  :ensure t
  :config
  (add-to-list 'lsp-file-watch-ignored "[/\\\\]\\.venv\\'")
  (add-to-list 'lsp-file-watch-ignored "[/\\\\]\\venv\\'")
  (add-to-list 'lsp-file-watch-ignored "[/\\\\]\\.git\\'")
  (setq lsp-completion-show-kind nil)
  (setq lsp-enable-snippet nil)
  :custom
  (lsp-completion-provider :none)
  :init
  (setq lsp-keymap-prefix "C-c l")
  (defun w/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(flex)))
  :hook
  (prog-mode . w/lsp-deferred-if-supported)
  (lsp-completion-mode . w/lsp-mode-setup-completion)
  :commands lsp)

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-doc-header t)
  (setq lsp-ui-doc-include-signature t)
  (setq lsp-ui-doc-border (face-foreground 'default))
  (setq lsp-ui-sideline-show-code-actions t)
  (setq lsp-ui-sideline-delay 0.05))


(w/featurep
 'python

 (use-package python
   :ensure nil
   :custom
   (python-indent-offset 4))

 (use-package pyvenv
   :ensure t)

 (use-package lsp-pyright
   :ensure t
   :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)))))


(w/featurep
 'clojure

 (use-package flycheck-clj-kondo
   :ensure t
   :custom
   (flycheck-highlighting-mode 'sexps))

 (use-package clojure-mode
   :ensure t
   :config (require 'flycheck-clj-kondo))

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
               ("C-C M-n" . cider-repl-set-ns))))


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
              ("RET" . nil)
              ("C-j" . paredit-newline)
              ("M-)" . paredit-forward-slurp-sexp)
              ("M-(" . paredit-wrap-round)
              ("M-[" . paredit-wrap-square)
              ("M-{" . paredit-wrap-curly)))


(w/featurep
 'common-lisp

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
        (load (expand-file-name "~/.quicklisp/clhs-use-local.el") t)))))


(w/featurep
 'haskell

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


(use-package yaml-mode
  :ensure t
  :defer t
  :bind (:map yaml-mode-map ("C-m" . newline-and-indent)))


(w/featurep
 'rust

 (when (file-directory-p (expand-file-name "~/.cargo"))

   (use-package rust-mode
     :ensure t
     :after (lsp-mode lsp-ui)
     :config (add-to-list 'exec-path (expand-file-name "~/.cargo/bin"))
     :custom
     (lsp-rust-analyzer-server-display-inlay-hints t)
     (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
     (lsp-rust-analyzer-display-chaining-hints t)
     (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
     (lsp-rust-analyzer-display-closure-return-type-hints t)
     (lsp-rust-analyzer-display-parameter-hints nil)
     (lsp-rust-analyzer-display-reborrow-hints nil))

   (use-package cargo
     :after rust-mode
     :ensure t
     :hook (rust-mode . cargo-minor-mode))))


(w/featurep
 'csv

 (use-package csv-mode
   :ensure t
   :hook (csv-mode . disable-auto-composition)))


(use-package markdown-mode
  :ensure t
  :defer t)

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
              standard-indent 4
              show-trailing-whitespace t
              visible-bell nil
              ring-bell-function 'ignore
              redisplay-dont-pause t ;; don't pause display on input
              ;; disable bidirectional text support for slight performance bonus
              bidi-display-reordering nil)

;; Don't ask for confirmation when opening symlinked file
(setq vc-follow-symlinks t)

;; Do not ask for confirmation
(setq confirm-nonexistent-file-or-buffer nil)

;; Show trailing whitespace
(setq-default show-trailing-whitespace t)

;; Do not show annoying menu-bar tips
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

;; Show the current match number and the total number of matches in
;; the search prompt.
(setq isearch-lazy-count 1)

;; Personal dictionary file for ispell
(setq ispell-personal-dictionary (concat user-emacs-directory "ispell_personal"))

;; Firefox as default browser, Chrome for work related links
(when (eq system-type 'darwin)
  (setq browse-url-chrome-program "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"
        browse-url-firefox-program "/Applications/Firefox.app/Contents/MacOS/firefox"))
(setq browse-url-handlers
      '(("https://helpshift.atlassian.net/\.*" . browse-url-chrome)
        ("https://gerrit.helpshift.com/\.*" . browse-url-chrome)
        ("." . browse-url-firefox)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keybindings and useful functions

(bind-key "M-o"   'other-window)
(bind-key "M-1"   'delete-other-windows)
(bind-key "M-0"   'delete-window)
(bind-key "M-]"   'ns-next-frame)
(bind-key "M-'"   'pop-to-mark-command)

;; Indent/De-indent selection by one tab length
(global-set-key (kbd "s->") 'indent-rigidly-right-to-tab-stop)
(global-set-key (kbd "s-<") 'indent-rigidly-left-to-tab-stop)

;; Browse url at point when clicking and pressing super
(global-set-key (kbd "<s-mouse-1>")
                (lambda (event)
                  (interactive (list last-command-event))
                  (posn-set-point (event-end event))
                  (browse-url (thing-at-point 'url t))))

(global-set-key [(super o)] 'other-frame)

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
           (set-window-start w2 s1)))))
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
 (lambda (arg)
   (interactive "P")
   (if arg
       (load-file user-init-file)
       (find-file user-init-file))))

(w/featurep
 'clojure

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
     (let* ((project-dir (projectile-project-root))
            (port (or iport
                      (read-nrepl-port project-dir))))
       (if (and project-dir port)
           (cider-connect (list :host "localhost" :port port :project-dir project-dir))
         (let* ((project-dir (read-directory-name "Project: " "~/src/helpshift/moby" nil nil nil))
                (port (or iport
                          (read-nrepl-port project-dir)
                          (read-number "Port: "))))
           (cider-connect (list :host "localhost" :port port :project-dir project-dir)))))))

 (bind-key "C-9" 'cider-connect-helper))

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

(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there.

Source: http://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/"
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)
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
           (cl-count t (buffer-list)
                  :test-not
                  (lambda (ignore buf)
                    (null (cdr (assoc 'buffer-file-truename
                                      (buffer-local-variables buf))))))))
(bind-key "C-x u" 'uptime)

(defun new-scratch-buffer ()
  "generate and switch to a new temporary buffer"
  (interactive)
  (let* ((buffer-name (read-string "Buffer mode: "))
         (major-mode-name (intern (concat buffer-name "-mode"))))
    (switch-to-buffer (generate-new-buffer (format "*scratch:%s*" buffer-name)))
    (when (fboundp major-mode-name)
      (funcall major-mode-name))))
(bind-key "C-c n" 'new-scratch-buffer)


(when (eq 27 emacs-major-version)
  "Fix for bug in latest emacs version"
  (defun load-history-filename-element (file-regexp)
    "Get the first elt of `load-history' whose car matches FILE-REGEXP.
        Return nil if there isn't one."
    (let* ((loads load-history)
           (load-elt (and loads (car loads))))
      (save-match-data
        (while (and loads
                    (or (null (car load-elt))
                        (not (and (stringp (car load-elt))
                                  (string-match file-regexp (car load-elt))))))
          (setq loads (cdr loads)
                load-elt (and loads (car loads)))))
      load-elt)))

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

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc emacs-lisp)
;; End:
