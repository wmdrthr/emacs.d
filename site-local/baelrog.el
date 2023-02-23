;; -*- mode: Emacs-Lisp -*-
;; local.el
;; Time-stamp: <2022-11-28 11:47:23 shantanu>

(setq user-mail-address "shantanu@helpshift.com")

(setq w/features
      (append w/features '(fira-code
                           ligatures
                           org-mode
                           rust
                           clojure)))

;(push 'fira-code-ligatures packages)


(add-to-list 'exec-path "/opt/homebrew/bin")
