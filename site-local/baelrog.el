;; -*- mode: Emacs-Lisp -*-
;; local.el
;; Time-stamp: <2023-02-23 16:00:25 shantanu>

(setq user-mail-address "shantanu@helpshift.com")

(setq w/features
      (append w/features '(fira-code
                           ligatures
                           org-mode
                           rust
                           clojure)))

(add-to-list 'exec-path "/opt/homebrew/bin")

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc emacs-lisp)
;; End:
