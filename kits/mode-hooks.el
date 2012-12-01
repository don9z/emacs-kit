;; c/c++ mode
(add-hook 'c-mode-common-hook
          (lambda ()
            (local-set-key "\M-f" 'c-forward-into-nomenclature)
            (local-set-key "\M-b" 'c-backward-into-nomenclature)
            ;; set C code mode to Kernighan and Ritchie mode
            (c-set-style "K&R")
            ;; set tab width and c offset
            (setq c-tab-width 4)
            (setq tab-width c-tab-width)
            (setq c-basic-offset c-tab-width)
            (setq standard-indent c-tab-width)
            ;; make style variables global
            (setq c-style-variables-are-local-p nil)
            ;; brace that opens a substatement block, indent to zero
            (c-set-offset 'substatement-open 0)
            ;; switch/case: make each case line indent from switch
            (c-set-offset 'case-label '+)
            ;; make open-braces after a case: statement indent to 0 (default was '+)
            (c-set-offset 'statement-case-open 0)
            ;; make DEL take all previous whitespace with it
            (setq c-hungry-delete-key t)
            ;; show in which function
            (which-function-mode t)
            ;; make #define be left-aligned
            (setq c-electric-pound-behavior (quote (alignleft)))
            ;; line number minor mode
            (linum-mode t)
            (make-face-unitalic 'font-lock-comment-face)
            ;; line width indication
            (fci-mode t)
            ;; enable cscope key bindings
            (enable-cscope-shortcut c-mode-map)
            (enable-cscope-shortcut c++-mode-map)))

;; emacs lisp mode
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (fci-mode t)
            (linum-mode)))

;; java mode
(add-hook 'java-mode-hook
          (lambda ()
            (fci-mode t)
            (linum-mode)
            (enable-cscope-shortcut java-mode-map)))

;; makefile mode
(add-hook 'makefile-mode-hook
          (lambda ()
            (linum-mode)))

;; python mode
(add-hook 'python-mode-hook
          (lambda ()
            (fci-mode t)
            (highlight-indentation-mode)
            (linum-mode)
            (flymake-python-pyflakes-load)))

;; ruby mode
(add-hook 'ruby-mode-hook
          (lambda ()
            (fci-mode t)
            (highlight-indentation-mode)
            (linum-mode)))

;; scheme mode
(add-hook 'scheme-mode-hook
          (lambda ()
            (fci-mode t)
            (linum-mode)
            (autopair-mode)))


(provide 'mode-hooks)
