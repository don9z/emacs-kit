;; c/c++ mode
(add-hook 'c-mode-common-hook
          (lambda ()
            ;; set style to Kernighan and Ritchie mode
            (c-set-style "k&r")
            ;; highlight c warnings
            (cwarn-mode t)
            ;; set indent width
            (setq c-basic-offset tab-width)
            (setq standard-indent tab-width)
            ;; zero indent the brace that opens a substatement block
            (c-set-offset 'substatement-open 0)
            ;; make each 'case' line indent below 'switch'
            (c-set-offset 'case-label '+)
            ;; align '#define' to the leftmost of a line
            (setq c-electric-pound-behavior (quote (alignleft)))
            ;; show function name in mode line
            (which-function-mode t)
            ;; set compile command
            (setq compile-command "make -C ")
            ;; do not use italic font on comments
            (make-face-unitalic 'font-lock-comment-face)
            ;; display line number as sidebar
            (linum-mode t)
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
