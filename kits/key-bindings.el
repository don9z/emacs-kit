(global-set-key [C-left] 'enlarge-window-horizontally)
(global-set-key [C-right] 'shrink-window-horizontally)
(global-set-key [C-up] 'enlarge-window)
(global-set-key [C-down] 'shrink-window)

(global-set-key "\C-s" 'isearch-forward-regexp)
(global-set-key "\C-r" 'isearch-backward-regexp)
(global-set-key "\M-%" 'query-replace-regexp)

(global-set-key (kbd "C--") 'undo)
(global-set-key "\M-l" 'goto-line)

(global-set-key "\C-x\C-b" 'ibuffer)
(global-set-key "\M-s" 'save-buffer)
(global-set-key "\M-k" 'kill-this-buffer)
(global-set-key "\M-r" 'revert-buffer)

;; compilation
(global-set-key "\M-6" 'compile)
(global-set-key "\M-^" 'next-error)
(global-set-key (kbd "C-6") 'kill-compilation)

;; shell-here
(global-set-key "\M-7" 'shell-here)

;; grep-find
(global-set-key "\M-8" 'grep-find)
(setq grep-find-command
      "find . -name .svn -prune -o -name TAGS -prune -o -name .git -prune -o -type f -print0 | xargs -0 grep -nHE ")

;; map the window manipulation keys to meta 0, 1, 2, o
(global-set-key (kbd "M-3") 'split-window-horizontally)
(global-set-key (kbd "M-2") 'split-window-vertically)
(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-0") 'delete-window)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-+") 'balance-windows)

;; fix conflicted key bindings
(add-hook 'dired-mode-hook
          (lambda ()
            (define-key dired-mode-map (kbd "M-o") 'other-window)))
(add-hook 'ibuffer-mode-hook
          (lambda ()
            (define-key ibuffer-mode-map (kbd "M-o") 'other-window)))
(add-hook 'diff-mode-hook
          (lambda ()
            (define-key diff-mode-map (kbd "M-o") 'other-window)))
(add-hook 'scheme-interaction-mode-hook
          (lambda ()
            (define-key scheme-interaction-mode-map (kbd "M-o") 'other-window)))
(add-hook 'scheme-mode-hook
          (lambda ()
            (define-key scheme-mode-map (kbd "M-o") 'other-window)
            (define-key scheme-mode-map (kbd "M-O") 'xscheme-send-buffer)))
(add-hook 'magit-mode-hook
          (lambda ()
            (define-key magit-mode-map (kbd "M-1") 'delete-other-windows)
            (define-key magit-mode-map (kbd "M-2") 'split-window-vertically)
            (define-key magit-mode-map (kbd "M-3") 'split-window-horizontally)))
(add-hook 'geiser-repl-mode-hook
          (lambda ()
            (define-key geiser-repl-mode-map (kbd "M-`") 'next-use-buffer)))
(add-hook 'geiser-mode-hook
          (lambda ()
            (define-key geiser-mode-map (kbd "M-`") 'next-use-buffer)))

;; indexing using cscope
(defun enable-cscope-shortcut (language-mode-map)
  (require 'xcscope)
  (define-key language-mode-map "\M-9" 'cscope-find-global-definition)
  (define-key language-mode-map "\M-(" 'cscope-pop-mark)
  (define-key language-mode-map (kbd "C-9")
    'cscope-find-functions-calling-this-function))
;; expand-region
(global-set-key (kbd "C-M-m") 'er/expand-region)
;; mark-multiple
(global-set-key (kbd "C-x r t") 'inline-string-rectangle)
(global-set-key (kbd "C-,") 'mark-previous-like-this)
(global-set-key (kbd "C-.") 'mark-next-like-this)
;; org
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-cc" 'org-capture)
;; highlight-symbol
(global-set-key "\C-ch" 'highlight-symbol-at-point)
;;------------------------------------------------------------------------;;

(provide 'key-bindings)
