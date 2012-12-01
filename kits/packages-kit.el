(require 'cl)
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

(defvar packages-list
  '(
    auctex
    ascii
    auto-complete
    autopair
    clojure-mode
    csv-mode
    cyberpunk-theme
    dummy-h-mode
    ecb
    expand-region
    fill-column-indicator
    flymake-python-pyflakes
    git-blame
    golden-ratio
    highlight-indentation
    highlight-symbol
    ido-ubiquitous
    magit
    mark-multiple
    markdown-mode
    melpa
    nav
    php-mode
    protobuf-mode
    rainbow-mode
    scss-mode
    shell-here
    sicp
    web-mode
    yaml-mode
    yasnippet
    )
  "List of packages needs to be installed at launch")

(defun install-packages ()
  (defun has-package-not-installed ()
    (loop for p in packages-list
          when (not (package-installed-p p)) do (return t)
          finally (return nil)))
  (when (has-package-not-installed)
    (message "%s" "Refreshing packages database...")
    (package-refresh-contents)
    (message "%s" "Done refreshing")
    ;; install the missing packages
    (dolist (p packages-list)
      (when (not (package-installed-p p))
        (package-install p)))))
(install-packages)

(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.h$" . dummy-h-mode))
(add-to-list 'auto-mode-alist '("\\.phtml$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode))

(setq exec-path (append exec-path (list (expand-file-name "xscope/" kit-extensions-dir))))
(autoload 'cscope-set-initial-directory "xcscope" "Cscope" t)
(autoload 'csv-mode "csv-mode" "Major mode for CSV files" t)
(autoload 'dummy-h-mode "dummy-h-mode" "Dummy H mode" t)
(autoload 'fci-mode "fill-column-indicator" "Show column indicator" t)
(autoload 'run-scheme "xscheme" "Run mit-scheme" t)
(autoload 'run-geiser "geiser" "Geiser" t)


;; auto-complete
(require 'auto-complete-config)
(ac-config-default)
(setq ac-modes
      (append ac-modes '(org-mode objc-mode sql-mode text-mode)))

;; yasnippet
(require 'yasnippet)
(yas/global-mode 1)
(require 'dropdown-list)
(setq yas-prompt-functions '(yas-dropdown-prompt
                             yas-ido-prompt
                             yas-completing-prompt))

;; enable ido-ubiquitous
(ido-ubiquitous-mode)

;; fill-column-indicator
(eval-after-load "fill-column-indicator"
  '(progn
     (setq fci-rule-color "#303030")
     (setq fci-rule-use-dashes t)))

;; ecb
(eval-after-load "ecb"
  '(progn
     (setq ecb-tip-of-the-day nil)
     ;; Too fix the error, kinda workaround
     (setq stack-trace-on-error t)
     (defun ecb-enable-own-temp-buffer-show-futition (switch) switch)))


(provide 'packages-kit)
