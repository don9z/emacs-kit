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
    auto-indent-mode
    autopair
    clojure-mode
    csv-mode
    cursor-chg
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
    org ;; seems emacs 24 ships org, but put it here for sure
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

(defun has-package-not-installed ()
  (loop for p in packages-list
        when (not (package-installed-p p)) do (return t)
        finally (return nil)))

(defun install-packages ()
  (when (has-package-not-installed)
    ;; Check for new packages (package versions)
    (message "%s" "Get latest versions of all packages...")
    (package-refresh-contents)
    (message "%s" " done.")
    ;; Install the missing packages
    (dolist (p packages-list)
      (when (not (package-installed-p p))
        (package-install p)))))

(install-packages)

;; org-mode
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
;; javascript-mode
(add-to-list 'auto-mode-alist '("\\.pac\\'" . javascript-mode))
;; auto-revert-tail-mode
(add-to-list 'auto-mode-alist '("\\.log\\'" . auto-revert-tail-mode))
;; markdown-mode
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
;; yaml-mode
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
;; dummy-h-mode
(add-to-list 'auto-mode-alist '("\\.h$" . dummy-h-mode))
(autoload 'dummy-h-mode "dummy-h-mode" "Dummy H mode" t)
;; web-mode
(add-to-list 'auto-mode-alist '("\\.phtml$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb$" . web-mode))
;; csv-mode
(add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode))
(autoload 'csv-mode "csv-mode"
  "Major mode for editing CSV files" t)

;; fill-column-indicator
(autoload 'fci-mode "fill-column-indicator" "Fill column indicator" t)
;; xscheme
(autoload 'run-scheme "xscheme" "Run mit-scheme" t)
;; geiser
(autoload 'run-geiser "geiser" "Geiser" t)
;; cscope
(setq exec-path (append exec-path (list (expand-file-name "xscope/" kit-extensions-dir))))
(autoload 'cscope-set-initial-directory "xcscope" "Cscope" t)


;;------------------------------------------------------------------------;;
;; auto-complete
(require 'auto-complete-config)
(ac-config-default)
(setq ac-modes
      (append ac-modes '(org-mode objc-mode sql-mode text-mode)))
;;------------------------------------------------------------------------;;


;;------------------------------------------------------------------------;;
;; yasnippet
(require 'yasnippet)
(yas/global-mode 1)
(require 'dropdown-list)
(setq yas-prompt-functions '(yas-dropdown-prompt
                             yas-ido-prompt
                             yas-completing-prompt))
;;------------------------------------------------------------------------;;


;;------------------------------------------------------------------------;;
;; cursor-chg
(require 'cursor-chg)
;; on for overwrite/read-only/input mode
(change-cursor-mode 1)
;; on when idle
(toggle-cursor-type-when-idle 1)
;;------------------------------------------------------------------------;;


;;------------------------------------------------------------------------;;
;; Enable ido-ubiquitous
(ido-ubiquitous-mode)
;;------------------------------------------------------------------------;;


;;------------------------------------------------------------------------;;
;; fill-column-indicator
(eval-after-load "fill-column-indicator"
  '(progn
     (setq fci-rule-color "#303030")
     (setq fci-rule-use-dashes t)))
;;------------------------------------------------------------------------;;


;;------------------------------------------------------------------------;;
;; ecb
(eval-after-load "ecb"
  '(progn
     (setq ecb-tip-of-the-day nil)
     ;; Too fix the error, kinda workaround
     (setq stack-trace-on-error t)
     (defun ecb-enable-own-temp-buffer-show-futition (switch) switch)))
;;------------------------------------------------------------------------;;


;;------------------------------------------------------------------------;;
;; auto-indent-mode
(auto-indent-mode)
;; join next line with white-space deleted when kill at the end of a line
(setq auto-indent-kill-line-at-eol 'nil)
;;------------------------------------------------------------------------;;

(provide 'packages-kit)
