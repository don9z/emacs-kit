;;; packages-kit.el --- Emacs kit packages settings
;;
;; Copyright (c) 2012 Chris Zheng
;;
;; Author: Chris Zheng <chrisdcheng@gmail.com>
;; URL: https://github.com/don9z/emacs-kit
;; Version: 1.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file is for packages from ELPA or MELPA

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

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
    dropdown-list
    dummy-h-mode
    ecb
    ess
    expand-region
    fill-column-indicator
    flymake-python-pyflakes
    git-blame
    go-mode
    golden-ratio
    highlight-indentation
    highlight-symbol
    ido-ubiquitous
    jedi
    magit
    markdown-mode
    multiple-cursors
    melpa
    nav
    php-mode
    powerline
    protobuf-mode
    rainbow-mode
    rvm
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
(add-to-list 'auto-mode-alist '("\\.thrift$" . thrift-mode))

(autoload 'csv-mode "csv-mode" "Major mode for CSV files" t)
(autoload 'dummy-h-mode "dummy-h-mode" "Dummy H mode" t)
(autoload 'fci-mode "fill-column-indicator" "Show column indicator" t)
(autoload 'run-scheme "xscheme" "Run mit-scheme" t)
(autoload 'run-geiser "geiser" "Geiser" t)
(autoload 'jedi:setup "jedi" nil t)
(autoload 'thrift-mode "thrift-mode" "Thrift mode" t)
(autoload 'dash-at-point "dash-at-point" "Search with Dash" t nil)

;; auto-complete
(require 'auto-complete-config)
(ac-config-default)
(setq ac-modes
      (append ac-modes '(org-mode objc-mode sql-mode text-mode)))

;; yasnippet
(require 'yasnippet)
(setq yas-snippet-dirs (list kit-snippets-dir))
(yas-global-mode 1)
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
