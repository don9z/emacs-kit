;;; key-bindings.el --- Emacs kit key bindings
;;
;; Copyright (c) 2014 Chris Zheng
;;
;; Author: Chris Zheng <chrisdcheng@gmail.com>
;; URL: https://github.com/don9z/emacs-kit
;; Version: 1.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file sets key bindings

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

;; set window size
(global-set-key [C-left] 'enlarge-window-horizontally)
(global-set-key [C-right] 'shrink-window-horizontally)
(global-set-key [C-up] 'enlarge-window)
(global-set-key [C-down] 'shrink-window)
(global-set-key (kbd "M-+") 'balance-windows)

;; always use regex on searching
(global-set-key "\C-s" 'isearch-forward-regexp)
(global-set-key "\C-r" 'isearch-backward-regexp)
(global-set-key "\M-%" 'query-replace-regexp)

(global-set-key (kbd "C--") 'undo)
(global-set-key "\M-l" 'goto-line)

(global-set-key "\C-x\C-b" 'ibuffer)
(global-set-key "\M-s" 'save-buffer)
(add-hook 'org-agenda-mode-hook
          (lambda ()
            (define-key org-agenda-mode-map "\M-s" 'org-save-all-org-buffers)))
(global-set-key "\M-k" 'kill-this-buffer)
(global-set-key "\M-r" 'revert-buffer)

;; core utilities keybindings
(global-set-key "\C-\M-j" 'kit-join-line-above)
(global-set-key (kbd "<C-tab>") 'kit-rotate-windows)
(global-set-key (kbd "M-=") 'kit-bounce-sexp)
(global-set-key "\M-`" 'kit-next-use-buffer)
(global-set-key "\M-~" 'kit-prev-use-buffer)
(global-set-key "\M-u" 'kit-flip-word-case)
(global-set-key (kbd "<C-return>") 'kit-insert-empty-line-above)
;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'kit-smarter-move-beginning-of-line)

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
(global-set-key (kbd "C-M-0") 'delete-window)
(global-set-key (kbd "M-0") 'other-window)

;; fix conflicted key bindings
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

;; expand-region
(global-set-key (kbd "C-M-m") 'er/expand-region)
;; multiple-cursors
(global-set-key (kbd "C-,") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-.") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c C-.") 'mc/mark-all-like-this)
;; org
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-cc" 'org-capture)
;; highlight-symbol
(global-set-key "\C-ch" 'highlight-symbol-at-point)
;; magit
(global-set-key "\C-xg" 'magit-status)
;; jedi
(add-hook 'python-mode-hook
          (lambda ()
            (define-key python-mode-map (kbd "M-.") 'jedi:key-goto-definition)
            (define-key python-mode-map (kbd "M-/") 'jedi:dot-complete)
            ))

(provide 'key-bindings)
