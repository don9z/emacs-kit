;;; builtin-kit.el --- Emacs kit builtin settings
;;
;; Copyright (c) 2014 Chris Zheng
;;
;; Author: Chris Zheng <chrisdcheng@gmail.com>
;; URL: https://github.com/don9z/emacs-kit
;; Version: 1.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file is for global preferences and built-in packages.

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

;; set find-file default path
(setq default-directory "~/")
;; set environment coding system
(set-language-environment "UTF-8")
;; auto revert buffer globally
(global-auto-revert-mode t)
;; set TAB and indention
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
;; kill line with RET if cursor is at the line beginning
(setq kill-whole-line t)
;; save bookmark during set
(setq bookmark-save-flag 1)
;; y or n is suffice for a yes or no question
(fset 'yes-or-no-p 'y-or-n-p)
;; stop creating those backup~ files
(setq make-backup-files nil)
;; stop creating those #autosave# files
(setq auto-save-default nil)
;; always add new line to the end of a file
(setq require-final-newline t)
;; add no new lines when "arrow-down key" at the end of a buffer
(setq next-line-add-newlines nil)
;; prevent the annoying beep on errors
(setq ring-bell-function 'ignore)
;; remove trailing whitespaces before save
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; enable to support navigate in camelCase words
(global-subword-mode t)
;; hide startup splash screen
(setq inhibit-startup-screen t)

;; shell-mode settings
(setq explicit-shell-file-name "/bin/bash")
(setq shell-file-name "/bin/bash")
;; always insert at the bottom
(setq comint-scroll-to-bottom-on-input t)
;; no duplicates in command history
(setq comint-input-ignoredups t)
;; what to run when i press enter on a line above the current prompt
(setq comint-get-old-input (lambda () ""))
;; max shell history size
(setq comint-input-ring-size 1000)
;; show all in emacs interactive output
(setenv "PAGER" "cat")
;; set lang to enable Chinese display in shell-mode
(setenv "LANG" "en_US.UTF-8")
;; fix easypg error "Inappropriate ioctl for device" when open .gpg file
(setq epa-pinentry-mode 'loopback)

;; org-mode
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
;; javascript-mode
(add-to-list 'auto-mode-alist '("\\.pac\\'" . javascript-mode))
;; auto-revert-tail-mode
(add-to-list 'auto-mode-alist '("\\.log\\'" . auto-revert-tail-mode))


;; org-mode
(eval-after-load "org"
  '(progn
     (add-to-list 'org-modules 'org-habit)))
;; tasks
(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "STARTED(s)" "WAITING(w@/!)"
                        "|" "DONE(d!/!)" "CANCELLED(c@/!)"))))
(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("STARTED" :foreground "blue" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold))))
(setq org-use-fast-todo-selection t)
;; parent can't be marked as done unless all children are done
(setq org-enforce-todo-dependencies t)
(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)
;; show CLOSED: [timestamp]
(setq org-log-done 'time)
;; capture
(setq org-default-notes-file (expand-file-name "refile.org" kit-org-dir))
(setq org-capture-templates
      (quote (("t" "Todo" entry (file "") "* TODO %?\n  OPENED: %U\n %i")
              ("n" "Note" entry (file "") "* %?\n  OPENED: %U\n %i")
              ("j" "Journal" entry (file "") "* %?\n  OPENED: %U\n %i")
              ("h" "Habit" entry (file "")
               "* TODO %?\n  SCHEDULED: %t\n  OPENED: %U\n  :PROPERTIES:\n  :STYLE: habit\n  :END:\n  %i"))))
(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))
(setq org-reverse-note-order t)
;; agenda
(setq org-agenda-files (list (expand-file-name "work.org" kit-org-dir)))
(setq org-agenda-dim-blocked-tasks nil)
(add-hook 'org-mode-hook
          (lambda () (setq truncate-lines nil)))


;; ido-mode
(ido-mode t)
(setq ido-enable-flex-matching t)

;; erc
(eval-after-load "erc"
  '(progn
     (setq erc-nick "don9z")
     (erc-autojoin-mode t)
     (setq erc-autojoin-channels-alist
           '((".*\\.freenode.net" "#emacs")))))

;; ispell
(setq ispell-program-name "aspell"
      ispell-extra-args '("--sug-mode=ultra"))


(provide 'builtin-kit)
