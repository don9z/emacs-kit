;;; ui-kit.el --- Emacs kit user interface settings
;;
;; Copyright (c) 2014 Chris Zheng
;;
;; Author: Chris Zheng <chrisdcheng@gmail.com>
;; URL: https://github.com/don9z/emacs-kit
;; Version: 1.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file is for Emacs UI.

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

;; set theme
(when (window-system)
  ;;(load-theme 'blackboard t)
  )

;; display current buffer file path on frame title
(setq frame-title-format
      '("%S" (buffer-file-name "%f" (dired-directory dired-directory "%b"))))

;; enable auto save the desktop
;; (variables, buffers, frame and window configuration)
(desktop-save-mode)

;; set cursor color to green
(set-cursor-color "LawnGreen")
;; make the cursor thinner (as modern editors do)
(setq-default cursor-type 'bar)

;; set font per language
;; default
(set-face-attribute
  'default nil :font "Inconsolata 14")
;; chinese
(dolist (charset '(han))
    (set-fontset-font (frame-parameter nil 'font)
                      charset
                      (font-spec :family "Microsoft Yahei" :size 14)))

;; hide the toolbar
(tool-bar-mode -1)

;; hide the scroll bars
(toggle-scroll-bar -1)

;; always show linum fringe
(global-nlinum-mode +1)
;; enlarge linum width to fix number being cut off problem
(setq nlinum-format "%d ")

;; set the fringe width left 8 right 1
(fringe-mode '(8 . 1))
;; set fringe color same as background color
(set-face-attribute 'fringe nil :background nil)

;; set column to 80
(setq-default fill-column 80)
;; highlight marked region
(setq transient-mark-mode t)
;; enable syntax highlight
(global-font-lock-mode t)
;; always show paren, brace, and curly brace "partners"
(show-paren-mode t)

;; configure mode line
(line-number-mode +1)
(column-number-mode +1)
(display-time-mode +1)

;; loop window transparency
(setq alpha-list '((100 100) (95 65) (85 55) (75 45) (65 35)))
(defun loop-alpha ()
  (interactive)
  (let ((alpha-value (car alpha-list)))
    ((lambda (left right)
       (set-frame-parameter (selected-frame) 'alpha (list left right))
       (add-to-list 'default-frame-alist (cons 'alpha (list left right))))
     (car alpha-value) (car (cdr alpha-value)))
    (setq alpha-list (cdr (append alpha-list (list alpha-value))))))


(provide 'ui-kit)
