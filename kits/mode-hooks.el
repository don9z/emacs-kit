;;; mode-hooks.el --- Emacs kit mode hooks
;;
;; Copyright (c) 2014 Chris Zheng
;;
;; Author: Chris Zheng <chrisdcheng@gmail.com>
;; URL: https://github.com/don9z/emacs-kit
;; Version: 1.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file sets mode hooks.

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

;; c/c++ mode
(defun enable-cscope ()
  (when (file-exists-p "/usr/local/bin/cscope-indexer")
    (require 'xcscope)))
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
            ;; enable cscope
            (enable-cscope)
            ;; set compile command
            (setq compile-command "make -C ")
            ;; line width indication
            (fci-mode t)
            ))

;; emacs lisp mode
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (fci-mode t)
            ))

;; java mode
(add-hook 'java-mode-hook
          (lambda ()
            (enable-cscope)
            (fci-mode t)
            ))

;; python mode
(add-hook 'python-mode-hook
          (lambda ()
            (highlight-indentation-mode)
            (flymake-python-pyflakes-load)
            (fci-mode t)
            ))

;; ruby mode
(add-hook 'ruby-mode-hook
          (lambda ()
            (fci-mode t)
            (highlight-indentation-mode)
            ))

;; scheme mode
(add-hook 'scheme-mode-hook
          (lambda ()
            (fci-mode t)
            (autopair-mode)
            ))

;; shell mode
(add-hook 'shell-mode-hook
          (lambda ()
            (linum-mode -1)))

;; olivetti mode
;; disable word-wrap to fix chinese character line truncate problem
(add-hook 'olivetti-mode-hook
          (lambda ()
            (setq word-wrap nil)))

(provide 'mode-hooks)
