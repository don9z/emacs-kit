;;; init.el --- Emacs kit for GNU Emacs
;;
;; Copyright (c) 2012 Chris Zheng
;;
;; Author: Chris Zheng <chrisdcheng@gmail.com>
;; URL: https://github.com/don9z/emacs-kit
;; Version: 1.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file sets up load path and loads kits files

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

(message
 "Emacs kit is starting up... Be patient, Master %s!"
 (getenv "USER"))

(defvar emacs-kit-dir (file-name-directory load-file-name)
  "The root directory of the Emacs kit")

(defvar kit-modules-dir (expand-file-name "kits" emacs-kit-dir)
  "Directory of official kits files")
(add-to-list 'load-path kit-modules-dir)

(defvar kit-extensions-dir (expand-file-name "extensions" emacs-kit-dir)
  "Directory of extensions that are not available in ELPA or MELPA")
(add-to-list 'load-path kit-extensions-dir)

(let ((default-directory kit-extensions-dir))
  (normal-top-level-add-subdirs-to-load-path))

(defvar kit-snippets-dir
  (expand-file-name "extensions/yasnippet-snippets" emacs-kit-dir)
  "Directory of snippets used by yasnippets")

(defvar kit-org-dir (expand-file-name "org" emacs-kit-dir)
  "Directory of org agenda files")
(when (not (file-exists-p kit-org-dir))
  (message "Make directory to store org files: %s" kit-org-dir)
  (make-directory kit-org-dir))

(require 'packages-kit)
(require 'builtin-kit)
(require 'core-utilities)
(require 'ui-kit)
(require 'key-bindings)
(require 'mode-hooks)

;; OSX specific settings
(when (eq system-type 'darwin)
  (require 'osx-kit))

(server-start)

(message
 "Emacs kit is equipped. May the force be with you, Master %s!"
 (getenv "USER"))
