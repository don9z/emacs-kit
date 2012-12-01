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
(let ((default-directory kit-extensions-dir))
  (normal-top-level-add-subdirs-to-load-path))

(require 'builtin-kit)
(require 'packages-kit)
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
