(message
 "Emacs kit is starting up... Be patient, Master %s!"
 (getenv "USER"))

;; Set emacs kit root dir and add to load-path
(defvar emacs-kit-dir (file-name-directory load-file-name))
;; Add kit modules to load-path
(defvar kit-modules-dir (expand-file-name "kits" emacs-kit-dir))
(add-to-list 'load-path kit-modules-dir)
;; Add extensions dir to load-path as well as its subdirs
(defvar kit-extensions-dir (expand-file-name "extensions" emacs-kit-dir))
(let ((default-directory kit-extensions-dir))
  (normal-top-level-add-subdirs-to-load-path))

;; Sync $PATH from Shell to Emacs
(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable
to match that used by the user's shell."
  (interactive)
  (let ((path-from-shell
         (replace-regexp-in-string "[ \t\n]*$" ""
                                   (shell-command-to-string
                                    "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))
(set-exec-path-from-shell-PATH)

(require 'builtin-kit)

(require 'packages-kit)

(require 'core-utilities)

(require 'ui-kit)

(require 'key-bindings)

(require 'mode-hooks)

;; For emacsclient
(server-start)

(message
 "Emacs kit is equipped. May the force be with you, Master %s!"
 (getenv "USER"))
