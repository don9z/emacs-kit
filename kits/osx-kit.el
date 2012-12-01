;; on osx, Emacs doesn't use the $PATH if it is not started from the shell
(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs 'exec-path' and PATH environment variable
to match that used by the user's shell."
  (interactive)
  (let ((path-from-shell
         (replace-regexp-in-string "[ \t\n]*$" ""
                                   (shell-command-to-string
                                    "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))
(set-exec-path-from-shell-PATH)

(setq mac-command-modifier 'meta)

(provide 'osx-kit)
