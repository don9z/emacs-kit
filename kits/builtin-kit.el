;; set environment coding system
(set-language-environment "UTF-8")
;; show paren, brace, and curly brace "partners"
(show-paren-mode t)
;; big kill ring buffer pool
(setq kill-ring-max 200)
;; set TAB set
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
;; save bookmarks to default file
(setq bookmark-save-flag 1)
;; set column to 80
(setq default-fill-column 80)
;; enable syntax highlight
(global-font-lock-mode t)
;; enable mark highlight
(setq transient-mark-mode t)
;; allow paste between emacs and external applications
(setq x-select-enable-clipboard t)
;; stop creating those backup~ files
(setq make-backup-files nil)
;; stop creating those #***# files
(setq auto-save-default nil)
;; always add new line to the end of a file
(setq require-final-newline t)
;; no new lines when you press the "arrow-down key" at end of the buffer
(setq next-line-add-newlines nil)
;; make the y or n suffice for a yes or no question
(fset 'yes-or-no-p 'y-or-n-p)
;; kill whole if curson is at line beginning
(setq kill-whole-line t)
;; highlight C/C++ warning
(global-cwarn-mode 1)
;; set compile command
(setq compile-command "make -C ")
;; prevent the annoying beep on errors, use flash instead
(setq visible-bell t)
(setq ring-bell-function 'ignore)
;; remove trailing whitespace before save
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; highlight current line
(if (window-system)
    (global-hl-line-mode))

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
(setq comint-input-ring-size 5000)
;; show all in emacs interactive output
(setenv "PAGER" "cat")
;; set lang to enable Chinese display in shell-mode
(setenv "LANG" "en_US.UTF-8")


;; enable org-habit
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
(setq org-default-notes-file "~/Dropbox/Documents/org/refile.org")
(setq org-capture-templates
      (quote (("t" "Todo" entry (file "~/Dropbox/Documents/org/refile.org")
               "* TODO %?\n  OPENED: %U\n %i")
              ("n" "Note" entry (file "~/Dropbox/Documents/org/refile.org")
               "* %?\n  OPENED: %U\n %i")
              ("j" "Journal" entry (file "~/Dropbox/Documents/org/refile.org")
               "* %?\n  OPENED: %U\n %i")
              ("h" "Habit" entry (file "~/Dropbox/Documents/org/refile.org")
               "* TODO %?\n  SCHEDULED: %t\n  OPENED: %U\n  :PROPERTIES:\n  :STYLE: habit\n  :END:\n  %i"))))
(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))
(setq org-reverse-note-order t)
;; agenda
(setq org-agenda-files (quote ("~/Dropbox/Documents/org/work.org")))
(setq org-agenda-dim-blocked-tasks nil)
(add-hook 'org-mode-hook
          (lambda () (setq truncate-lines nil)))
;; mobileOrg
(setq org-directory "~/Dropbox/Documents/org")
(setq org-mobile-directory "~/Dropbox/MobileOrg")
(setq org-mobile-inbox-for-pull "~/Dropbox/Documents/org/refile.org")
(setq org-mobile-files(quote ("~/Dropbox/Documents/org/work.org")))
(setq org-mobile-force-id-on-agenda-items nil)
;; moble sync
;; (add-hook 'after-init-hook 'org-mobile-pull)
;; (add-hook 'kill-emacs-hook 'org-mobile-push)
(defvar org-mobile-sync-timer nil)
(defvar org-mobile-sync-idle-secs (* 60 10))
(defun org-mobile-sync ()
  (interactive)
  (org-mobile-pull)
  (org-mobile-push))
(defun org-mobile-sync-enable ()
  "enable mobile org idle sync"
  (interactive)
  (setq org-mobile-sync-timer
        (run-with-idle-timer org-mobile-sync-idle-secs t
                             'org-mobile-sync)));
(defun org-mobile-sync-disable ()
  "disable mobile org idle sync"
  (interactive)
  (cancel-timer org-mobile-sync-timer))
(org-mobile-sync-enable)

;; ido-mode
(ido-mode t)
;; enable fuzzy matching
(setq ido-enable-flex-matching t)

;; erc
(eval-after-load "erc"
  '(progn
     (setq erc-nick "don9z")
     (erc-autojoin-mode t)
     (setq erc-autojoin-channels-alist
           '((".*\\.freenode.net" "#emacs")))
     ))

;; ispell
(setq ispell-program-name "aspell"
      ispell-extra-args '("--sug-mode=ultra"))

(provide 'builtin-kit)
