;;------------------------------------------------------------------------;;
;; Profile
(setq user-full-name "Chris Zheng")

;; Sync $PATH from terminal to Emacs
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

;; Set extension dir location
(defvar emacs-d "/Users/chris/Dropbox/emacs-kit/emacs.d/"
  "Location of all extensions in")
;; Add all dirs in emacs-d recursively to load-path
(let ((default-directory emacs-d))
  (normal-top-level-add-subdirs-to-load-path))
;;------------------------------------------------------------------------;;


;;------------------------------------------------------------------------;;
;; Packages sync at start
(require 'package)
(package-initialize)
;; (add-to-list 'package-archives
;;              '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))

(require 'cl)
;; Guarantee all packages are installed on start
(defvar packages-list
  '(
    auctex
    ascii
    auto-complete
    auto-indent-mode
    autopair
    clojure-mode
    csv-mode
    cursor-chg
    cyberpunk-theme
    dummy-h-mode
    ecb
    expand-region
    fill-column-indicator
    flymake-python-pyflakes
    git-blame
    golden-ratio
    highlight-indentation
    highlight-symbol
    ido-ubiquitous
    magit
    mark-multiple
    markdown-mode+
    melpa
    multi-term
    nav
    org ;; seems emacs 24 ships org, but put it here for sure
    php-mode
    protobuf-mode
    rainbow-mode
    scss-mode
    shell-here
    sicp
    web-mode
    yaml-mode
    yasnippet
    )
  "List of packages needs to be installed at launch")

(defun has-package-not-installed ()
  (loop for p in packages-list
        when (not (package-installed-p p)) do (return t)
        finally (return nil)))
(when (has-package-not-installed)
  ;; Check for new packages (package versions)
  (message "%s" "Get latest versions of all packages...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; Install the missing packages
  (dolist (p packages-list)
    (when (not (package-installed-p p))
      (package-install p))))
;;------------------------------------------------------------------------;;


;;------------------------------------------------------------------------;;
;; Auto mode & autoload
;; javascript-mode
(add-to-list 'auto-mode-alist '("\\.pac\\'" . javascript-mode))
;; auto-revert-tail-mode
(add-to-list 'auto-mode-alist '("\\.log\\'" . auto-revert-tail-mode))
;; markdown-mode
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
;; yaml-mode
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
;; dummy-h-mode
(add-to-list 'auto-mode-alist '("\\.h$" . dummy-h-mode))
(autoload 'dummy-h-mode "dummy-h-mode" "Dummy H mode" t)
;; web-mode
(add-to-list 'auto-mode-alist '("\\.phtml$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb$" . web-mode))
;; csv-mode
(add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode))
(autoload 'csv-mode "csv-mode"
  "Major mode for editing comma-separated value files." t)

;; revbufs
(autoload 'revbufs "revbufs" "" t)
;; fill-column-indicator
(autoload 'fci-mode "fill-column-indicator" "" t)
;; xscheme
(autoload 'run-scheme "xscheme" "" t)
;; geiser
(autoload 'run-geiser "geiser" "" t)
;; cscope
(setq exec-path (append exec-path (list (concat emacs-d "xcscope/"))))
(autoload 'cscope-set-initial-directory "xcscope" "" t)
;;------------------------------------------------------------------------;;


;;------------------------------------------------------------------------;;
;; Functions
(defun bounce-sexp ()
  "Will bounce between matching parens"
  (interactive)
  (let ((prev-char (char-to-string (preceding-char)))
	    (next-char (char-to-string (following-char))))
     (cond
       ((string-match "[[{(<]" next-char) (forward-sexp 1))
       ((string-match "[\]})>]" prev-char) (backward-sexp 1))
       (t (error "%s" "Not on a paren, brace, or bracket")))))
(global-set-key (kbd "M-=") 'bounce-sexp)

;; Compute the length of the marked region
(defun region-length ()
  "length of a region"
  (interactive)
  (message (format "%d" (- (region-end) (region-beginning)))))

;; Buffer-switching methods
(defun buffer-ignored (str)
  (or
    ;;buffers I don't want to switch to
	(string-match "^\\*Buffer List\\*$" str)
    (string-match "^\\*GNU Emacs\\*$" str)
	(string-match "^TAGS" str)
	(string-match "^\\*Messages\\*$" str)
	(string-match "^\\*Completions\\*$" str)
	(string-match "^\\*scratch\\*$" str)
	(string-match "^\\*ESS\\*$" str)
	(string-match "^ " str)
	(string-match "Mew message" str)
	(string-match "output\\*$" str)
	(string-match "^\\*TeX silent\\*$" str)
    (string-match "^\\*\[e|E\]diff.*\\*$" str)
    (string-match "^\\*Help\\*$" str)
    (string-match "^refile.org" str)
    (string-match "^\\*magit-.*\\*$" str)
    (with-current-buffer (get-buffer str)
      (eq major-mode 'dired-mode))
    ;;Test to see if the window is visible on an existing visible frame.
    ;;Because I can always ALT-TAB to that visible frame, I never want to
    ;;Ctrl-TAB to that buffer in the current frame.  That would cause
    ;;a duplicate top-level buffer inside two frames.
    (memq str
      (mapcar
        (lambda (x)
          (buffer-name
            (window-buffer
              (frame-selected-window x))))
        (visible-frame-list)))))
(defun next-buffer (ls)
  "Switch to next buffer in ls skipping unwanted ones."
  (let* ((ptr ls) bf bn go)
    (while (and ptr (null go))
      (setq bf (car ptr)  bn (buffer-name bf))
      ;; Skip over
      (if (null (buffer-ignored bn))
        (setq go bf)
        (setq ptr (cdr ptr))))
    (if go (switch-to-buffer go))))
(defun prev-use-buffer ()
  "Switch to previous buffer in current window."
  (interactive)
  (next-buffer (reverse (buffer-list))))
(defun next-use-buffer ()
  "Switch to the other buffer (2nd in list-buffer) in current window."
  (interactive)
  (bury-buffer (current-buffer))
  (next-buffer (buffer-list)))
(global-set-key "\M-`" 'next-use-buffer)
(global-set-key "\M-~" 'prev-use-buffer)

;; Show ascii table
(defun ascii-table ()
  "Print the ascii table"
  (interactive)
  (switch-to-buffer "*ASCII*")
  (erase-buffer)
  (insert (format "ASCII characters up to number %d.\n" 254))
  (let ((i 0))
    (while (< i 254)
      (setq i (+ i 1))
      (insert (format "%4d %c\n" i i))))
  (beginning-of-buffer))

;; Insert date into buffer
(defun insert-date ()
  "Insert date at point"
  (interactive)
  (insert (format-time-string "%a %b %e, %Y %l:%M %p")))

;; Convert a buffer from dos ^M end of lines to unix end of lines
(defun dos2unix ()
  (interactive)
    (goto-char (point-min))
      (while (search-forward "\r" nil t) (replace-match "")))
;; vice versa
(defun unix2dos ()
  (interactive)
    (goto-char (point-min))
      (while (search-forward "\n" nil t) (replace-match "\r\n")))

(defun move-line (&optional n)
  "Move current line N (1) lines up/down leaving point in place."
  (interactive "p")
  (when (null n)
    (setq n 1))
  (let ((col (current-column)))
    (beginning-of-line)
    (next-line 1)
    (transpose-lines n)
    (previous-line 1)
    (forward-char col)))
(defun move-line-up (n)
  "Moves current line N (1) lines up leaving point in place."
  (interactive "p")
  (move-line (if (null n) -1 (- n))))
(defun move-line-down (n)
  "Moves current line N (1) lines down leaving point in place."
  (interactive "p")
  (move-line (if (null n) 1 n)))
(global-set-key [M-up] 'move-line-up)
(global-set-key [M-down] 'move-line-down)

;; A no-op function to bind to if you want to set a keystroke to null
(defun void ()
  "this is a no-op"
  (interactive))
;;------------------------------------------------------------------------;;


;;------------------------------------------------------------------------;;
;; UI settings
;; Using this method to guarantee that font in speedbar is same size as buffer
(setq default-frame-alist
   (append
     '(
       (left . 50)
       (top . 0)
       (width . 100)
       (height . 40)
       (cursor-color . "#ff7700")
       ;;(font . "-apple-Source_Code_Pro-medium-normal-normal-*-13-*-*-*-m-0-iso10646-1")
       (font . "-apple-Inconsolata-medium-normal-normal-*-14-*-*-*-m-0-iso10646-1")
       )
     default-frame-alist))

;; Loop window transparency
(setq alpha-list '((100 100) (95 65) (85 55) (75 45) (65 35)))
(defun loop-alpha ()
  (interactive)
  (let ((alpha-value (car alpha-list)))
    ((lambda (left right)
       (set-frame-parameter (selected-frame) 'alpha (list left right))
       (add-to-list 'default-frame-alist (cons 'alpha (list left right))))
     (car alpha-value) (car (cdr alpha-value)))
    (setq alpha-list (cdr (append alpha-list (list alpha-value))))))

;; Hide the tool bar
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode 0))
;; Show menu bar on Mac and Terminal
(if (window-system)
    (if (boundp 'mac-option-modifier) (menu-bar-mode t) (menu-bar-mode nil))
  (menu-bar-mode t))
;; Emacs gurus don't need no stinking scroll bars
(when (fboundp 'toggle-scroll-bar)
  (toggle-scroll-bar -1))

;; Show time
(display-time-mode 1)

(defun enter-my-chinese-writing-mode ()
  "Set font in current buffer"
  (interactive)
  (setq buffer-face-mode-face '(:family "Microsoft YaHei"))
  (buffer-face-mode))
(defun leave-my-chinese-writing-mode ()
  "Unset buffer face"
  (interactive)
  (buffer-face-mode -1))
;;------------------------------------------------------------------------;;


;;------------------------------------------------------------------------;;
;; Miscellaneous
;; Show paren, brace, and curly brace "partners" at all times
(show-paren-mode t)
;; Show column and line number on mode line
(setq column-number-mode t)
(setq line-number-mode t)
;; Big buffer pool
(setq kill-ring-max 200)
;; TAB set
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
;; Save bookmarks to default file
(setq bookmark-save-flag 1)
;; Column to 80
(setq default-fill-column 80)
;; Syntax highlight
(global-font-lock-mode t)
(setq transient-mark-mode t)
;; Allow paste between emacs and external application
(setq x-select-enable-clipboard t)
;; Stop creating those backup~ files
(setq make-backup-files nil)
;; Stop creating those #***# files
(setq auto-save-default nil)
;; Add new line to file end
(setq require-final-newline t)
;; No new lines when you press the "arrow-down key" at end of the buffer
(setq next-line-add-newlines nil)
;; Make the y or n suffice for a yes or no question
(fset 'yes-or-no-p 'y-or-n-p)
;; Kill whole if curson is at line beginning using ctrl+k
(setq kill-whole-line t)
;; Highlight C/C++ warning
(global-cwarn-mode 1)
;; Compile command
(setq compile-command "make -C ")
;; Display current buffer file path to frame title
(setq frame-title-format
  '("%S" (buffer-file-name "%f" (dired-directory dired-directory "%b"))))
;; Prevent the annoying beep on errors, use flash instead
(setq visible-bell t)
;; No warnings
(setq ring-bell-function 'ignore)
;; Remove trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; Always auto indent
(auto-indent-mode)
;; Join next line with white-space deleted when kill at the end of a line
(setq auto-indent-kill-line-at-eol 'nil)
;; highlight current line
(if (window-system)
    (global-hl-line-mode))

;; Settings of shell-mode
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
;;------------------------------------------------------------------------;;


;;------------------------------------------------------------------------;;
;; Keybindings
(global-set-key [home] 'beginning-of-buffer)
(global-set-key [end] 'end-of-buffer)
(global-set-key [C-left] 'enlarge-window-horizontally)
(global-set-key [C-right] 'shrink-window-horizontally)
(global-set-key [C-up] 'enlarge-window)
(global-set-key [C-down] 'shrink-window)
(global-set-key [S-left] 'other-window)

(global-set-key "\C-s" 'isearch-forward-regexp)
(global-set-key "\C-r" 'isearch-backward-regexp)
(global-set-key "\M-%" 'query-replace-regexp)

(global-set-key (kbd "C--") 'undo)
(global-set-key "\C-x\C-b" 'ibuffer)
(global-set-key "\M-s" 'save-buffer)
(global-set-key "\M-k" 'kill-this-buffer)
(global-set-key "\M-l" 'goto-line)
(global-set-key "\M-u"
                '(lambda ()
                   (interactive) (backward-word 1) (upcase-word 1)))
(global-set-key "\M-\S-u"
                '(lambda ()
                   (interactive) (backward-word 1) (downcase-word 1)))
(global-set-key "\M-r" 'revert-buffer)

;; Compile
(global-set-key "\M-6" 'compile)
(global-set-key "\M-^" 'next-error)
(global-set-key (kbd "C-6") 'kill-compilation)

;; grep-find
(global-set-key "\M-8" 'grep-find)
(setq grep-find-command
      "find . -name .svn -prune -o -name TAGS -prune -o -name .git -prune -o -type f -print0 | xargs -0 grep -nHE ")

;; shell-here
(global-set-key "\M-7" 'shell-here)

;; Indexing using cscope
(defun enable-cscope-shortcut (language-mode-map)
  (require 'xcscope)
  (define-key language-mode-map "\M-9" 'cscope-find-global-definition)
  (define-key language-mode-map "\M-(" 'cscope-pop-mark)
  (define-key language-mode-map (kbd "C-9")
    'cscope-find-functions-calling-this-function))

;; Map the window manipulation keys to meta 0, 1, 2, o
(global-set-key (kbd "M-3") 'split-window-horizontally)
(global-set-key (kbd "M-2") 'split-window-vertically)
(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-0") 'delete-window)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-+") 'balance-windows)
;; Replace some modes' key binding
(add-hook 'dired-mode-hook
          (lambda ()
            (define-key dired-mode-map (kbd "M-o") 'other-window)))
(add-hook 'ibuffer-mode-hook
          (lambda ()
            (define-key ibuffer-mode-map (kbd "M-o") 'other-window)))
(add-hook 'diff-mode-hook
          (lambda ()
            (define-key diff-mode-map (kbd "M-o") 'other-window)))
(add-hook 'scheme-interaction-mode-hook
          (lambda ()
            (define-key scheme-interaction-mode-map (kbd "M-o") 'other-window)))
(add-hook 'scheme-mode-hook
          (lambda ()
            (define-key scheme-mode-map (kbd "M-o") 'other-window)
            (define-key scheme-mode-map (kbd "M-O") 'xscheme-send-buffer)))
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

;; For Mac OS X meta key
(when (boundp 'mac-command-modifier)
  (setq mac-command-modifier 'meta))
;; Mac OS X conventions
(global-set-key (kbd "M-a") 'mark-whole-buffer)

;; expand-region
(global-set-key (kbd "C-M-m") 'er/expand-region)
;; mark-multiple
(global-set-key (kbd "C-x r t") 'inline-string-rectangle)
(global-set-key (kbd "C-,") 'mark-previous-like-this)
(global-set-key (kbd "C-.") 'mark-next-like-this)
;; org
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-cc" 'org-capture)
;; highlight-symbol
(global-set-key "\C-ch" 'highlight-symbol-at-point)
;;------------------------------------------------------------------------;;


;;------------------------------------------------------------------------;;
;; mode settings

;; c/c++ mode
(add-hook 'c-mode-common-hook
          (lambda ()
            (local-set-key "\M-f" 'c-forward-into-nomenclature)
            (local-set-key "\M-b" 'c-backward-into-nomenclature)
            ;; Set C code mode to Kernighan and Ritchie mode
            (c-set-style "K&R")
            ;; Set tab width and c offset
            (setq c-tab-width 4)
            (setq tab-width c-tab-width)
            (setq c-basic-offset c-tab-width)
            (setq standard-indent c-tab-width)
            ;; Make style variables gloable
            (setq c-style-variables-are-local-p nil)
            ;; Brace that opens a substatement block, indent to zero
            (c-set-offset 'substatement-open 0)
            ;; Switch/case: make each case line indent from switch
            (c-set-offset 'case-label '+)
            ;; Make open-braces after a case: statement indent to 0 (default was '+)
            (c-set-offset 'statement-case-open 0)
            ;; Make DEL take all previous whitespace with it
            (setq c-hungry-delete-key t)
            ;; Show in which function
            (which-function-mode t)
            ;; Make a #define be left-aligned
            (setq c-electric-pound-behavior (quote (alignleft)))
            ;; Line number minor mode
            (linum-mode t)
            (make-face-unitalic 'font-lock-comment-face)
            ;; Line width indication
            (fci-mode t)
            (enable-cscope-shortcut c-mode-map)
            (enable-cscope-shortcut c++-mode-map)))

;; emacs lisp mode
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (fci-mode t)
            (linum-mode)))

;; java mode
(add-hook 'java-mode-hook
          (lambda ()
            (fci-mode t)
            (linum-mode)
            (enable-cscope-shortcut java-mode-map)
            ))

;; makefile mode
(add-hook 'makefile-mode-hook
          (lambda ()
            (linum-mode)))

;; python mode
(add-hook 'python-mode-hook
          (lambda ()
            (fci-mode t)
            (highlight-indentation-mode)
            (linum-mode)
            (flymake-python-pyflakes-load)))

;; ruby mode
(add-hook 'ruby-mode-hook
          (lambda ()
            (fci-mode t)
            (highlight-indentation-mode)
            (linum-mode)))

;; scheme mode
(add-hook 'scheme-mode-hook
          (lambda ()
            (fci-mode t)
            (linum-mode)
            (autopair-mode)))
;;------------------------------------------------------------------------;;


;;------------------------------------------------------------------------;;
;; Theme
(add-to-list 'custom-theme-load-path (concat emacs-d "blackboard-theme"))
(if (window-system)
    (load-theme 'blackboard t))
;;------------------------------------------------------------------------;;

;;------------------------------------------------------------------------;;
;; Load auto-complete
(require 'auto-complete-config)
(ac-config-default)
(setq ac-modes
      (append ac-modes '(org-mode objc-mode sql-mode text-mode)))
;;------------------------------------------------------------------------;;


;;------------------------------------------------------------------------;;
;; Load yasnippet
(require 'yasnippet)
(yas/global-mode 1)
(require 'dropdown-list)
(setq yas-prompt-functions '(yas-dropdown-prompt
                             yas-ido-prompt
                             yas-completing-prompt))
;;------------------------------------------------------------------------;;


;;------------------------------------------------------------------------;;
;; org
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
;; Enable org-habit
(eval-after-load "org"
  '(progn
     (add-to-list 'org-modules 'org-habit)))
;; Task
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
;; Parent can't be marked as done unless all children are done
(setq org-enforce-todo-dependencies t)
(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)
;; Show CLOSED: [timestamp]
(setq org-log-done 'time)
;; Capture
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
;; Agenda
(setq org-agenda-files (quote ("~/Dropbox/Documents/org/work.org")))
(setq org-agenda-dim-blocked-tasks nil)
(add-hook 'org-mode-hook
          (lambda () (setq truncate-lines nil)))
;; MobileOrg
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
;;------------------------------------------------------------------------;;


;;------------------------------------------------------------------------;;
;; Enable cursor style
(require 'cursor-chg)
;; On for overwrite/read-only/input mode
(change-cursor-mode 1)
;; On when idle
(toggle-cursor-type-when-idle 1)
;;------------------------------------------------------------------------;;


;;------------------------------------------------------------------------;;
;; Enable ido-mode
(require 'ido)
(ido-mode t)
;; Enable fuzzy matching
(setq ido-enable-flex-matching t)
;;------------------------------------------------------------------------;;


;;------------------------------------------------------------------------;;
;; Enable ido-ubiquitous
(ido-ubiquitous-mode)
;;------------------------------------------------------------------------;;


;;------------------------------------------------------------------------;;
(setq flymake-gui-warnings-enabled nil)
(setq flymake-log-level 0)
;; Show error message in mini buffer
(defun my-flymake-show-help ()
   (when (get-char-property (point) 'flymake-overlay)
     (let ((help (get-char-property (point) 'help-echo)))
       (if help (message "%s" help)))))
(add-hook 'post-command-hook 'my-flymake-show-help)
;; Add to Makefile:
;; .PHONY: check-syntax
;; check-syntax:
;;------------------------------------------------------------------------;;


;;------------------------------------------------------------------------;;
;; Set aspell as spell check tool
(setq ispell-program-name "aspell"
      ispell-extra-args '("--sug-mode=ultra"))
;;------------------------------------------------------------------------;;


;;------------------------------------------------------------------------;;
(eval-after-load "fill-column-indicator"
  '(progn
     (setq fci-rule-color "#303030")
     (setq fci-rule-use-dashes t)))
;;------------------------------------------------------------------------;;


;;------------------------------------------------------------------------;;
;; erc
(eval-after-load "erc"
  '(progn
     (setq erc-nick "don9z")
     (erc-autojoin-mode t)
     (setq erc-autojoin-channels-alist
           '((".*\\.freenode.net" "#emacs")))
     ))
;;------------------------------------------------------------------------;;


;;------------------------------------------------------------------------;;
;; ecb
(eval-after-load "ecb"
  '(progn
     (setq ecb-tip-of-the-day nil)
     ;; Too fix the error, kinda workaround
     (setq stack-trace-on-error t)
     (defun ecb-enable-own-temp-buffer-show-futition (switch) switch)))
;;------------------------------------------------------------------------;;


;; For emacsclient
(server-start)
