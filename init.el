;------------------------------------------------------------------------;;
;; Profile
(setq user-full-name "Chris Zheng")

(setenv "LANG" "en_US.UTF-8")

;; Set extension dir location
(defvar emacs-d "~/Dropbox/Emacs/emacs.d/"
  "Location of all extensions in")
;; Add site-lisp to load path
(add-to-list 'load-path (concat emacs-d "site-lisp"))

;; File mode settings
(add-to-list 'auto-mode-alist '("\\.pac\\'" . javascript-mode))
(add-to-list 'auto-mode-alist '("\\.log\\'" . auto-revert-tail-mode))
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

;; Count words in buffer
(defun count-words-buffer ()
  "Count the number of words in current the buffer, print in the minibuffer"
  (interactive)
  (save-excursion
    (let ((count 0))
      (goto-char (point-min))
      (while (< (point) (point-max))
        (forward-word 1)
        (setq count (1+ count)))
      (message "buffer contains %d words." count))))

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
    '((left . 50) 
      (top . 0)
      (width . 100) 
      (height . 40)
      ) 
    default-frame-alist))
(add-to-list 'default-frame-alist 
             '(font . "-unknown-Inconsolata-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1"))

;; Font zoom out  C-x C-=
;; Font zoom in   C-x C--
;; Font reset     C-x C-0

;; Frame transparant
;;(add-to-list 'default-frame-alist (cons 'alpha (list 90 50)))

;; Slow down the mouse wheel acceleration
;; (when (boundp 'mouse-wheel-scroll-amount)
;;   (setq mouse-wheel-scroll-amount '(0.01)))

;; Hide the tool bar
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode 0))

;; Show menu bar on Mac and Terminal
(if (window-system)
    (if (boundp 'mac-option-modifier) (menu-bar-mode t) (menu-bar-mode nil))
  (menu-bar-mode t))
;; Show time
(display-time-mode 1)
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
;; Wrap line
(set-default 'truncate-partial-width-windows (not truncate-partial-width-windows))
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
;; Emacs gurus don't need no stinking scroll bars
(when (fboundp 'toggle-scroll-bar)
  (toggle-scroll-bar -1))
;; Prevent the annoying beep on errors, use flash instead
(setq visible-bell t)
;; No warnings
(setq ring-bell-function 'ignore)
;; For emacsclient
(server-start)
;; Set to use bash as Shell
(setq explicit-shell-file-name "/bin/bash")
(setq shell-file-name "/bin/bash")
;; Settings of shell-mode
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
;;------------------------------------------------------------------------;;


;;------------------------------------------------------------------------;;
;; Keybindings
(global-set-key [S-left] 'other-window)
(global-set-key [C-delete] 'kill-word)
(global-set-key [C-backspace] 'backward-kill-word)
(global-set-key [home] 'beginning-of-buffer)
(global-set-key [end] 'end-of-buffer)
(global-set-key [C-left] 'enlarge-window-horizontally)
(global-set-key [C-right] 'shrink-window-horizontally)
(global-set-key [C-up] 'enlarge-window)
(global-set-key [C-down] 'shrink-window)

(global-set-key "\C-s" 'isearch-forward-regexp)
(global-set-key "\C-r" 'isearch-backward-regexp)
(global-set-key "\M-%" 'query-replace-regexp)
(global-set-key (kbd "C--") 'undo)
(global-set-key "\C-x\C-b" 'ibuffer)
(global-set-key "\M-s" 'save-buffer)

;; Compile
(global-set-key "\M-6" 'compile)
(global-set-key "\M-^" 'next-error)
(global-set-key (kbd "C-6") 'kill-compilation)

(global-set-key "\M-7" 'shell-current-directory)
(global-set-key "\M-8" 'grep-find)

;; Indexing using cscope
(global-set-key "\M-9" 'cscope-find-global-definition)
(global-set-key "\M-(" 'cscope-pop-mark)
(global-set-key (kbd "C-9") 'cscope-find-functions-calling-this-function)

;; Map the window manipulation keys to meta 0, 1, 2, o
(global-set-key (kbd "M-3") 'split-window-horizontally)
(global-set-key (kbd "M-2") 'split-window-vertically)
(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-0") 'delete-window)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-+") 'balance-windows)
;; Replace dired's M-o
(add-hook 'dired-mode-hook 
          (lambda () 
            (define-key dired-mode-map (kbd "M-o") 'other-window)))
;; Replace ibuffer's M-o
(add-hook 'ibuffer-mode-hook 
          (lambda () 
            (define-key ibuffer-mode-map (kbd "M-o") 'other-window)))

(global-set-key "\M-k" 'kill-this-buffer)
(global-set-key "\M-l" 'goto-line)
(global-set-key "\M-u" 
                '(lambda () 
                   (interactive) (backward-word 1) (upcase-word 1)))
(global-set-key "\M-\S-u" 
                '(lambda () 
                   (interactive) (backward-word 1) (downcase-word 1)))
;; Reverse buffer
(global-set-key "\M-r" 'revert-buffer)

;; For Mac OS X meta key
(when (boundp 'mac-command-modifier)
  (setq mac-command-modifier 'meta))
;; Mac OS X conventions
(global-set-key (kbd "M-a") 'mark-whole-buffer)

;; Cheat sheet for key bindings
;; M-^ move current line to the end of prev line
;;------------------------------------------------------------------------;;


;;------------------------------------------------------------------------;;
;; mode settings
;;
;; c/c++ mode
;; set up my tab length as a variable
(defun get-my-tab-length () 4)
(defun my-c-mode-hook ()
  (local-set-key "\M-f" 'c-forward-into-nomenclature)
  (local-set-key "\M-b" 'c-backward-into-nomenclature)
  ;; Set C code mode to Kernighan and Ritchie mode
  (c-set-style "K&R")
  ;; Set tab width and c offset
  (setq tab-width (get-my-tab-length))
  (setq c-basic-offset (get-my-tab-length))
  (setq standard-indent (get-my-tab-length))
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
  ;; High light line minor mode
  (hl-line-mode t)
  ;; Line number minor mode
  (linum-mode t)
  ;; Setting for auto-complete
  (define-key c-mode-base-map "\M-/" 'ac-complete-clang)
  ;; Line width indication
  (fci-mode t)
  (make-face-unitalic 'font-lock-comment-face)
)
(add-hook 'c-mode-common-hook 'my-c-mode-hook)


(add-hook 'emacs-lisp-mode-hook
          (lambda () 
            ;; Line width indicator
            (fci-mode t)
            (hl-line-mode)
            (linum-mode)))

(add-hook 'java-mode-hook
          (lambda ()
            ;; Line width indicator
            (fci-mode t)
            (hl-line-mode)
            (linum-mode)))

(add-hook 'makefile-mode-hook
          (lambda ()
            (hl-line-mode)
            (linum-mode)))

(add-hook 'python-mode-hook
          (lambda ()
            ;; Line width indicator
            (fci-mode t)
            (highlight-indentation)
            (linum-mode)))

(add-hook 'ruby-mode-hook
          (lambda ()
            ;; Line width indicator
            (fci-mode t)
            (highlight-indentation)
            (linum-mode)))
;;------------------------------------------------------------------------;;


;;------------------------------------------------------------------------;;
;; Theme
(add-to-list 'custom-theme-load-path (concat emacs-d "custom-themes"))
(load-theme 'blackboard t)
;;------------------------------------------------------------------------;;


;;------------------------------------------------------------------------;;
;; Add marmalade to package repo
(require 'package)
(package-initialize)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
;;------------------------------------------------------------------------;;


;;------------------------------------------------------------------------;;
;; php mode
(require 'php-mode)
;;------------------------------------------------------------------------;;


;;------------------------------------------------------------------------;;
;; Load auto-complete
(add-to-list 'load-path (concat emacs-d "auto-complete"))
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories (concat emacs-d "auto-complete/dict"))
(ac-config-default)
(setq ac-modes
      (append ac-modes '(org-mode objc-mode jde-mode sql-mode
                                  change-log-mode text-mode
                                  makefile-gmake-mode makefile-bsdmake-mo
                                  autoconf-mode makefile-automake-mode)))

(require 'auto-complete-clang)
;; If you want to add a compiler flag (most of time is the include path), 
;; just create a file call .clang-completion-config.el under the directory you put your source in. 
;; With the content like this:
;; (setq clang-completion-flags (split-string "-pthread -I/usr/include/gtk-2.0 
;; -I/usr/lib/gtk-2.0/include -I/usr/include/atk-1.0 
;; -I/usr/include/cairo -I/usr/include/pango-1.0 -I/usr/include/gio-unix-2.0/ 
;; -I/usr/include/pixman-1 -I/usr/include/freetype2 -I/usr/include/libpng12 
;; -I/usr/include/glib-2.0 -I/usr/lib/glib-2.0/include"))
;;------------------------------------------------------------------------;;


;;------------------------------------------------------------------------;;
;; Load xcscope
(add-to-list 'load-path (concat emacs-d "cscope-15.7a/xcscope"))
;; Add cscope-indexer execute path
(when (file-exists-p (concat emacs-d "cscope-15.7a/xcscope/cscope-indexer"))
  (setq exec-path (append exec-path 
                          '((concat emacs-d "cscope-15.7a/xcscope/")))))
(require 'xcscope)
;; (add-hook 'java-mode-hook (function cscope:hook))
;; * Keybindings:
;;
;; All keybindings use the "C-c s" prefix, but are usable only while
;; editing a source file, or in the cscope results buffer:
;;
;;      C-c s s         Find symbol. *********
;;      C-c s d         Find global definition.
;;      C-c s g         Find global definition (alternate binding).
;;      C-c s G         Find global definition without prompting.
;;      C-c s c         Find functions calling a function. *********
;;      C-c s C         Find called functions (list functions called
;;                      from a function). *********
;;      C-c s t         Find text string.
;;      C-c s e         Find egrep pattern.
;;      C-c s f         Find a file.
;;      C-c s i         Find files #including a file.
;;
;; These pertain to navigation through the search results:
;;
;;      C-c s b         Display *cscope* buffer.
;;      C-c s B         Auto display *cscope* buffer toggle.
;;      C-c s n         Next symbol.
;;      C-c s N         Next file.
;;      C-c s p         Previous symbol.
;;      C-c s P         Previous file.
;;      C-c s u         Pop mark.
;;
;; These pertain to setting and unsetting the variable,
;; `cscope-initial-directory', (location searched for the cscope database
;;  directory):
;;
;;      C-c s a         Set initial directory. *********
;;      C-c s A         Unset initial directory.
;;
;; These pertain to cscope database maintenance:
;;
;;      C-c s L         Create list of files to index. *********
;;      C-c s I         Create list and index.
;;      C-c s E         Edit list of files to index.
;;      C-c s W         Locate this buffer's cscope directory
;;                      ("W" --> "where").
;;      C-c s S         Locate this buffer's cscope directory.
;;                      (alternate binding: "S" --> "show").
;;      C-c s T         Locate this buffer's cscope directory.
;;                      (alternate binding: "T" --> "tell").
;;      C-c s D         Dired this buffer's directory.
;;------------------------------------------------------------------------;;


;;------------------------------------------------------------------------;;
;; Load org-mode
(add-to-list 'load-path (concat emacs-d "org-mode/lisp"))
(add-to-list 'load-path (concat emacs-d "org-mode/contrib/lisp"))
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
;; MobileOrg
(setq org-directory "~/Dropbox/Documents/org")
(setq org-mobile-directory "~/Dropbox/MobileOrg")
(setq org-mobile-inbox-for-pull "~/Dropbox/Documents/org/refile.org")
(setq org-mobile-files(quote ("~/Dropbox/Documents/org/work.org")))
(setq org-mobile-force-id-on-agenda-items nil)
(add-hook 'after-init-hook 'org-mobile-pull)
(add-hook 'kill-emacs-hook 'org-mobile-push)
;; Task
(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "STARTED(s)" "|" "DONE(d!/!)")
              (sequence "WAITING(w@/@)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)"))))
(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("NEXT" :foreground "blue" :weight bold)
              ("STARTED" :foreground "blue" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
              ("HOLD" :foreground "magenta" :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold))))
(setq org-use-fast-todo-selection t)
;; (setq org-todo-state-tags-triggers
;;       (quote (("CANCELLED" ("CANCELLED" . t))
;;               ("WAITING" ("WAITING" . t))
;;               ("HOLD" ("WAITING" . t) ("HOLD" . t))
;;               ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
;;               ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
;;               ("STARTED" ("WAITING") ("CANCELLED") ("HOLD"))
;;               ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))
;; Enable clock when done
(setq org-log-done t)
;; Capture
(setq org-default-notes-file "~/Dropbox/Documents/org/refile.org")
(global-set-key "\C-cc" 'org-capture)
(setq org-capture-templates
      (quote (("t" "Todo" entry (file "~/Dropbox/Documents/org/refile.org")
               "* TODO %?\n  %U\n %i")
              ("n" "Note" entry (file "~/Dropbox/Documents/org/refile.org")
               "* %? :NOTE:\n  %U\n %i")
              ("j" "Journal" entry (file+datetree "~/Dropbox/Documents/org/refile.org")
               "* %?\n  %U\n %i")
              ("h" "Habit" entry (file "~/Dropbox/Documents/org/refile.org")
               "* NEXT %?\n  %U\nSCHEDULED: %t .+1d/3d\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n  %i"))))
(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))
(setq org-reverse-note-order t)
;; Agenda
(setq org-agenda-files (quote ("~/Dropbox/Documents/org/work.org")))
(setq org-agenda-dim-blocked-tasks nil)
(add-hook 'org-mode-hook 
          (lambda () (setq truncate-lines nil)))

;; TAB - Subtree cycling  S-TAB - Global cycling
;; M-RET - Insert same level heading  M-S-RET Insert TODO entry 
;; M-left/right - Promote/Demote current heading by one level
;; M-S-left/right - Promote/Demote current subtree by one level
;; M-S-up/down - Move subtree up/down
;; C-c C-t - Rotate TODO state 
;; C-c C-s - Schedule  C-c C-d - Deadline
;; C-c a t - Show the global TODO list
;; S-M-RET Insert a new TODO entry
;;------------------------------------------------------------------------;;


;;------------------------------------------------------------------------;;
;; Load yasnippet
(add-to-list 'load-path (concat emacs-d "yasnippet"))
(require 'yasnippet)
(yas/global-mode 1)
;;------------------------------------------------------------------------;;


;;------------------------------------------------------------------------;;
;; Enable highlight symbol
(require 'highlight-symbol)
(global-set-key "\C-ch" 'highlight-symbol-at-point)
;; (global-set-key [] 'highlight-symbol-next)
;; (global-set-key [] 'highlight-symbol-prev)
;; (global-set-key [] 'highlight-symbol-remove-all)
;;------------------------------------------------------------------------;;


;;------------------------------------------------------------------------;;
;; Enable cursor style
(require 'cursor-chg)  ; Load this library
(change-cursor-mode 1) ; On for overwrite/read-only/input mode
(toggle-cursor-type-when-idle 1) ; On when idle
;;------------------------------------------------------------------------;;


;;------------------------------------------------------------------------;;
;;Enable ido-mode
(require 'ido)
(ido-mode t)
;; Enable fuzzy matching
(setq ido-enable-flex-matching t)
;;------------------------------------------------------------------------;;


;;------------------------------------------------------------------------;;
;; Pinyin input
(add-to-list 'load-path (concat emacs-d "eim"))
(autoload 'eim-use-package "eim" "Another emacs input method")
;; Tooltip is not good for use
(setq eim-use-tooltip nil)
(register-input-method
 "eim-py" "euc-cn" 'eim-use-package
 "拼音" "汉字拼音输入法" "py.txt")
;; Use ; to input English
(require 'eim-extra)
(global-set-key ";" 'eim-insert-ascii)
;;------------------------------------------------------------------------;;


;;------------------------------------------------------------------------;;
;; Reverse all out of date buffers
(require 'revbufs)
;;------------------------------------------------------------------------;;


;;------------------------------------------------------------------------;;
;; (autoload 'flymake-find-file-hook "flymake" "" t)
;; (add-hook 'find-file-hook 'flymake-find-file-hook)
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
;; (when (file-exists-p "/usr/local/share/emacs/site-lisp/auctex.el")
;;   (load "/usr/local/share/emacs/site-lisp/auctex.el" nil t t)
;;   (load "/usr/local/share/emacs/site-lisp/preview-latex.el" nil t t))
;;------------------------------------------------------------------------;;


;;------------------------------------------------------------------------;;
;; Enable multiple shells
(require 'shell-current-directory);
;;------------------------------------------------------------------------;;


;;------------------------------------------------------------------------;;
(when (file-exists-p "/Applications/MIT-Scheme.app/Contents/Resources/mit-scheme")
  (setq scheme-program-name
        "/Applications/MIT-Scheme.app/Contents/Resources/mit-scheme")
  (require 'xscheme))
;;------------------------------------------------------------------------;;


;;------------------------------------------------------------------------;;
;; Set line indicator to column 80
(require 'fill-column-indicator)
;;------------------------------------------------------------------------;;


;;------------------------------------------------------------------------;;
(require 'highlight-indentation)
;;------------------------------------------------------------------------;;


;;------------------------------------------------------------------------;;
(add-to-list 'load-path (concat emacs-d "emacs-nav"))
(require 'nav)
;;------------------------------------------------------------------------;;


;;------------------------------------------------------------------------;;
(require 'markdown-mode)
;; Set .markdown files mode to markdown-mode
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
;;------------------------------------------------------------------------;;


;;------------------------------------------------------------------------;;
(require 'clojure-mode)
;; Set .clj files to clojure-mode
(add-to-list 'auto-mode-alist '("\\.clj\\'" . clojure-mode))
;;------------------------------------------------------------------------;;


;;------------------------------------------------------------------------;;
(require 'ftl)
(autoload 'turn-on-ftl-mode "ftl" nil t)
(add-hook 'html-mode-hook 'turn-on-ftl-mode t t)
(add-hook 'xml-mode-hook 'turn-on-ftl-mode t t)
(add-hook 'text-mode-hook 'turn-on-ftl-mode t t)
;;------------------------------------------------------------------------;;


;;------------------------------------------------------------------------;;
(add-to-list 'load-path (concat emacs-d "mark-multiple"))
(require 'inline-string-rectangle)
(global-set-key (kbd "C-x r t") 'inline-string-rectangle)
(require 'mark-more-like-this)
(global-set-key (kbd "C-,") 'mark-previous-like-this)
(global-set-key (kbd "C-.") 'mark-next-like-this)
;;------------------------------------------------------------------------;;


;;------------------------------------------------------------------------;;
(add-to-list 'load-path (concat emacs-d "expand-region"))
(require 'expand-region)
(global-set-key (kbd "C-M-m") 'er/expand-region)
;;------------------------------------------------------------------------;;


;;------------------------------------------------------------------------;;
(when (file-exists-p "/usr/local/share/doc/git-core/contrib/emacs")
  (add-to-list 'load-path "/usr/local/share/doc/git-core/contrib/emacs")
  (require 'git)
  (require 'git-blame))
;;------------------------------------------------------------------------;;


;;------------------------------------------------------------------------;;
(require 'rvm)
(rvm-use-default) ;; use rvm's default ruby for the current Emacs session
;;------------------------------------------------------------------------;;


;;------------------------------------------------------------------------;;
(require 'erc)
;; joining && autojoing
;; make sure to use wildcards for e.g. freenode as the actual server
;; name can be be a bit different, which would screw up autoconnect
(erc-autojoin-mode t)
(setq erc-autojoin-channels-alist
  '((".*\\.freenode.net" "#emacs" "#ruby" "#java")))
;;------------------------------------------------------------------------;;


;;------------------------------------------------------------------------;;
;; emacs-java
(add-to-list 'load-path (concat emacs-d "emacs-java"))
(require 'java-mode-plus)
(require 'java-docs)
(java-docs 
 "/Library/Java/JavaVirtualMachines/1.6.0_29-b11-402.jdk/Contents/Classes/")
;;------------------------------------------------------------------------;;


;;------------------------------------------------------------------------;;
(require 'protobuf-mode)
;;------------------------------------------------------------------------;;


(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(ecb-options-version "2.40")
  ;; Exclude .svn .git TAGS from grep-find path
 '(grep-find-command "find . -name .svn -prune -o -name TAGS -prune -o -name .git -prune -o -type f -print0 | xargs -0 grep -nHE "))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

;; Tips
;; sudo edit
;;   C-c C-f /sudo::/path/to/file
;; follow-mode
;;   Show same buffer in two windows but scroll as one.
;; Games
;;   tetris
;; Compile
;;   emacs -batch -f batch-byte-compile *.el
;; align-regexp
;; occur
;;   search for regex match lines in file, shows in other buffer
;; re-builder
;;   search lines match and mark
;; anything
;;   like quicksivler
;; tramp
;;   /sudo::/, /user@site:/, /user@site#port:/
;; rectangle
;;   cut: C-x r k -> C-x r y
;;   add with space: C-x r o
;;   replace with space: C-x r c
;;   delete: C-x r d
;;   replace with char: C-x r t, then input chars
;; wdired-change-to-wdired-mode
;;   edit dir like file
;; multi-occur-in-matching-buffers
;; apropos
;;   search for emacs commands with regex
;; follow-mode
;; M-l downcase-word
;;   Convert following word to lower case, moving over.
;; M-t transpose-words C-t transpose chars C-x C-t transpose-lines
;; M-m back-to-indentation
;; M-x revert-buffer-with-coding-system
;;   Change text coding system, e.g, from GBK to UTF-8