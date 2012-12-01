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

(defun region-length ()
  "Length of a region"
  (interactive)
  (message (format "%d" (- (region-end) (region-beginning)))))

(defun buffer-ignored (str)
  (or
   ;;buffers I don't want to switch to
   (string-match "^\\*GNU Emacs\\*$" str)
   (string-match "^TAGS" str)
   (string-match "^\\*Messages\\*$" str)
   (string-match "^\\*Completions\\*$" str)
   (string-match "^\\*ESS\\*$" str)
   (string-match "^ " str)
   (string-match "Mew message" str)
   (string-match "output\\*$" str)
   (string-match "^\\*TeX silent\\*$" str)
   (string-match "^\\*\[e|E\]diff.*\\*$" str)
   (string-match "^\\*Help\\*$" str)
   (string-match "^\\*magit-.*\\*$" str)
   (string-match "^\\*Compile-Log*\\*$" str)
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
      ;; skip over
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

(defun insert-date ()
  "Insert date at point"
  (interactive)
  (insert (format-time-string "%a %b %e, %Y %l:%M %p")))

(defun dos2unix ()
  "Convert line end from dos to unix"
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t) (replace-match "")))
(defun unix2dos ()
  "Convert line end from unix to dos"
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

(defun flip-word-case ()
  (interactive)
  (defvar flag 't)
  (backward-word 1)
  (cond
   (flag
    (upcase-word 1)
    (setq flag 'nil))
   (t
    (downcase-word 1)
    (setq flag 't))))
(global-set-key "\M-u"
                'flip-word-case)

;; switch font setting from default to YaHei
(defun enter-my-chinese-writing-mode ()
  "Set font in current buffer"
  (interactive)
  (setq buffer-face-mode-face '(:family "Microsoft YaHei"))
  (buffer-face-mode))
(defun leave-my-chinese-writing-mode ()
  "Unset buffer face"
  (interactive)
  (buffer-face-mode -1))


(provide 'core-utilities)
