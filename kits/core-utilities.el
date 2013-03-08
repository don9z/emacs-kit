;;; core-utilities.el --- Emacs kit core utilities
;;
;; Copyright (c) 2012 Chris Zheng
;;
;; Author: Chris Zheng <chrisdcheng@gmail.com>
;; URL: https://github.com/don9z/emacs-kit
;; Version: 1.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file provides function utilities.

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

(defun kit-open-with ()
  "Open the file of a buffer in an external program"
  (interactive)
  (when buffer-file-name
    (shell-command (concat
                    (if (eq system-type 'darwin)
                        "open"
                      (read-shell-command "Open current file with: "))
                    " "
                    buffer-file-name))))

(defun kit-google ()
  "Googles a query or region if any."
  (interactive)
  (browse-url
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (url-hexify-string (if mark-active
         (buffer-substring (region-beginning) (region-end))
       (read-string "Google: "))))))

(defun kit-bounce-sexp ()
  "Will bounce between matching parens"
  (interactive)
  (let ((prev-char (char-to-string (preceding-char)))
	    (next-char (char-to-string (following-char))))
    (cond
     ((string-match "[[{(<]" next-char) (forward-sexp 1))
     ((string-match "[\]})>]" prev-char) (backward-sexp 1))
     (t (error "%s" "Not on a paren, brace, or bracket")))))

(defun kit-region-length ()
  "Length of a region"
  (interactive)
  (message (format "%d" (- (region-end) (region-beginning)))))

(defun kit-buffer-ignored (str)
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
   (string-match "^\\*Calendar\\*$" str)
   (string-match "^work.org$" str)
   (string-match "^refile.org$" str)
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
(defun kit-next-buffer (ls)
  "Switch to next buffer but skipping unwanted ones."
  (let* ((ptr ls) bf bn go)
    (while (and ptr (null go))
      (setq bf (car ptr)  bn (buffer-name bf))
      ;; skip over
      (if (null (kit-buffer-ignored bn))
          (setq go bf)
        (setq ptr (cdr ptr))))
    (if go (switch-to-buffer go))))
(defun kit-prev-use-buffer ()
  "Switch to previous buffer in current window."
  (interactive)
  (kit-next-buffer (reverse (buffer-list))))
(defun kit-next-use-buffer ()
  "Switch to the other buffer (2nd in list-buffer) in current window."
  (interactive)
  (bury-buffer (current-buffer))
  (kit-next-buffer (buffer-list)))

(defun kit-ascii-table ()
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

(defun kit-insert-date ()
  "Insert date at point"
  (interactive)
  (insert (format-time-string "%a %b %e, %Y %l:%M %p")))

(defun kit-dos2unix ()
  "Convert line end from dos to unix"
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t) (replace-match "")))
(defun kit-unix2dos ()
  "Convert line end from unix to dos"
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\n" nil t) (replace-match "\r\n")))

(defun kit-move-line (&optional n)
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
(defun kit-move-line-up (n)
  "Moves current line N (1) lines up leaving point in place."
  (interactive "p")
  (move-line (if (null n) -1 (- n))))
(defun kit-move-line-down (n)
  "Moves current line N (1) lines down leaving point in place."
  (interactive "p")
  (move-line (if (null n) 1 n)))

(defun kit-flip-word-case ()
  "Fip word case"
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

(defun kit-new-scratch ()
  "Create a new scratch buffer"
  (interactive)
  (progn
    (switch-to-buffer
     (get-buffer-create (generate-new-buffer-name "*scratch*")))
    (emacs-lisp-mode)))

(defun kit-insert-empty-line-above ()
  (interactive)
  (let ((pos (point))
        (cur-max (point-max)))
    (beginning-of-line)
    (newline-and-indent)
    (goto-char (+ pos (- (point-max) cur-max)))))

(defun kit-join-line-above ()
  (interactive)
  (join-line -1))

(defun kit-rotate-windows ()
  "Rotate your windows"
  (interactive)
  (cond ((not (> (count-windows)1))
         (message "You can't rotate a single window!"))
        (t
         (setq i 1)
         (setq numWindows (count-windows))
         (while  (< i numWindows)
           (let* (
                  (w1 (elt (window-list) i))
                  (w2 (elt (window-list) (+ (% i numWindows) 1)))

                  (b1 (window-buffer w1))
                  (b2 (window-buffer w2))

                  (s1 (window-start w1))
                  (s2 (window-start w2))
                  )
             (set-window-buffer w1  b2)
             (set-window-buffer w2 b1)
             (set-window-start w1 s2)
             (set-window-start w2 s1)
             (setq i (1+ i)))))))

(defun kit-reset-ui (&optional frame)
  "Reset UI to a frame split 80-80 horizontally"
  (interactive)
  (if frame (select-frame frame))
  (delete-other-windows)
  (if (window-system)
      (cond
       ((= 1050 (display-pixel-height))
        (set-frame-size (selected-frame) 170 60)
        (set-frame-position (selected-frame) 50 0))
       ((= 800 (display-pixel-height))
        (set-frame-size (selected-frame) 170 50)
        (set-frame-position (selected-frame) 20 0))
       (t
        (set-frame-size (selected-frame) 170 45)
        (set-frame-position (selected-frame) 20 0))
       ))
  (split-window-horizontally))

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
