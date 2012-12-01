;; set theme
(add-to-list 'custom-theme-load-path
             (expand-file-name "blackboard-theme" kit-extensions-dir))
(load-theme 'blackboard t)

;; display current buffer file path on frame title
(setq frame-title-format
      '("%S" (buffer-file-name "%f" (dired-directory dired-directory "%b"))))

;; set frame size, cursor color and font
(setq default-frame-alist
      (append
       '(
         (left . 50)
         (top . 0)
         (width . 100)
         (height . 40)
         (cursor-color . "LawnGreen")
         (font . "-apple-Inconsolata-medium-normal-normal-*-14-*-*-*-m-0-iso10646-1")
         )
       default-frame-alist))

;; hide the toolbar
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; hide the scroll bars
(when (fboundp 'toggle-scroll-bar)
  (toggle-scroll-bar -1))

;; make the fringe thinner (default is 8 in pixels)
(if (fboundp 'fringe-mode)
    (fringe-mode 2))

;; make the cursor thinner (as modern editors do)
(setq default-cursor-type 'bar)

;; set column to 80
(setq-default fill-column 80)
;; highlight marked region
(setq transient-mark-mode t)
;; enable syntax highlight
(global-font-lock-mode t)
;; highlight the current line
(when (window-system)
    (global-hl-line-mode))
;; always show paren, brace, and curly brace "partners"
(show-paren-mode t)

;; configure mode line
(line-number-mode +1)
(column-number-mode +1)
(display-time-mode +1)

;; loop window transparency
(setq alpha-list '((100 100) (95 65) (85 55) (75 45) (65 35)))
(defun loop-alpha ()
  (interactive)
  (let ((alpha-value (car alpha-list)))
    ((lambda (left right)
       (set-frame-parameter (selected-frame) 'alpha (list left right))
       (add-to-list 'default-frame-alist (cons 'alpha (list left right))))
     (car alpha-value) (car (cdr alpha-value)))
    (setq alpha-list (cdr (append alpha-list (list alpha-value))))))


(provide 'ui-kit)
