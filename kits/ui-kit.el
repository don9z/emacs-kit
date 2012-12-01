;; UI settings
;; frame size, cursor color and font setting
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

;; hide the tool bar
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode 0))
;; show menu bar only on Mac and Terminal
(if (window-system)
    (if (boundp 'mac-option-modifier) (menu-bar-mode t) (menu-bar-mode nil))
  (menu-bar-mode t))
;; emacs gurus don't need no stinking scroll bars
(when (fboundp 'toggle-scroll-bar)
  (toggle-scroll-bar -1))

;; theme
(add-to-list 'custom-theme-load-path (expand-file-name "blackboard-theme" kit-extensions-dir))
(if (window-system)
    (load-theme 'blackboard t))

;; font setting
(defun enter-my-chinese-writing-mode ()
  "Set font in current buffer"
  (interactive)
  (setq buffer-face-mode-face '(:family "Microsoft YaHei"))
  (buffer-face-mode))
(defun leave-my-chinese-writing-mode ()
  "Unset buffer face"
  (interactive)
  (buffer-face-mode -1))

(provide 'ui-kit)
