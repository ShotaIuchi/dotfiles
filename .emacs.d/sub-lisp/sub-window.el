;; start screen hide
(setq inhibit-startup-message t)

;; start window-size :: maximized / fullscreen
(set-frame-parameter nil 'fullscreen 'maximized)

;; frame
(if window-system
    (progn
      (tool-bar-mode 0)
      (menu-bar-mode 0)
      (global-linum-mode 0)
      (scroll-bar-mode 0)
      (line-number-mode t)
      (column-number-mode t)))

;; popup window
(if (not (require 'popwin nil t))
    (message "!!! WORNING !!! | require : popwin")
  (setq display-buffer-function 'popwin:display-buffer)
  (setq popwin:popup-window-position 'bottom)
  (setq popwin:popup-window-height '20))


(provide 'sub-window)
