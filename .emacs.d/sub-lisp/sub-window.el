;; ========================================================================
;; start screen (hide)
;; ========================================================================
(setq inhibit-startup-message t)

;; ========================================================================
;; start size
;;  > maximized or fullscreen
;; ========================================================================
(set-frame-parameter nil 'fullscreen 'maximized)

;; ========================================================================
;; resize
;; ========================================================================
(global-set-key (kbd "M-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "M-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "M-<up>") 'shrink-window)
(global-set-key (kbd "M-<down>") 'enlarge-window)

;; ========================================================================
;; window system
;; ========================================================================
(if window-system
    (progn
      (tool-bar-mode 0)
      (menu-bar-mode 0)
      (global-linum-mode 0)
      (scroll-bar-mode 0)
      (line-number-mode t)
      (column-number-mode t)))

;; ========================================================================
;; popup
;; ========================================================================
(if (not (require 'popwin nil t))
    (message "!!! WORNING !!! | require : popwin")
  (setq display-buffer-function 'popwin:display-buffer)
  (setq popwin:popup-window-position 'bottom)
  (setq popwin:popup-window-height '20)
  ;; exception
  (push '("*grep*" :noselect t) popwin:special-display-config))


(provide 'sub-window)
