;; ========================================================================
;;  window size
;; ------------------------------------------------------------------------
;;  maximized or fullscreen
;; ========================================================================
(set-frame-parameter nil 'fullscreen 'maximized)


;; ========================================================================
;;  window item
;; ========================================================================
(cond ((window-system)
       (tool-bar-mode 0)
       (menu-bar-mode 0)
       (global-linum-mode 0)
       (scroll-bar-mode 0)
       (line-number-mode t)
       (column-number-mode t))
      (t
       (menu-bar-mode 0)
       (global-linum-mode 0)
       (line-number-mode t)
       (column-number-mode t)))


;; ========================================================================
;;  startup
;; ========================================================================
(setq inhibit-startup-message t)


(provide 'sub-appearance)
