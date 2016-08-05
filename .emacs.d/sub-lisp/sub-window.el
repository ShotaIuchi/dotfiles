;; ========================================================================
;;  resize
;; ========================================================================
(global-set-key (kbd "M-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "M-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "M-<up>") 'shrink-window)
(global-set-key (kbd "M-<down>") 'enlarge-window)


;; ========================================================================
;;  popup
;; ========================================================================
(if (not (require 'popwin nil t))
    (message "!!! WORNING !!! | require : popwin")
  (setq display-buffer-function 'popwin:display-buffer)
  (setq popwin:popup-window-position 'bottom)
  (setq popwin:popup-window-height '20)
  ;; exception
  (push '("*grep*" :noselect t) popwin:special-display-config))


;; ========================================================================
;;  windata
;; ------------------------------------------------------------------------
;;   target : helm
;; ========================================================================
(if (not (require 'windata nil t))
    (message "!!! WORNING !!! | require : windata")
  ;;(setq helm-windata '(frame left 0.3 nil))
  (setq helm-windata '(frame left 0.5 nil))
  (defun my/helm-display-buffer (buffer)
    (apply 'windata-display-buffer buffer helm-windata))
  (setq helm-display-function 'my/helm-display-buffer))


(provide 'sub-window)
