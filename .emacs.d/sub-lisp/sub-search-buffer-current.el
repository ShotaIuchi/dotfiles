;; ========================================================================
;;  helm-imenu
;; ------------------------------------------------------------------------
;;   C-u C-i    .. imenu
;; ========================================================================
(when 'flag-helm-init
  (global-set-key (kbd "C-u C-i") 'helm-imenu))


;; ========================================================================
;;  helm-occure
;; ------------------------------------------------------------------------
;;   C-u C-o    .. occur
;; ========================================================================
(when 'flag-helm-init
  (global-set-key (kbd "C-u C-o") 'helm-occur))


;; ========================================================================
;;  helm-swoop
;; ------------------------------------------------------------------------
;;   C-u C-s    .. current buffer
;;   C-u C-a    .. all buffer
;; ========================================================================
(when 'flag-helm-init
  (if (not (require 'helm-swoop nil t))
      (message "!!! WORNING !!! | require : helm-swoop")
    (when (require 'migemo nil t)
      (helm-migemo-mode 1))
    (setq helm-multi-swoop-edit-save t)
    (setq helm-swoop-split-with-multiple-windows nil)
    (setq helm-swoop-split-direction 'split-window-vertically)
    (setq helm-swoop-speed-or-color nil)
    (setq helm-swoop-move-to-line-cycle t)
    (setq helm-swoop-use-line-number-face t)
    (setq helm-swoop-use-fuzzy-match t)
    (global-set-key (kbd "C-u C-s") 'helm-swoop)
    (global-set-key (kbd "C-u C-a") 'helm-multi-swoop-all)))


(provide 'sub-search-buffer-current)
