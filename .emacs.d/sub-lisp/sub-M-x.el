;; ========================================================================
;;  smex
;; ------------------------------------------------------------------------
;;   M-x            .. smex
;;   C-c C-c M-x    .. M-x (default)
;; ========================================================================
(if (not (require 'smex nil t))
    (message "!!! WORNING !!! | require : smex")
  (smex-initialize)
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command))


;; ========================================================================
;;  helm
;; ------------------------------------------------------------------------
;;   C-u C-x
;; ========================================================================
(when 'flag-helm-init
  (global-set-key (kbd "C-u C-x") 'helm-M-x))


(provide 'sub-M-x)
