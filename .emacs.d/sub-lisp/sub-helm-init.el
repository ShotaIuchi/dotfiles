;; ========================================================================
;;  helm
;; ========================================================================
(defvar flag-helm-init 0)
(if (not (require 'helm-config nil t))
    (message "!!! WORNING !!! | require : helm-config")
  (require 'helm)
  (setq helm-idle-delay 1.2)
  (setq helm-input-idle-delay 1.2)
  (defconst flag-helm-init t))


(provide 'sub-helm-init)
