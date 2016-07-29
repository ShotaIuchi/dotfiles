;; ========================================================================
;;  auto-complete
;; ========================================================================
(if (not (require 'auto-complete-config nil t))
    (message "!!! WORNING !!! | require : auto-complete-config")
  ;;(global-auto-complete-mode t)
  (setq ac-dwim t)
  (setq ac-auto-start nil)
  (ac-set-trigger-key "TAB")
  (ac-config-default)
  (define-key ac-completing-map (kbd "TAB") 'auto-complete)
  (define-key ac-completing-map (kbd "C-n") 'ac-next)
  (define-key ac-completing-map (kbd "C-p") 'ac-previous))


(provide 'sub-auto-complete)
