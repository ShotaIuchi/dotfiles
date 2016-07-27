;; ========================================================================
;;  skk
;; ========================================================================
(if (not (require 'skk nil t))
    (message "!!! WORNING !!! | require : skk")
  (global-set-key (kbd "C-x j") 'skk-mode)
  (global-set-key (kbd "C-x C-j") 'skk-mode)
  (global-set-key (kbd "C-x C-\\") 'skk-auto-fill-mode)
  (setq default-input-method "japanese-skk")
  (unless (require 'skk-study nil t)
    (message "!!! WORNING !!! | require : skk-study")))


(provide 'sub-ime)
