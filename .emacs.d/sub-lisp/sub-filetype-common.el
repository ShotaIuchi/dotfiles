;; ========================================================================
;;  indent
;; ========================================================================
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 4)


;; ========================================================================
;;  hook
;; ========================================================================
(defun hook-common ()
  (setq tab-width 4)
  (setq indent-tabs-mode nil)
  (setq c-basic-offset 4)
  (hs-minor-mode 1)
  (local-set-key (kbd "C-u C-c") 'hs-hide-level)
  (local-set-key (kbd "C-u C-t") 'hs-toggle-hiding)
  )


(provide 'sub-filetype-common)
