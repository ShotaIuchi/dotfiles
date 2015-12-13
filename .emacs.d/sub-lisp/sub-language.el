;; ========================================================================
;;  C,C++
;; ========================================================================
(defun c-mode-hook-impl ()
  (c-set-style "linux")
  (setq c-basic-offset 4)
  (setq c-tab-always-indent nil)
  (setq c-auto-newline t)
  (setq c-hungry-delete-key t)
  (setq indent-tabs-mode nil))
(add-hook 'c-mode-hook 'c-mode-hook-impl)
(add-hook 'c++-mode-hook 'c-mode-hook-impl)


(provide 'sub-language)

