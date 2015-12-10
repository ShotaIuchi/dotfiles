;; ========================================================================
;;  C,C++
;; ========================================================================
(defun language-mode-hook ()
  (c-set-style "linux")
  (setq c-basic-offset 4)
  (setq c-tab-always-indent nil)
  (setq c-auto-newline t)
  (setq c-hungry-delete-key t)
  (setq indent-tabs-mode nil)

  (require 'rainbow-delimiters)
  (rainbow-delimiters-mode)
  )
(add-hook 'c-mode-hook 'language-mode-hook)
(add-hook 'c++-mode-hook 'language-mode-hook)

(provide 'sub-language)

