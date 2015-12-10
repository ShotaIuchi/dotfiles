;; ========================================================================
;;  C,C++
;; ========================================================================
(defun language-mode-hook ()
  (require 'google-c-style)
  (google-set-c-style)
  )
(add-hook 'c-mode-hook 'language-mode-hook)
(add-hook 'c++-mode-hook 'language-mode-hook)

(provide 'sub-language)

