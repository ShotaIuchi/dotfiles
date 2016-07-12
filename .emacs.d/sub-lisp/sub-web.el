;; ========================================================================
;; google translate
;; ========================================================================
(if (not (require 'google-translate nil t))
    (message "!!! WORNING !!! | require : google-translate")
  (global-set-key (kbd "C-c C-t") 'google-translate-at-point)
  (global-set-key (kbd "C-c C-e") 'google-translate-query-translate)
  (global-set-key (kbd "C-c C-j") 'google-translate-query-translate-reverse)
  (custom-set-variables
   '(google-translate-default-source-language "en")
   '(google-translate-default-target-language "ja")))


(provide 'sub-web)
