;; ========================================================================
;;  gradle
;; ========================================================================
(if (not (require 'gradle-mode nil t))
    (message "!!! WORNING !!! | require : gradle-mode")
  (gradle-mode 1))


(provide 'sub-filetype-build)
