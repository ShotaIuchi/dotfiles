;; ========================================================================
;;  color theme
;; ========================================================================
;; http://gnuemacscolorthemetest.googlecode.com/svn/html/index-java.html
;;(locate-library "color-theme")
;;(require 'color-theme)
;;(color-theme-initialize)
;;(color-theme-deep-blue)


;; ========================================================================
;;  theme
;; ========================================================================
(if (not (require 'tangotango-theme nil t))
    (message "!!! WORNING !!! | require : tangotango-theme")
  (load-theme 'tangotango t))


(if (not (require 'color-theme-solarized nil t))
    (message "!!! WORNING !!! | require : solarized-theme")
  (load-theme 'solarized t))


(provide 'sub-theme)
