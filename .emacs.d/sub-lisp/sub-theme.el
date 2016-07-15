;; http://gnuemacscolorthemetest.googlecode.com/svn/html/index-java.html
;(locate-library "color-theme")
;(require 'color-theme)
;(color-theme-initialize)
;(color-theme-deep-blue)

(if (not (require 'tangotango-theme nil t))
    (message "!!! WORNING !!! | require : tangotango-theme")
  (load-theme 'tangotango t))


(provide 'sub-theme)
