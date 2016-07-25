;; ========================================================================
;;  MS windows
;; ========================================================================
(when (and
       (os-type-windows)
       (ui-type-gui))
  (unless (require 'cygwin-mount nil t)
    (message "!!! WORNING !!! | require : cygwin-mount"))
  (unless (require 'setup-cygwin nil t)
    (message "!!! WORNING !!! | require : cygwin-mount")))


(provide 'sub-os-windows)
