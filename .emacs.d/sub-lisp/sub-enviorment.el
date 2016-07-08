(when (eq system-type 'windows-nt)
  (unless (require 'cygwin-mount nil t)
    (message "!!! WORNING !!! | require : cygwin-mount"))
  (unless (require 'setup-cygwin nil t)
    (message "!!! WORNING !!! | require : cygwin-mount")))


(provide 'sub-enviorment)
