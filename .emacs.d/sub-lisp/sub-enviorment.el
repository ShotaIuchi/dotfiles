(when (eq system-type 'windows-nt)
  (require 'cygwin-mount nil t)
  (require 'setup-cygwin nil t))

(provide 'sub-enviorment)
