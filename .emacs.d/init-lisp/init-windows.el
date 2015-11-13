(when (eq system-type 'windows-nt)
  (require 'cygwin-mount)
  (require 'setup-cygwin))

(provide 'sub-windows)

