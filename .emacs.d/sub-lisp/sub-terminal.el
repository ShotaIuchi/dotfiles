;; ========================================================================
;;  multi-term
;; ========================================================================
(if (not (require 'multi-term nil t))
    (message "!!! WORNING !!! | require : multi-term")
  (when (file-exists-p "/bin/zsh")
    (setq multi-term-program "/bin/zsh")))


(provide 'sub-terminal)
