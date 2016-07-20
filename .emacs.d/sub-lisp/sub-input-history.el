;; ========================================================================
;;  redo+
;; ------------------------------------------------------------------------
;;   C-.
;; ========================================================================
(if (not (require 'redo+))
    (message "!!! WORNING !!! | require : redo+")
  (global-set-key (kbd "C-.") 'redo)
  (setq undo-no-redo t)
  (setq undo-limit 600000)
  (setq undo-strong-limit 900000))


(provide 'sub-input-history)
