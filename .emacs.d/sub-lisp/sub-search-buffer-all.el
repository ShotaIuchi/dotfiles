;; ========================================================================
;;  color-moccur
;; ------------------------------------------------------------------------
;;   C-u C-p    .. all buffer
;;   C-u C-M-p  .. grep
;; ========================================================================
(if (not (require 'color-moccur nil t))
    (message "!!! WORNING !!! | require : color-moccur")
  (global-set-key (kbd "C-u C-p") 'moccur)
  (global-set-key (kbd "C-u C-M-p") 'moccur-grep))


(provide 'sub-search-buffer-all)
