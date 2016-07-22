;; ========================================================================
;;  elscreen
;; ------------------------------------------------------------------------
;;   C-z        .. prefix
;;   C-z C-c    .. new
;;   C-z C-k    .. kill
;;   C-z C-n    .. goto next
;;   C-z C-p    .. goto back
;;   C-z C-a    .. goto just before
;;   C-z C-u    .. goto select (helm)
;;   C-z <num>  .. goto id
;;   C-z C-f    .. new (open file)
;;   C-z b      .. new (new buffer)
;;   C-z d      .. new (dired)
;; ========================================================================
(if (not (require 'elscreen nil t))
    (message "!!! WORNING !!! | require : elscreen")

  (setq elscreen-prefix-key (kbd "C-z"))
  (elscreen-start)
  ;; do not display [x]
  (setq elscreen-tab-display-kill-screen nil)
  ;; do not display [<->]
  (setq elscreen-tab-display-control nil)
  (when 'flag-helm-init
    (global-set-key (kbd "C-z C-u") 'helm-elscreen)))


(provide 'sub-tab-bar)
