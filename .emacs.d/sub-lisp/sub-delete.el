;; ========================================================================
;;  delete region
;; ========================================================================
(delete-selection-mode t)


;; ========================================================================
;;  ci
;; ------------------------------------------------------------------------
;;  https://github.com/cs14095/ci.el
;;   Ctrl-c, i, w => kill a word
;;   Ctrl-c, i, t => kill inside <>
;;   Ctrl-c, i, ' => kill inside ''
;;   Ctrl-c, i, " => kill inside ""
;;   Ctrl-c, i, ( => kill inside ()
;;   Ctrl-c, i, { => kill inside {}
;; ========================================================================
(unless (require 'ci nil t)
  (message "!!! WORNING !!! | require : ci"))


(provide 'sub-delete)
