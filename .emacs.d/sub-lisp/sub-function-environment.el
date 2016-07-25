;; ========================================================================
;;  os
;; ========================================================================
(defun os-type-windows ()
  "Whether the OS windows."
  (or (eq system-type 'windows-nt)
      (eq system-type 'cygwin)))

(defun os-type-mac ()
  "Whether the OS mac."
  (eq system-type 'darwin))

(defun os-type-linux ()
  "Whether the OS linux."
  (eq system-type 'gnu/linux))


;; ========================================================================
;;  UI (CUI?/GUI?)
;; ========================================================================
(defun ui-type-gui ()
  "Whether the ui gui."
  (window-system))

(defun ui-type-cui ()
  "Whether the ui cui."
  (not (window-system)))

;; (defun gui-type-windows ()
;;   "Whether the gui windows."
;;   (or (eq window-system 'w32)
;;       (eq window-system 'pc)))

;; (defun gui-type-mac ()
;;   "Whether the gui mac."
;;   (eq window-system 'ns))

;; (defun gui-type-linux ()
;;   "Whether the gui linux."
;;   (eq window-system 'x))


(provide 'sub-function-environment)
