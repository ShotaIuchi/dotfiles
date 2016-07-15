;; ========================================================================
;;  emacs
;; ========================================================================
(defun emacs-version-eq (major minor)
  "Whether major and minor are eq."
  (and (= major emacs-major-version)
       (= minor emacs-minor-version)))

(defun emacs-version-new (major minor)
  "Whether major and minor are new."
  (and (<= major emacs-major-version)
       (<= minor emacs-minor-version)))

(defun emacs-version-old (major minor)
  "Whether major and minor are old."
  (and (>= major emacs-major-version)
       (>= minor emacs-minor-version)))
