;; ========================================================================
;;  path
;; ========================================================================
(defvar path-backup-dir
  (concat
   (expand-file-name user-emacs-directory) "backup/"))


;; ========================================================================
;;  backup
;; ========================================================================
(setq make-backup-files t)
(setq backup-directory-alist '(("" . path-backup-dir)))


;; ========================================================================
;;  auto-save
;; ========================================================================
(setq auto-save-default t)
(setq auto-save-list-file-prefix t)
(setq auto-save-file-name-transforms
      `((".*", (expand-file-name path-backup-dir) t)))


(provide 'sub-backup)
