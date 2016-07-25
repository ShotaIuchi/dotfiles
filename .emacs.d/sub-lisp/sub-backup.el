;; ========================================================================
;;  backup
;; ========================================================================
(setq make-backup-files t)
(setq backup-directory-alist
      (cons (cons "\\.*$" (expand-file-name "~/.emacs.d/backup"))
            backup-directory-alist))


;; ========================================================================
;;  auto-save
;; ========================================================================
(setq auto-save-default t)
(setq auto-save-list-file-prefix t)
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "~/.emacs.d/backup/") t)))



(provide 'sub-backup)
