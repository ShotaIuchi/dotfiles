;; ========================================================================
;;  GNU Global
;; ========================================================================
(if (not (require 'gtags nil t))
    (message "!!! WORNING !!! | require : gtags")

  ;; hook
  (cond ((require 'helm-gtags nil t)
         (setq helm-gtags-auto-update t)
         (setq gtags-mode-hook
               '(lambda ()
                  (local-set-key (kbd "M-t") 'helm-gtags-find-tag-from-here)
                  (local-set-key (kbd "M-r") 'helm-gtags-find-rtag)
                  (local-set-key (kbd "M-s") 'helm-gtags-find-symbol)
                  (local-set-key (kbd "M-p") 'helm-gtags-find-pattern)
                  (local-set-key (kbd "M-l") 'helm-gtags-select-path)
                  (local-set-key (kbd "M-a") 'helm-gtags-find-files)
                  (local-set-key (kbd "C-t") 'helm-gtags-pop-stack)
                  )
               ))
        (if (not (require 'ggtags nil t))
            (message "!!! WORNING !!! | require : ggtags")
          (setq gtags-mode-hook
                '(lambda ()
                   (local-set-key (kbd "M-t") 'ggtags-find-tag-dwim)
                   (local-set-key (kbd "M-r") 'ggtags-find-reference)
                   (local-set-key (kbd "M-s") 'ggtags-find-other-symbol)
                   (local-set-key (kbd "M-p") 'gtags-find-pattern)
                   (local-set-key (kbd "M-l") 'gtags-select-tag)
                   (local-set-key (kbd "M-a") 'ggtags-find-file)
                   (local-set-key (kbd "C-t") 'ggtags-prev-mark)
                   )))
        )
  (add-hook 'c-mode-hook (lambda () (gtags-mode 1)))
  (add-hook 'c++-mode-hook (lambda () (gtags-mode 1)))
  (add-hook 'java-mode-hook (lambda () (gtags-mode 1)))
  )


(provide 'sub-search-tags)
