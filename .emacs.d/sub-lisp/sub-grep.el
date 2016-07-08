;; ========================================================================
;;  grep
;; ========================================================================
(unless (require 'wgrep nil t)
  (message "!!! WORNING !!! | require : wgrep"))

(setq null-device "/dev/null")
(global-set-key (kbd "C-M-g") 'rgrep)


;; ========================================================================
;;  tags
;; ========================================================================
(if (not (require 'gtags nil t))
    (message "!!! WORNING !!! | require : gtags")
  (setq gtags-mode-hook
        '(lambda ()
           (local-set-key "\M-t" 'gtags-find-tag)
           (local-set-key "\M-r" 'gtags-find-rtag)
           (local-set-key "\M-s" 'gtags-find-symbol)
           (local-set-key "\M-p" 'gtags-find-pattern)
           (local-set-key "\M-a" 'gtags-find-file)
           (local-set-key "\C-t" 'gtags-pop-stack)
           ))
  (add-hook 'c-mode-hook (lambda () (gtags-mode 1)))
  (add-hook 'c++-mode-hook (lambda () (gtags-mode 1)))
  (add-hook 'java-mode-hook (lambda () (gtags-mode 1)))
  )


(provide 'sub-grep)
