(require 'gtags)

(setq gtags-mode-hook
      '(lambda ()
         (local-set-key "\M-t" 'gtags-find-tag)
         (local-set-key "\M-r" 'gtags-find-rtag)
         (local-set-key "\M-s" 'gtags-find-symbol)
         (local-set-key "\M-p" 'gtags-find-pattern)
         (local-set-key "\M-a" 'gtags-find-file)
         (local-set-key "\C-t" 'gtags-pop-stack)
         ))

(add-hook 'java-mode-hook (lambda () (gtags-mode t)))
(add-hook 'c-mode-hook (lambda () (gtags-mode t)))
(add-hook 'c++-mode-hook (lambda () (gtags-mode t)))

(provide 'init-tags)

