(require 'helm-config)
(require 'helm-ag)
;(require 'helm-gtags)
(require 'helm-ls-git)
(require 'helm-c-moccur)
;(require 'helm-descbinds)

(global-unset-key (kbd "C-u"))
(global-set-key (kbd "C-u C-u") 'helm-mini)
(global-set-key (kbd "C-u C-i") 'helm-imenu)
(global-set-key (kbd "C-u C-o") 'helm-occur)
(global-set-key (kbd "C-u C-g") 'helm-ag)
(global-set-key (kbd "C-u C-y") 'helm-show-kill-ring)

;; gtags
;(global-set-key (kbd "C-c p") 'helm-gtags-find-pattern)
;(global-set-key (kbd "C-c t") 'helm-gtags-find-tag)
;(global-set-key (kbd "C-c r") 'helm-gtags-find-rtag)
;(global-set-key (kbd "C-c a") 'helm-gtags-find-files)
;(global-set-key (kbd "C-c s") 'helm-gtags-find-symbol)

;; find
(global-set-key (kbd "M-o") 'helm-c-moccur-occur-by-moccur)
(setq helm-c-moccur-enable-initial-pattern t)
(setq helm-c-moccur-enable-auto-look-flag t)

(provide 'sub-anything)
