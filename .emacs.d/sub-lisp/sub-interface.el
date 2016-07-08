(setq helm-idle-delay 0.2)

(require 'helm-config)
(require 'helm-ag)
;(require 'helm-gtags)
(require 'helm-ls-git)
(require 'helm-c-moccur)
;(require 'helm-descbinds)


;;(setq helm-exit-idle-delay nil)

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

;; M-x
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)


(provide 'sub-interface)
