(require 'auto-complete-config)

;(global-auto-complete-mode t)
(ac-config-default)
(define-key ac-completing-map (kbd "TAB") 'auto-complete)
;(define-key ac-completing-map (kbd "C-n") 'ac-next)
;(define-key ac-completing-map (kbd "C-p") 'ac-previous)

(provide 'sub-auto-complete)

