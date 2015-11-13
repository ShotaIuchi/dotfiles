(require 'highlight-symbol)
(require 'rainbow-delimiters)
;(require 'auto-highlight-symbol nil t)

(global-set-key [(control f3)] 'highlight-symbol-at-point)
(global-set-key [f3] 'highlight-symbol-next)
(global-set-key [(shift f3)] 'highlight-symbol-prev)
;(global-set-key [(meta f3)] 'highlight-symbol-query-replace)
;(global-auto-highlight-symbol-mode t)

;; line
(global-hl-line-mode t)

;; delimiters
(add-hook 'java-mode-hook 'rainbow-delimiters-mode)
(add-hook 'c-mode-hook 'rainbow-delimiters-mode)
(add-hook 'c++-mode-hook 'rainbow-delimiters-mode)


(provide 'sub-highlight)

