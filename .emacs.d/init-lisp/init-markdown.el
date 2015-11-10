;;----------------------------------------------------------------------------
;; マークダウン記法
;;----------------------------------------------------------------------------
(require 'markdown-mode)

(setq markdown-command "c:/cygwin/bin/kramdown")
;(setq markdown-command "C:/cygwin/bin/kramdown --auto-ids")
;(setenv "LC_ALL" "C")

(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(provide 'init-markdown)
