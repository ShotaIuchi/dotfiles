;;----------------------------------------------------------------------------
;; マークダウン記法
;;----------------------------------------------------------------------------
(if (not (require 'markdown-mode))
    (message "!!! WORNING !!! | require : markdown-mode")

  ;;(setq markdown-command "C:/cygwin/bin/kramdown --auto-ids")
  ;;(setenv "LC_ALL" "C")
  (setq markdown-command "c:/cygwin/bin/kramdown")

  (add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
  )


(provide 'sub-markdown)
