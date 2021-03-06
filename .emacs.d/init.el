;; ========================================================================
;;  load directory
;; ========================================================================
(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

(add-to-list 'load-path (locate-user-emacs-file "sub-lisp"))
(add-to-list 'load-path (locate-user-emacs-file "site-lisp"))


;; ========================================================================
;;  Local setting (start)
;; ------------------------------------------------------------------------
;;   (setq url-proxy-services
;;       '(("http" . "hoge:xxxx")
;;         ("https" . "hoge:xxxx")))
;; ========================================================================
(when (file-exists-p "~/.emacs.local-start.el")
  (load "~/.emacs.local-start.el"))


;; ========================================================================
;;  function
;; ========================================================================
(require 'sub-function-init)
(require 'sub-function-version)
(require 'sub-function-environment)
(require 'sub-function-date)


;; ========================================================================
;;  enviorment
;; ========================================================================
(require 'sub-key-swap)
(require 'sub-gc)
(require 'sub-appearance)
(require 'sub-sound)


;; ========================================================================
;;  character
;; ========================================================================
(require 'sub-char-code)
(require 'sub-fonts)


;; ========================================================================
;;  interface
;; ========================================================================
(require 'sub-mouse)
(require 'sub-keybind)


;; ========================================================================
;;  package
;; ========================================================================
(require 'sub-package)
(require 'sub-package-windows)
(require 'sub-helm-init)


;; ========================================================================
;;  os
;; ========================================================================
(require 'sub-os-windows)


;; ========================================================================
;;  directory
;; ========================================================================
(require 'sub-directory)


;; ========================================================================
;;  buffer
;; ========================================================================
(require 'sub-M-x)
(require 'sub-buffer)
(require 'sub-buffer-list)
(require 'sub-open-file)
(require 'sub-recent)


;; ========================================================================
;;  shortcut
;; ========================================================================
(require 'sub-alias)
(require 'sub-cursor)


;; ========================================================================
;;  range
;; ========================================================================
(require 'sub-region)
(require 'sub-delete)
(require 'sub-rectangle)


;; ========================================================================
;;  layout
;; ========================================================================
(require 'sub-window)
(require 'sub-tab-bar)
(require 'sub-theme)
(require 'sub-highlight)


;; ========================================================================
;;  search
;; ========================================================================
(require 'sub-search)
(require 'sub-search-buffer-current)
(require 'sub-search-buffer-all)
(require 'sub-search-grep)
(require 'sub-search-tags)


;; ========================================================================
;;  input
;; ========================================================================
(require 'sub-ime)
(require 'sub-auto-complete)
(require 'sub-history)


;; ========================================================================
;;  tool
;; ========================================================================
(require 'sub-terminal)
(require 'sub-web)


;; ========================================================================
;;  history
;; ========================================================================
(require 'sub-git)
(require 'sub-backup)
(require 'sub-kill-ring)


;; ========================================================================
;;  filetype
;; ========================================================================
(require 'sub-yasnippet)
(require 'sub-syntax-check)
(require 'sub-filetype-common)
(require 'sub-filetype-c)
(require 'sub-filetype-build)
(require 'sub-filetype-html)
(require 'sub-filetype-text)
(require 'sub-filetype-org)


;; ========================================================================
;; Local setting (end)
;; ========================================================================
(when (file-exists-p "~/.emacs.local-end.el")
  (load "~/.emacs.local-end.el"))
