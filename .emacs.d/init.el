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
;;   > (setq url-proxy-services
;;   >     '(("http" . "hoge:xxxx")
;;   >       ("https" . "hoge:xxxx")))
;; ========================================================================
(when (file-exists-p "~/.emacs.local-start.el")
  (load "~/.emacs.local-start.el"))


;; ========================================================================
;;  character
;; ========================================================================
(require 'sub-char-code)
(require 'sub-fonts)


;; ========================================================================
;; Bootstrap config
;; ========================================================================
(require 'sub-keyboard)
(require 'sub-keybind)
(require 'sub-package)
;;(require 'sub-pkg-mg)
(require 'sub-enviorment)


;; ========================================================================
;; Layout config
;; ========================================================================
(require 'sub-highlight)
(require 'sub-window)
(require 'sub-theme)


;; ========================================================================
;; Search config
;; ========================================================================
(require 'sub-grep)


;; ========================================================================
;; Input confin
;; ========================================================================
(require 'sub-ime)
(require 'sub-interface)
(require 'sub-auto-complete)
(require 'sub-select)
(require 'sub-history)


;; ========================================================================
;; Text confin
;; ========================================================================
(require 'sub-indent)


;; ========================================================================
;; History confin
;; ========================================================================
(require 'sub-git)
(require 'sub-backup)


;; ========================================================================
;; Other confin
;; ========================================================================
(require 'sub-terminal)
(require 'sub-language)
(require 'sub-org-mode)
(require 'sub-insert)
(require 'sub-markdown)
(require 'sub-directory)
(require 'sub-web)


;; ========================================================================
;; Local setting (end)
;; ========================================================================
(when (file-exists-p "~/.emacs.local-end.el")
  (load "~/.emacs.local-end.el"))
