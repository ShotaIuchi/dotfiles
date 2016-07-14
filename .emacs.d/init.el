(setq user-emacs-directory "~/.emacs.d/")

(add-to-list 'load-path (expand-file-name "sub-lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))


;;----------------------------------------------------------------------------
;; Local setting (start)
;;
;; ------
;;  (setq url-proxy-services
;;      '(("http" . "hoge:xxxx")
;;        ("https" . "hoge:xxxx")))
;;----------------------------------------------------------------------------
(when (file-exists-p "~/.emacs.local-start.el")
  (load "~/.emacs.local-start.el"))


;;----------------------------------------------------------------------------
;; Bootstrap config
;;----------------------------------------------------------------------------
(require 'sub-keyboard)
(require 'sub-keybind)
(require 'sub-pkg-mg)
(require 'sub-enviorment)

;;----------------------------------------------------------------------------
;; Layout config
;;----------------------------------------------------------------------------
(require 'sub-highlight)
(require 'sub-window)
(require 'sub-theme)
(require 'sub-fonts)

;;----------------------------------------------------------------------------
;; Search config
;;----------------------------------------------------------------------------
(require 'sub-grep)

;;----------------------------------------------------------------------------
;; Input confin
;;----------------------------------------------------------------------------
(require 'sub-ime)
(require 'sub-interface)
(require 'sub-auto-complete)
(require 'sub-select)
(require 'sub-history)

;;----------------------------------------------------------------------------
;; Text confin
;;----------------------------------------------------------------------------
(require 'sub-indent)

;;----------------------------------------------------------------------------
;; History confin
;;----------------------------------------------------------------------------
(require 'sub-git)
(require 'sub-backup)

;;----------------------------------------------------------------------------
;; Other confin
;;----------------------------------------------------------------------------
(require 'sub-terminal)
(require 'sub-language)
(require 'sub-org-mode)
(require 'sub-insert)
(require 'sub-markdown)
(require 'sub-directory)
(require 'sub-web)

;;----------------------------------------------------------------------------
;; Local setting (end)
;;----------------------------------------------------------------------------
(when (file-exists-p "~/.emacs.local-end.el")
  (load "~/.emacs.local-end.el"))
