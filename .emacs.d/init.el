(setq user-emacs-directory "~/.emacs.d/")

(add-to-list 'load-path (expand-file-name "init-lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))


;;----------------------------------------------------------------------------
;; Bootstrap config
;;----------------------------------------------------------------------------
(require 'init-boot)
(require 'init-keyboard)
(require 'init-keybind)
(require 'init-proxy)
(require 'init-pkg-mg)
(require 'init-windows)

;;----------------------------------------------------------------------------
;; Layout config
;;----------------------------------------------------------------------------
(require 'init-highlight)
(require 'init-frame)
(require 'init-window)
(require 'init-theme)
(require 'init-fonts)

;;----------------------------------------------------------------------------
;; Input confin
;;----------------------------------------------------------------------------
(require 'init-helm)
(require 'init-auto-complete)
(require 'init-select)
(require 'init-history)
(require 'init-csharp)

;;----------------------------------------------------------------------------
;; Text confin
;;----------------------------------------------------------------------------
(require 'init-encode)
(require 'init-indent)

;;----------------------------------------------------------------------------
;; Serce config
;;----------------------------------------------------------------------------
(require 'init-tags)
(require 'init-find)
(require 'init-grep)

;;----------------------------------------------------------------------------
;; History confin
;;----------------------------------------------------------------------------
(require 'init-vcs)
(require 'init-backup)

;;----------------------------------------------------------------------------
;; Other confin
;;----------------------------------------------------------------------------
(require 'init-org-mode)
(require 'init-insert)
(require 'init-markdown)


;;; TMP
;(require 'omnisharp)
;(setq omnisharp-server-executable-path
;      "/home/XXX/server/OmniSharpServer/OmniSharp/bin/Debug/OmniSharp.exe")

