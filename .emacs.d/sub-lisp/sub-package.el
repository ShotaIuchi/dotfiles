;; ========================================================================
;;  setting PM
;; ========================================================================
;; ------------------------------------------------------------------------
;;  el-get
;; ------------------------------------------------------------------------
(add-to-list 'load-path (locate-user-emacs-file "el-get/el-get"))
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

;; ------------------------------------------------------------------------
;;  package
;; ------------------------------------------------------------------------
(require 'package nil t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
(package-initialize)
(package-refresh-contents)


;; ========================================================================
;; preinstall packages (all)
;; ========================================================================
(el-get-bundle tarao/el-get-lock)
(el-get-lock)
(el-get-lock-unlock)


;; ========================================================================
;; install packages (all)
;; ========================================================================
(el-get-bundle async)
(el-get-bundle auto-complete)
(el-get-bundle color-moccur)
(el-get-bundle color-theme)
(el-get-bundle expand-region)
(el-get-bundle helm)
(el-get-bundle helm-ag)
(el-get-bundle helm-c-moccur)
(el-get-bundle helm-gtags)
(el-get-bundle helm-swoop)
(el-get-bundle highlight-symbol)
(el-get-bundle multiple-cursors)
(el-get-bundle popup)
(el-get-bundle popwin)
(el-get-bundle windata)
(el-get-bundle rainbow-delimiters)
(el-get-bundle redo+)
(el-get-bundle smartrep)
(el-get-bundle markdown-mode)
(el-get-bundle wgrep)
(el-get-bundle ddskk)
(el-get-bundle cl-lib)
(el-get-bundle smex)
(el-get-bundle web-mode)
(el-get-bundle gradle-mode)
(el-get-bundle git-gutter)
(el-get-bundle migemo)
(el-get-bundle ggtags)
(el-get-bundle google-translate)
(el-get-bundle multi-term)
(el-get-bundle tangotango-theme)
(el-get-bundle helm-ls-git)
(el-get-bundle elscreen)
(el-get-bundle elscreen-persist)
(el-get-bundle hl-line+)
(el-get-bundle col-highlight)
(el-get-bundle crosshairs)


;; ========================================================================
;;  install packages (>=24.3.)
;; ========================================================================
(when (emacs-version-old 24 3)
  )


;; ========================================================================
;;  install packages (<=24.4.)
;; ========================================================================
(when (emacs-version-new 24 4)
  (el-get-bundle magit/magit)
  )


;; ========================================================================
;;  install packages (window)
;; ========================================================================
(when (eq system-type 'windows-nt)
  (el-get-bundle cygwin-mount)
  )


;; ========================================================================
;;  install packages (mac)
;; ========================================================================
(when (eq system-type 'darwin)
  )


;; ========================================================================
;;  install packages (linux)
;; ========================================================================
(when (eq system-type 'gnu/linux)
  )


(provide 'sub-package)
