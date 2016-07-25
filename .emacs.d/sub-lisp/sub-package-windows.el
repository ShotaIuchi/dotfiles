;; ========================================================================
;;  windows
;; ------------------------------------------------------------------------
;;  Run the following command because the error occurs at the time of the
;;  first start-up.
;;   > M-x package-install
;;   > async
;; ========================================================================
(when (os-type-windows)
  (if (not (require 'package nil t))
      (message "!!! WORNING !!! | require : package")

    ;; --------------------------------------------------------------------
    ;;  melpa - setting
    ;; --------------------------------------------------------------------
    ;; setting
    (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
    (add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

    (package-initialize)

    ;; auto-install
    (defvar installing-package-list
      '(
        async
        auto-complete
        cl-lib
        col-highlight
        color-moccur
        color-theme
        crosshairs
        cygwin-mount
        ddskk
        elscreen
        elscreen-persist
        expand-region
        ggtags
        git-gutter
        google-translate
        gradle-mode
        helm
        helm-ag
        helm-c-moccur
        helm-gtags
        helm-swoop
        highlight-symbol
        hl-line+
        markdown-mode
        migemo
        multi-term
        multiple-cursors
        popup
        popwin
        rainbow-delimiters
        redo+
        smartrep
        smex
        tangotango-theme
        web-mode
        wgrep
        windata
        ))

    (defvar installing-package-list-24_3 '())
    (defvar installing-package-list-24_4 '())
    (cond ((string-match "24.3." emacs-version)
           (defconst installing-package-list-24_3 '()))
          (t
           (defconst installing-package-list-24_4
             '(
               helm-ls-git
               magit
               )
             )))
    (defconst installing-package-list
      (append
       installing-package-list
       installing-package-list-24_3
       installing-package-list-24_4
       ))

    (dolist (package installing-package-list)
      (when (or (not (package-installed-p package)))
        (package-install package)))
    )
  )


(provide 'sub-package-windows)
