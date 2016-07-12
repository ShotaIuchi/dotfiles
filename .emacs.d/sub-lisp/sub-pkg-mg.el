(if (not (and
          (require 'package nil t)
          (require 'cl nil t)
          ))
    ((unless (require 'package nil t)
       (message "!!! WORNING !!! | require : package"))
     (unless (require 'cl nil t)
       (message "!!! WORNING !!! | require : cl")))

  ;;----------------------------------------------------------------------------
  ;; melpa - setting
  ;;----------------------------------------------------------------------------
  ;; setting
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
  ;;(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

  (package-initialize)

  ;; auto-install
  (defvar installing-package-list
    '(
      async
      auto-complete
      color-moccur
      color-theme
      cygwin-mount
      expand-region
      helm
      helm-ag
      helm-c-moccur
      helm-gtags
      helm-swoop
      highlight-symbol
      multiple-cursors
      popup
      popwin
      rainbow-delimiters
      redo+
      smartrep
      tangotango-theme
      markdown-mode
      wgrep
      ddskk
      cl-lib
      smex
      web-mode
      gradle-mode
      git-gutter
      migemo
      ggtags
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

  (let ((not-installed (loop for x in installing-package-list
                             when (not (package-installed-p x))
                             collect x)))
    (when not-installed
      (package-refresh-contents)
      (dolist (pkg not-installed)
        (package-install pkg))))
  )


(provide 'sub-pkg-mg)
