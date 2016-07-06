(require 'package)
(require 'cl)

;;----------------------------------------------------------------------------
;; melpa - setting
;;----------------------------------------------------------------------------
;; setting
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
;(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
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
    helm-ls-git
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
    magit
    git-gutter
   ))

(let ((not-installed (loop for x in installing-package-list
                           when (not (package-installed-p x))
                           collect x)))
  (when not-installed
    (package-refresh-contents)
    (dolist (pkg not-installed)
      (package-install pkg))))



(provide 'sub-pkg-mg)
