;; ========================================================================
;;  hook
;; ========================================================================
(if (not (require 'web-mode nil t))
    (message "!!! WORNING !!! | require : web-mode")

  ;; under Emacs23
  (when (< emacs-major-version 24)
    (defalias 'prog-mode 'fundamental-mode))

  ;; target filename extention
  (add-to-list 'auto-mode-alist '("\\.phtml$"     . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsp$"       . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x$"   . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb$"       . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?$"     . web-mode))
  (add-to-list 'auto-mode-alist '("\\.hta?$"      . web-mode))

  ;; indent
  (defun web-mode-hook ()
    "Hooks for Web mode."
    (hook-common)
    (setq web-mode-html-offset   4)
    (setq web-mode-css-offset    4)
    (setq web-mode-script-offset 4)
    (setq web-mode-php-offset    4)
    (setq web-mode-java-offset   4)
    (setq web-mode-asp-offset    4))
  (add-hook 'web-mode-hook 'web-mode-hook)
  )


(provide 'sub-filetype-html)
