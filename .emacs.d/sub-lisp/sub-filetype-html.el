;; ========================================================================
;;  hook
;; ========================================================================
(if (not (require 'web-mode nil t))
    (message "!!! WORNING !!! | require : web-mode")

  ;; under Emacs23
  (when (< emacs-major-version 24)
    (defalias 'prog-mode 'fundamental-mode))

  ;; target filename extention
  (add-to-list 'auto-mode-alist '("\\.phtml\\'"     . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'"   . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'"   . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'"       . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'"  . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'"    . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'"     . web-mode))

  ;; indent
  (setq web-mode-asp-offset           2)
  (setq web-mode-block-padding        2)
  (setq web-mode-code-indent-offset   2)
  (setq web-mode-comment-style        2)
  (setq web-mode-css-indent-offset    2)
  (setq web-mode-css-offset           2)
  (setq web-mode-html-offset          2)
  (setq web-mode-java-offset          2)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-php-offset           2)
  (setq web-mode-script-offset        2)
  (setq web-mode-script-padding       2)
  (setq web-mode-style-padding        2)
  )

(provide 'sub-filetype-html)
