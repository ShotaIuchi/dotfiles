;; ========================================================================
;;  web
;; ========================================================================
(if (not (require 'eww nil t))
    (message "!!! WORNING !!! | require : eww")

  ;; engine
  (setq eww-search-prefix "http://www.google.co.jp/search?q=")

  ;; keybind
  (global-set-key (kbd "C-c C-w") 'eww)
  (define-key eww-mode-map "r" 'eww-reload)
  (define-key eww-mode-map "c 0" 'eww-copy-page-url)
  (define-key eww-mode-map "p" 'scroll-down)
  (define-key eww-mode-map "n" 'scroll-up)

  ;; color
  (defvar eww-disable-colorize t)
  (defun shr-colorize-region--disable (orig start end fg &optional bg &rest _)
    (unless eww-disable-colorize
      (funcall orig start end fg)))
  (advice-add 'shr-colorize-region :around 'shr-colorize-region--disable)
  (advice-add 'eww-colorize-region :around 'shr-colorize-region--disable)
  (defun eww-disable-color ()
    "It does not reflect the character color in eww."
    (interactive)
    (setq-local eww-disable-colorize t)
    (eww-reload))
  (defun eww-enable-color ()
    "To reflect the character color in eww."
    (interactive)
    (setq-local eww-disable-colorize nil)
    (eww-reload))

  ;; image
  (defun eww-disable-images ()
    "Do not display images in eww."
    (interactive)
    (setq-local shr-put-image-function 'shr-put-image-alt)
    (eww-reload))
  (defun eww-enable-images ()
    "To display images in eww."
    (interactive)
    (setq-local shr-put-image-function 'shr-put-image)
    (eww-reload))
  (defun shr-put-image-alt (spec alt &optional flags)
    (insert alt))
  ;; defualt : disable
  (defun eww-mode-hook--disable-image ()
    (setq-local shr-put-image-function 'shr-put-image-alt))
  (add-hook 'eww-mode-hook 'eww-mode-hook--disable-image))


;; ========================================================================
;;  google translate
;; ========================================================================
(if (not (require 'google-translate nil t))
    (message "!!! WORNING !!! | require : google-translate")
  (global-set-key (kbd "C-c C-t") 'google-translate-at-point)
  (global-set-key (kbd "C-c C-e") 'google-translate-query-translate)
  (global-set-key (kbd "C-c C-j") 'google-translate-query-translate-reverse)
  (custom-set-variables
   '(google-translate-default-source-language "en")
   '(google-translate-default-target-language "ja")))


(provide 'sub-web)
