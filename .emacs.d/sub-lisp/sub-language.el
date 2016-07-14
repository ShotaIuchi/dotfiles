;; ========================================================================
;;  common
;; ========================================================================
(defun hook-common ()
  (hs-minor-mode 1)
  (local-set-key (kbd "C-u C-c") 'hs-hide-level)
  (local-set-key (kbd "C-u C-t") 'hs-toggle-hiding)
  )

;; ========================================================================
;;  C,C++
;; ========================================================================
(defun c-mode-hook-impl ()
  (hook-common)
  (c-set-style "linux")
  (c-set-offset 'innamespace 0)
  (setq c-basic-offset 4)
  (setq c-tab-always-indent nil)
  (setq c-hungry-delete-key t)
  (setq indent-tabs-mode nil))
(add-hook 'c-mode-hook 'c-mode-hook-impl)
(add-hook 'c++-mode-hook 'c-mode-hook-impl)

;; ========================================================================
;;  C#
;; ========================================================================
(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(setq auto-mode-alist
      (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))

;; ========================================================================
;;  Java
;; ========================================================================
(defun java-mode-hook-impl ()
  (hook-common)
  (c-mode-hook-impl))
(add-hook 'java-mode-hook 'java-mode-hook-impl)

;; ========================================================================
;;  Build
;; ========================================================================
(if (not (require 'gradle-mode nil t))
    (message "!!! WORNING !!! | require : gradle-mode")
  (gradle-mode 1))

;; ========================================================================
;;  Web
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


(provide 'sub-language)
