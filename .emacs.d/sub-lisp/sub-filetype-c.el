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


(provide 'sub-filetype-c)
