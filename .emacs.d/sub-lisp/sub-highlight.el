;;----------------------------------------------------------------------------
;; line (center)
;;----------------------------------------------------------------------------
(if (not (require 'hl-line nil t))
    (message "!!! WORNING !!! | require : hl-line")
  ;; mode:on
  (global-hl-line-mode t)
  ;; hook:remove
  (remove-hook 'post-command-hook #'global-hl-line-highlight)
  (remove-hook 'change-major-mode-hook #'global-hl-line-unhighlight)
  (remove-hook 'post-command-hook #'global-hl-line-maybe-unhighlight)
  ;; keybind
  (global-set-key (kbd "C-c C-;") 'global-hl-line-center)
  (defun global-hl-line-center ()
    (interactive)
    (global-hl-line-highlight)
    (recenter-top-bottom)))

;;----------------------------------------------------------------------------
;; symble
;;----------------------------------------------------------------------------
(if (not (require 'highlight-symbol nil t))
    (message "!!! WORNING !!! | require : highlight-symbol")
  ;;(require 'auto-highlight-symbol nil t)
  ;;(global-set-key [(meta f3)] 'highlight-symbol-query-replace)
  ;;(global-auto-highlight-symbol-mode t)
  (global-set-key [(control f3)] 'highlight-symbol-at-point)
  (global-set-key [f3] 'highlight-symbol-next)
  (global-set-key [(shift f3)] 'highlight-symbol-prev)
  )

;;----------------------------------------------------------------------------
;; Emphasis of parentheses
;;----------------------------------------------------------------------------
(if (not (and
          (require 'rainbow-delimiters nil t)
          (require 'cl-lib nil t)
          (require 'color nil t)
          ))
    ((unless (require 'rainbow-delimiters nil t)
       (message "!!! WORNING !!! | require : rainbow-delimiters"))
     (unless (require 'cl-lib nil t)
       (message "!!! WORNING !!! | require : cl-lib"))
     (unless (require 'color nil t)
       (message "!!! WORNING !!! | require : color")))
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  (defun rainbow-delimiters-using-stronger-colors ()
    (interactive)
    (cl-loop
     for index from 1 to rainbow-delimiters-max-face-count
     do
     (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
       (cl-callf color-saturate-name (face-foreground face) 30))))
  (add-hook 'emacs-startup-hook 'rainbow-delimiters-using-stronger-colors))


(provide 'sub-highlight)
