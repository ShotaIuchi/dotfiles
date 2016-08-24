;; ========================================================================
;;  line (center)
;; -----------------------------------------------------------------------
;;   C-c C-;    to center and highlight line
;; ========================================================================
(cond ((not (require 'col-highlight nil t))
       (message "!!! WORNING !!! | require : col-highlight"))
      ((not (require 'crosshairs nil t))
       (message "!!! WORNING !!! | require : col-highlight"))
      (t
       (global-set-key (kbd "C-c C-;") 'global-hl-line-center)
       (defun global-hl-line-center ()
         (interactive)
         (recenter-top-bottom)
         (crosshairs-highlight))))


;; ========================================================================
;;  highlight all
;; -----------------------------------------------------------------------
;;   C-F3       highlight (toggle)
;;   F3         next
;;   S-F3       prev
;; ========================================================================
(if (not (require 'highlight-symbol nil t))
    (message "!!! WORNING !!! | require : highlight-symbol")
  ;;(require 'auto-highlight-symbol nil t)
  ;;(global-set-key [(meta f3)] 'highlight-symbol-query-replace)
  ;;(global-auto-highlight-symbol-mode t)
  (global-set-key [(control f3)] 'highlight-symbol-at-point)
  (global-set-key [f3] 'highlight-symbol-next)
  (global-set-key [(shift f3)] 'highlight-symbol-prev)
  )


;; ========================================================================
;;  emphasis of parentheses
;; ========================================================================
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


;; ========================================================================
;;  whitespace
;;   see : http://qiita.com/itiut@github/items/4d74da2412a29ef59c3a
;; ========================================================================
(if (not (require 'whitespace nil t))
    (message "!!! WORNING !!! | require : whitespace")
  (setq whitespace-style '(face
                           trailing
                           tabs
                           spaces
                           empty
                           space-mark
                           tab-mark
                           ))
  (setq whitespace-display-mappings
        '((space-mark ?\u3000 [?\u25a1])
          ;; WARNING: the mapping below has a problem.
          ;; When a TAB occupies exactly one column, it will display the
          ;; character ?\xBB at that column followed by a TAB which goes to
          ;; the next TAB column.
          ;; If this is a problem for you, please, comment the line below.
          (tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])))
  (setq whitespace-space-regexp "\\(\u3000+\\)")
  (global-whitespace-mode t)
  ;; (defvar my/bg-color "#232323")
  ;; (set-face-attribute 'whitespace-trailing nil
  ;;                     :background my/bg-color
  ;;                     :foreground "DeepPink"
  ;;                     :underline t)
  ;; (set-face-attribute 'whitespace-tab nil
  ;;                     :background my/bg-color
  ;;                     :foreground "LightSkyBlue"
  ;;                     :underline t)
  ;; (set-face-attribute 'whitespace-space nil
  ;;                     :background my/bg-color
  ;;                     :foreground "GreenYellow"
  ;;                     :weight 'bold)
  ;; (set-face-attribute 'whitespace-empty nil
  ;;                     :background my/bg-color)
  )


(provide 'sub-highlight)
