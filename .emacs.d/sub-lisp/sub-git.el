;; ========================================================================
;;  helm-ls-git
;; ------------------------------------------------------------------------
;;   C-u C-l    .. ls
;; ========================================================================
(when 'flag-helm-init
  (if (not (require 'helm-ls-git nil t))
      (message "!!! WORNING !!! | require : helm-ls-git")
    (global-set-key (kbd "C-u C-l") 'helm-ls-git-ls)))


;; ========================================================================
;;  magit
;; ------------------------------------------------------------------------
;;   C-u C-s    .. status
;; ========================================================================
(if (not (require 'magit nil t))
    (message "!!! WORNING !!! | require : magit")
  (global-set-key (kbd "C-c C-s") 'magit-status)
  ;;(if (eq system-type 'windows-nt)
  ;;    (setq magit-git-executable "C:/cygwin/bin/git.exe"))
  ;;(set-face-foreground 'magit-diff-add "#00000000")
  ;;(set-face-foreground 'magit-diff-del "#00000000")
  ;;(set-face-background 'magit-item-highlight "#00000000")
  ;;(set-face-foreground 'magit-diff-add "green3")
  ;;(set-face-foreground 'magit-diff-del "red3")
  )


;; ========================================================================
;;  gutter
;; ------------------------------------------------------------------------
;;   C-x C-g    .. toggle
;; ========================================================================
(if (not (require 'git-gutter nil t))
    (message "!!! WORNING !!! | require : git-gutter")
  ;;(global-git-gutter-mode t)
  (global-set-key (kbd "C-x C-g") 'git-gutter:toggle))


(provide 'sub-git)
