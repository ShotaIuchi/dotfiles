;; ========================================================================
;; helm
;; ========================================================================
(if (not (require 'helm-config nil t))
    (message "!!! WORNING !!! | require : helm-config")

  (setq helm-idle-delay 0.2)

  (global-unset-key (kbd "C-u"))
  (global-set-key (kbd "C-u C-u") 'helm-mini)
  (global-set-key (kbd "C-u C-i") 'helm-imenu)
  (global-set-key (kbd "C-u C-y") 'helm-show-kill-ring)
  (global-set-key (kbd "C-u C-x") 'helm-M-x)
  (global-set-key (kbd "C-u C-o") 'helm-occur)

  ;;(require 'helm-c-moccur)
  ;;(require 'helm-descbinds)

  ;; ag
  (if (not (require 'helm-ag nil t))
      (message "!!! WORNING !!! | require : helm-ag")
    (global-set-key (kbd "C-u C-g") 'helm-ag))

  ;; swoop
  (if (not (require 'helm-swoop nil t))
      (message "!!! WORNING !!! | require : helm-swoop")
    (when (require 'migemo nil t)
      (helm-migemo-mode 1))
    (setq helm-multi-swoop-edit-save t)
    (setq helm-swoop-split-with-multiple-windows nil)
    (setq helm-swoop-split-direction 'split-window-vertically)
    (setq helm-swoop-speed-or-color nil)
    (setq helm-swoop-move-to-line-cycle t)
    (setq helm-swoop-use-line-number-face t)
    (setq helm-swoop-use-fuzzy-match t)
    (global-set-key (kbd "C-u C-s") 'helm-swoop)
    (global-set-key (kbd "C-u C-a") 'helm-multi-swoop-all))

  ;; ls-git
  (if (not (require 'helm-ls-git nil t))
      (message "!!! WORNING !!! | require : helm-ls-git")
    (global-set-key (kbd "C-u C-l") 'helm-ls-git-ls)))


;; ========================================================================
;; smex
;;  > M-x
;; ========================================================================
(if (not (require 'smex nil t))
    (message "!!! WORNING !!! | require : smex")
  (smex-initialize)
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  ;; This is your old M-x.
  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command))


(provide 'sub-interface)
