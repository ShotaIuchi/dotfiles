;; ========================================================================
;;  grep
;; ========================================================================
(unless (require 'wgrep nil t)
  (message "!!! WORNING !!! | require : wgrep"))

(setq null-device "/dev/null")
(global-set-key (kbd "C-M-g") 'rgrep)


;; ========================================================================
;;  tags
;; ========================================================================
(if (not (require 'gtags nil t))
    (message "!!! WORNING !!! | require : gtags")
  (setq gtags-mode-hook
        '(lambda ()
           (local-set-key "\M-t" 'gtags-find-tag)
           (local-set-key "\M-r" 'gtags-find-rtag)
           (local-set-key "\M-s" 'gtags-find-symbol)
           (local-set-key "\M-p" 'gtags-find-pattern)
           (local-set-key "\M-a" 'gtags-find-file)
           (local-set-key "\C-t" 'gtags-pop-stack)
           ))
  (add-hook 'c-mode-hook (lambda () (gtags-mode 1)))
  (add-hook 'c++-mode-hook (lambda () (gtags-mode 1)))
  (add-hook 'java-mode-hook (lambda () (gtags-mode 1)))
  )


;; ========================================================================
;; migemo
;; ========================================================================
(if (not (and
          (require 'migemo nil t)
          (executable-find "cmigemo")))
    (message "!!! WORNING !!! | require : migemo")
  (setq migemo-options '("-q" "--emacs"))
  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil)
  (setq migemo-coding-system 'utf-8-unix)
  (load-library "migemo")
  (migemo-init)

  ;; ========================================================================
  ;; windows
  ;;  > cmigemo : http://www.kaoriya.net/software/cmigemo/
  ;;  >           -> download : C/Migemo for Windows xxbit
  ;;  >           --> .\cmigemo.dll, .\cmigemo.exe
  ;;  >           -->   =copy=> ..\emacs\libexec\emacs\xx.x\xxx\
  ;;  >           --> .\dict\utf-8
  ;;  >           -->   =copy=> ~\.emacs.d\etc\dict\cmigemo\
  ;; ========================================================================
  (when (eq system-type 'windows-nt)
    (setq migemo-command "cmigemo")
    (setq migemo-dictionary (expand-file-name "~/.emacs.d/etc/dict/cmigemo/utf-8/migemo-dict"))
    )

  ;; ========================================================================
  ;; mac
  ;;  > brew install cmigemo
  ;; ========================================================================
  (when (eq system-type 'darwin)
    (setq migemo-command "/usr/local/bin/cmigemo")
    (setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")
    )

  ;; ========================================================================
  ;; linux
  ;;  > sudo apt-get install cmigemo
  ;; ========================================================================
  (when (eq system-type 'gnu/linux)
    (setq migemo-command "cmigemo")
    (setq migemo-dictionary "/usr/share/cmigemo/utf-8/migemo-dict")
    )
  )


(provide 'sub-grep)
