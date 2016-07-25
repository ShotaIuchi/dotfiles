;; ========================================================================
;;  migemo (roman alphabet)
;; ========================================================================
(cond ((not (require 'migemo nil t))
       (message "!!! WORNING !!! | require : migemo.el"))
      ((not (executable-find "cmigemo"))
       (message "!!! WORNING !!! | require : cmigemo"))
      (t    
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
       (when (os-type-windows)
         (setq migemo-command "cmigemo")
         (setq migemo-dictionary (expand-file-name "~/.emacs.d/etc/dict/cmigemo/utf-8/migemo-dict"))
         )

       ;; ========================================================================
       ;; mac
       ;;  > brew install cmigemo
       ;; ========================================================================
       (when (os-type-mac)
         (setq migemo-command "/usr/local/bin/cmigemo")
         (setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")
         )

       ;; ========================================================================
       ;; linux
       ;;  > sudo apt-get install cmigemo
       ;; ========================================================================
       (when (os-type-linux)
         (setq migemo-command "cmigemo")
         (setq migemo-dictionary "/usr/share/cmigemo/utf-8/migemo-dict")
         )
       ))


(provide 'sub-search)
