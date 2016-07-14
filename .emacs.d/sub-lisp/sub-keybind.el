;;----------------------------------------------------------------------------
;; yes/no --> y/n
;;----------------------------------------------------------------------------
(defalias 'yes-or-no-p 'y-or-n-p)

;;----------------------------------------------------------------------------
;; Set the keybind.
;;----------------------------------------------------------------------------
(delete-selection-mode t)                       ; Region can be deleted.
(global-set-key (kbd "M-k") 'kill-buffer)       ; Cut the cursor below.

;;----------------------------------------------------------------------------
;; mouse
;;----------------------------------------------------------------------------
(xterm-mouse-mode t)
(mouse-wheel-mode t)
(global-set-key [mouse-4] (kbd "C-p"))
(global-set-key [mouse-5] (kbd "C-n"))

;;----------------------------------------------------------------------------
;; Unset the keybind.
;;----------------------------------------------------------------------------
(global-unset-key (kbd "M-c"))                  ; unset - capitalize-word
(global-unset-key (kbd "M-l"))                  ; unset - downcase-word

;;----------------------------------------------------------------------------
;; F5
;; <http://qiita.com/ironsand/items/749b032d33d389972b4b>
;;----------------------------------------------------------------------------
(defun revert-buffer-no-confirm (&optional force-reverting)
  "Interactive call to revert-buffer. Ignoring the auto-save
 file and not requesting for confirmation. When the current buffer
 is modified, the command refuses to revert it, unless you specify
 the optional argument: force-reverting to true."
  (interactive "P")
  ;;(message "force-reverting value is %s" force-reverting)
  (if (or force-reverting (not (buffer-modified-p)))
      (revert-buffer :ignore-auto :noconfirm)
    (error "The buffer has been modified")))
(global-set-key (kbd "<f5>") 'revert-buffer-no-confirm)


(provide 'sub-keybind)
