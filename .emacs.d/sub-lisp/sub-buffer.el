;; ========================================================================
;;  Set the keybind.
;; ========================================================================
(global-set-key (kbd "M-k") 'kill-buffer)       ; Cut the cursor below.


;; ========================================================================
;;  F5
;; ------------------------------------------------------------------------
;;   <http://qiita.com/ironsand/items/749b032d33d389972b4b>
;; ========================================================================
(global-set-key (kbd "<f5>") 'revert-buffer-no-confirm)
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


(provide 'sub-buffer)
