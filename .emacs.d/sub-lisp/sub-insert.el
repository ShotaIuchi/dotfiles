;;----------------------------------------------------------------------------
;; day/time
;;----------------------------------------------------------------------------
(defun insert-today()
  (interactive)
  (insert (format-time-string "%Y-%m-%d %a" (current-time))))

(defun insert-now()
  (interactive)
  (insert (format-time-string "%H:%M:%S" (current-time))))

(provide 'sub-insert)

