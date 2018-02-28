;; ========================================================================
;;  indent top
;; -----------------------------------------------------------------------
;;   C-a
;; ========================================================================
(global-set-key "\C-a" 'beggining-of-indented-line)
(defun beggining-of-indented-line (current-point)
  "[top]<-->[indent]"
  (interactive "d")
  (if (string-match
       "^[ \t]+$"
       (save-excursion
         (buffer-substring-no-properties
          (progn (beginning-of-line) (point))
          current-point)))
      (beginning-of-line)
    (back-to-indentation)))

(setq scroll-conservatively 35
      scroll-margin 0
      scroll-step 4)

(provide 'sub-cursor)
