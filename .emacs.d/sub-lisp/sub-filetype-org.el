;; ========================================================================
;;  font size
;; ========================================================================
(custom-set-faces
 '(org-level-1 ((t (:inherit outline-1 :height 1.0))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.0))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.0))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.0))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.0))))
 )


;; ========================================================================
;;  TODO - list
;; ========================================================================
(setq org-todo-keywords
      '((sequence "TODO(t)" "WAIT(w)" "|" "DONE(d)" "SOMEDAY(s)")))
(setq org-log-done 'time)


;; ========================================================================
;;  TODO - ranking
;; ========================================================================
(defun org-my-calculate-score-insert ()
  (interactive)
  (let ((pos (point)) (score 0) (max 0)
        (end (string-match "^\*" (buffer-substring (line-beginning-position) (point-max)))))
    (if (not end) (setq end (point-max))
      (setq end (+ (line-beginning-position) end)))
    (outline-backward-same-level 0)
    (while (and (search-forward-regexp "^\\W*\+ *\[[ X-]\]\\W*[SABCDE]:" nil t) (< (point) end))
      (backward-char 1)
      (let ((pt 0) (cchar (preceding-char)))
        (if (eq cchar (string-to-char "S"))
            (setq pt (expt 2 5))
          (setq pt (expt 2 (- 5 (- cchar (- (string-to-char "A") 1))))))
        (message (format "found: %s, %d" (string cchar) pt))
        (when (string-match "\\[X\\]" (buffer-substring (line-beginning-position) (line-end-position)))
          (setq score (+ score pt)))
        (setq max (+ max pt)) ))
    (goto-char pos)
    (unless (< max 1)
      (outline-backward-same-level 0)
      (search-forward-regexp "^.*\]" nil t)
      (let ((delp (point)) (pofs (- (line-end-position) (point))) (nofs 0))
        (delete-region (point) (line-end-position))
        (tab-to-tab-stop)
        (insert (format "+{%3d/%3d}" score max))
        (message (format "score/max: %d/%d" score max))
        (setq nofs (- (point) delp))
        (goto-char (+ pos (- nofs pofs)))))
    (list score max)))

(defadvice org-toggle-checkbox
  (after org-toggle-checkbox-advice activate)
  (org-my-calculate-score-insert))

(ad-activate 'org-toggle-checkbox 'org-toggle-checkbox-advice)

(defvar org-my-get-ranking-day-split 4.0)
(defvar org-my-get-ranking-day-start 8.0)
(defvar org-my-get-ranking-day-end 24.5)

(defun org-my-get-ranking ()
  (interactive)
  (let ((pos (point)) (score 0) (max 0) (expect 0) (slist (list)) (rank 1) (mrank 1) (erank 1) (num 0))
    (let ((scores (org-my-calculate-score-insert)))
      (setq score (car scores))
      (setq max (car (cdr scores))))
    (goto-char (point-min))
    (while (and (search-forward-regexp "\\+{\\W*" nil t)
                (string-match "[0-9]+/\\W*[0-9]+}" (buffer-substring (point) (line-end-position))))
      (setq slist (cons (string-to-number (buffer-substring (point) (search-forward-regexp "[0-9]+" nil t))) slist)))
    (let ((next 99999) (ratio 0.0) (time 0.0) (active (- org-my-get-ranking-day-end org-my-get-ranking-day-start)))
      (setq time (+ (nth 2 (decode-time (current-time))) (/ (nth 1 (decode-time (current-time))) 60.0)))
      (when (< time org-my-get-ranking-day-split) (setq time (+ time 24.0)))
      (setq time (- time org-my-get-ranking-day-start))
      (when (< time 0.0) (setq time 0.0))
      (when (> time active) (setq time active))
      (setq ratio (/ time active))
      (if t (setq expect (/ score ratio))
        (setq expect score))
      (when (> expect max) (setq expect max))
      (while slist
        (let ((top (car slist)))
          (when (< score top) (setq rank (+ rank 1)))
          (when (< max top) (setq mrank (+ mrank 1)))
          (when (< expect top) (setq erank (+ erank 1)))
          (when (and (< score top) (> next top)) (setq next top)))
        (setq num (+ num 1))
        (setq slist (cdr slist)))
      (message (format "score:[%d/%d] rank:[%d/%d] ~%d || expected:[%d(%d)] || day:[%.1f%s] time:[%.1fh/%.1fh]"
                       score max rank num (- next score) expect erank (* 100.0 ratio) "%%" time (- active time)))
      (goto-char pos))))

(add-hook 'org-mode-hook
          '(lambda ()
             (org-defkey org-mode-map (kbd "C-c C-;") 'org-my-get-ranking)))


(provide 'sub-filetype-org)
