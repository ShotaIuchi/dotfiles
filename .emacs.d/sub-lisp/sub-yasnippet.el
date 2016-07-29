;; ========================================================================
;;  yasnippet
;; ========================================================================
(if (not (require 'yasnippet nil t))
    (message "!!! WORNING !!! | require : yasnippet")

  (setq yas-snippet-dirs
        '("~/.emacs.d/yasnippet/sub"
          ;; https://github.com/AndreaCrotti/yasnippet-snippets
          ;; https://github.com/gausby/HTML5-YASnippet-bundle
          "~/.emacs.d/yasnippet/site"
          ))

  ;; new snippet
  (define-key yas-minor-mode-map (kbd "C-x i n") 'yas-new-snippet)
  ;; edit snippet
  (define-key yas-minor-mode-map (kbd "C-x i v") 'yas-visit-snippet-file)
  ;; insert snippet
  (define-key yas-minor-mode-map (kbd "C-x i i") 'yas-insert-snippet)

  (yas-global-mode t))


(provide 'sub-yasnippet)
