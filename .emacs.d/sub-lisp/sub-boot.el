;;----------------------------------------------------------------------------
;; 起動時設定
;;----------------------------------------------------------------------------

;; スタート画面非表示
(setq inhibit-startup-message t)

;; スタート時Windowサイズ - maximized(全画面) | fullboth(フルスクリーン)
(set-frame-parameter nil 'fullscreen 'maximized)

(provide 'sub-boot)
