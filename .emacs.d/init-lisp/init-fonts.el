;; フォント
;; abcdefghijklmnopqrstuvwxyz
;; ABCDEFGHIJKLMNOPQRSTUVWXYZ
;; `1234567890-=\[];',./
;; ~!@#$%^&*()_+|{}:"<>?
;;
;; 壱弐参四五壱弐参四五壱弐参四五壱弐参四五壱弐参四五壱弐参四五
;; 123456789012345678901234567890123456789012345678901234567890
;; ABCdeＡＢＣｄｅ
;;
;; ┌─────────────────────────────┐
;; │　　　　　　　　　　　　　罫線                            │
;; └─────────────────────────────┘
;;

(set-face-attribute 'default nil :family "Consolas")
(set-fontset-font (frame-parameter nil 'font)
                  'japanese-jisx0208
                  '("ＭＳ ゴシック" . "unicode-bmp")
                  )
(set-fontset-font (frame-parameter nil 'font)
                  'katakana-jisx0201
                  '("ＭＳ ゴシック" . "unicode-bmp")
                  )

(provide 'init-fonts)
