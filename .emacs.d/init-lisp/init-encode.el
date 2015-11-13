(setq default-input-method "W32-IME")
(setq-default w32-ime-mode-line-state-indicator "[--]")
(w32-ime-initialize)
(setq w32-ime-mode-line-state-indicator-list '("[--]" "[あ]" "[--]"))
(global-set-key [M-kanji] 'ignore)

(set-language-environment  'utf-8)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-file-name-coding-system 'shift_jis)

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
(set-face-attribute 'default nil :family "Consolas" :height 100)
(set-fontset-font (frame-parameter nil 'font)
                  'japanese-jisx0208
                  (font-spec :family "ＭＳ ゴシック" :size 14))
;(set-face-attribute 'default nil :family "Consolas")
;(setq face-font-rescale-alist '("ＭＳ ゴシック"))


;(set-language-environment 'Japanese)
;(set-terminal-coding-system 'utf-8)
;(set-keyboard-coding-system 'utf-8)
;(set-buffer-file-coding-system 'utf-8-unix)
;(setq default-buffer-file-coding-system 'utf-8)
;(prefer-coding-system 'utf-8)
;(set-default-coding-systems 'utf-8)
;(setq file-name-coding-system 'utf-8)
;(set-clipboard-coding-system 'utf-8)
;(setq default-input-method 'japanese-anthy)

(provide 'sub-encode)
