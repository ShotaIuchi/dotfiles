;; フォント
;; abcdefghijklmnopqrstuvwxyz
;; ABCDEFGHIJKLMNOPQRSTUVWXYZ
;; `1234567890-=\[];',./
;; ~!@#$%^&*()_+|{}:"<>?
;;
;; 壱弐参四五壱弐参四五壱弐参四五壱弐参四五壱弐参四五壱弐参四五
;; あいうえおかきくけこさしすせそたちつてとなにぬねのはひふへほ
;; アイウエオカキクケコサシスセソタチツテトナニヌネノハヒフヘホ
;; 123456789012345678901234567890123456789012345678901234567890
;; ABCdeＡＢＣｄｅ
;;
;; ┌─────────────────────────────┐
;; │　　　　　　　　　　　　　罫線                            │
;; └─────────────────────────────┘
;;

;; japanese
(set-language-environment  'utf-8)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-file-name-coding-system 'shift_jis)


;; windows
(when (eq system-type 'windows-nt)
  (setq default-input-method "W32-IME")
  (setq-default w32-ime-mode-line-state-indicator "[--]")
  (w32-ime-initialize)
  (setq w32-ime-mode-line-state-indicator-list '("[--]" "[あ]" "[--]"))
  (global-set-key [M-kanji] 'ignore)

  (set-face-attribute 'default nil :family "Consolas" :height 104)
  (set-fontset-font nil 'japanese-jisx0208 (font-spec :family "メイリオ"))
  (setq face-font-rescale-alist '(("メイリオ" . 1.08)))
  )


;; mac
(when (eq system-type 'darwin)
  (set-face-attribute 'default nil :family "monaco" :height 104)
  (set-fontset-font nil 'japanese-jisx0208 (font-spec :family "Hiragino Kaku Gothic ProN"))
  (setq face-font-rescale-alist '(("Hiragino Kaku Gothic ProN" . 1.20))))


;; linux
(when (eq system-type 'gnu/linux)
  )

(provide 'sub-fonts)
