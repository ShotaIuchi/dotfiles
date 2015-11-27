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

; japanese
(set-language-environment  'utf-8)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-file-name-coding-system 'shift_jis)

; windows
(when (eq system-type 'windows-nt)
  ; ime
  (setq default-input-method "W32-IME")
  (setq-default w32-ime-mode-line-state-indicator "[--]")
  (w32-ime-initialize)
  (setq w32-ime-mode-line-state-indicator-list '("[--]" "[あ]" "[--]"))
  (global-set-key [M-kanji] 'ignore)

  ; font
  (set-face-attribute 'default nil :family "Consolas" :height 100)
  (set-fontset-font (frame-parameter nil 'font)
                    'japanese-jisx0208
                    (font-spec :family "ＭＳ ゴシック" :size 14))
  )

; mac
(when (eq system-type 'darwin)
  ; font
  (set-face-attribute 'default nil :family "Monaco" :height 90)
  (set-fontset-font (frame-parameter nil 'font)
                    'japanese-jisx0208
                    '("Hiragino Maru Gothic Pro" . "iso10646-1"))
  (set-fontset-font (frame-parameter nil 'font)
                    'japanese-jisx0212
                    '("Hiragino Maru Gothic Pro" . "iso10646-1"))
  (set-fontset-font (frame-parameter nil 'font)
                    'mule-unicode-0100-24ff
                    '("monaco" . "iso10646-1"))
  (setq face-font-rescale-alist
      '(("^-apple-hiragino.*" . 1.2)
        (".*osaka-bold.*" . 1.2)
        (".*osaka-medium.*" . 1.2)
        (".*courier-bold-.*-mac-roman" . 1.0)
        (".*monaco cy-bold-.*-mac-cyrillic" . 0.9)
        (".*monaco-bold-.*-mac-roman" . 0.9)
        ("-cdac$" . 1.3)))
  )

(when (eq system-type 'gnu/linux)
  )

(provide 'sub-fonts)
