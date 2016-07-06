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

;; ========================================================================
;; japanese
;; ========================================================================
(set-language-environment  'utf-8)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-file-name-coding-system 'shift_jis)


;; ========================================================================
;;  util
;; ========================================================================
(defun is-font-exist (font-name)
  (if (null (x-list-fonts font-name)) nil t))


;; ========================================================================
;; windows
;; ========================================================================
(when (eq system-type 'windows-nt)
  (if (is-font-exist "MeiryoKe_Gothic")
      (progn
        (set-face-attribute 'default nil :family "MeiryoKe_Gothic" :height 90)
        (set-fontset-font nil 'japanese-jisx0208 (font-spec :family "MeiryoKe_Gothic"))
        (setq face-font-rescale-alist '(("MeiryoKe_Gothic" . 1.00))))
    (set-face-attribute 'default nil :family "Consolas" :height 104)
    (set-fontset-font nil 'japanese-jisx0208 (font-spec :family "メイリオ"))
    (setq face-font-rescale-alist '(("メイリオ" . 1.08)))))


;; ========================================================================
;; mac
;; ========================================================================
(when (eq system-type 'darwin)
  (set-face-attribute 'default nil :family "monaco" :height 104)
  (set-fontset-font nil 'japanese-jisx0208 (font-spec :family "Hiragino Kaku Gothic ProN"))
  (setq face-font-rescale-alist '(("Hiragino Kaku Gothic ProN" . 1.20))))


;; ========================================================================
;; linux
;; ========================================================================
(when (eq system-type 'gnu/linux)
  )


;; ========================================================================
;;  emphasis
;; ========================================================================
(require 'whitespace)
(setq whitespace-style '(face           ; faceで可視化
                         trailing       ; 行末
                         tabs           ; タブ
                         empty          ; 先頭/末尾の空行
                         space-mark     ; 表示のマッピング
                         tab-mark
                         ))
(setq whitespace-space-regexp "\\(\x3000+\\)")
(setq whitespace-display-mappings
      '((space-mark ?\x3000 [?\□])
        (tab-mark   ?\t   [?\xBB ?\t])
        ))
(global-whitespace-mode 1)

(provide 'sub-fonts)
