;; ========================================================================
;;  sample
;; ========================================================================
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
;;  util
;; ========================================================================
(defun is-font-exist (font-name)
  (if (null (x-list-fonts font-name)) nil t))


;; ========================================================================
;; windows
;; ========================================================================
(when (os-type-windows)
  (when (ui-type-gui)
    (if (is-font-exist "MeiryoKe_Gothic")
        (progn
          (set-face-attribute 'default nil :family "MeiryoKe_Gothic" :height 90)
          (set-fontset-font nil 'japanese-jisx0208 (font-spec :family "MeiryoKe_Gothic"))
          (setq face-font-rescale-alist '(("MeiryoKe_Gothic" . 1.00))))
      (set-face-attribute 'default nil :family "Consolas" :height 104)
      (set-fontset-font nil 'japanese-jisx0208 (font-spec :family "メイリオ"))
      (setq face-font-rescale-alist '(("メイリオ" . 1.08))))))


;; ========================================================================
;; mac
;; ========================================================================
(when (os-type-mac)
  (set-face-attribute 'default nil :family "monaco" :height 104)
  (set-fontset-font nil 'japanese-jisx0208 (font-spec :family "Hiragino Kaku Gothic ProN"))
  (setq face-font-rescale-alist '(("Hiragino Kaku Gothic ProN" . 1.20))))


;; ========================================================================
;; linux
;; ========================================================================
(when (os-type-linux)
  (set-face-attribute 'default nil :family "Ricty Diminished" :height 104)
  (set-fontset-font nil 'japanese-jisx0208 (font-spec :family "Ricty Diminished"))
  (setq face-font-rescale-alist '(("Ricty Diminished" . 1.00))))


(provide 'sub-fonts)
