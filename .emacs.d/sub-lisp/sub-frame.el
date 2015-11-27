(if window-system
    (progn
      (tool-bar-mode 0)
      (menu-bar-mode 0)
      (global-linum-mode 0)
      (scroll-bar-mode 0)
      (line-number-mode t)
      (column-number-mode t))
  )

(provide 'sub-frame)
