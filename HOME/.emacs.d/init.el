;; -*- lexical-binding: t; -*-
;; ==============================================================================
;; Emacs Init
;; ==============================================================================

;; ------------------------------------------------------------------------------
;; Restore Startup Performance Settings
;; ------------------------------------------------------------------------------

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024)  ; 16MB
                  file-name-handler-alist default-file-name-handler-alist)))

;; ------------------------------------------------------------------------------
;; Package Management
;; ------------------------------------------------------------------------------

(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu"   . "https://elpa.gnu.org/packages/")))
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; Retry with refreshed archives when package install fails
(defvar my/package-archives-refreshed nil)
(advice-add 'package-install :around
            (lambda (orig-fn &rest args)
              (condition-case err
                  (apply orig-fn args)
                (error
                 (if my/package-archives-refreshed
                     (signal (car err) (cdr err))
                   (setq my/package-archives-refreshed t)
                   (package-refresh-contents)
                   (apply orig-fn args))))))

;; ------------------------------------------------------------------------------
;; Basic Settings
;; ------------------------------------------------------------------------------

;; Encoding
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)

;; Backup / Autosave (collect in one place)
(setq backup-directory-alist '(("." . "~/.emacs.d/backups/"))
      auto-save-file-name-transforms '((".*" "~/.emacs.d/autosave/" t))
      backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(make-directory "~/.emacs.d/backups/" t)
(make-directory "~/.emacs.d/autosave/" t)

;; C-h as backspace (help moved to C-?)
(define-key key-translation-map (kbd "C-h") (kbd "DEL"))
(global-set-key (kbd "C-?") 'help-command)

;; General behavior
(setq-default indent-tabs-mode nil
              tab-width 4)
(setq ring-bell-function 'ignore
      use-short-answers t
      confirm-kill-emacs 'y-or-n-p
      scroll-conservatively 101
      scroll-margin 5
      mouse-wheel-scroll-amount '(3 ((shift) . 1))
      mouse-wheel-progressive-speed nil)

;; Line numbers
(global-display-line-numbers-mode t)
(setq display-line-numbers-type 'relative)

;; Highlight current line
(global-hl-line-mode t)

;; Show matching parentheses
(show-paren-mode t)
(setq show-paren-delay 0)

;; Auto-close brackets
(electric-pair-mode t)

;; Recent files
(recentf-mode t)
(setq recentf-max-saved-items 100)

;; Minibuffer history persistence
(savehist-mode t)

;; Remember cursor position
(save-place-mode t)

;; Auto-revert files changed on disk
(global-auto-revert-mode t)

;; Column number in mode-line
(column-number-mode t)

;; Delete selection on type
(delete-selection-mode t)

;; Native compilation warnings
(setq native-comp-async-report-warnings-errors 'silent)

;; ------------------------------------------------------------------------------
;; Theme & Appearance
;; ------------------------------------------------------------------------------

(use-package doom-themes
  :config
  (load-theme 'doom-tokyo-night t)
  (doom-themes-org-config))

(use-package doom-modeline
  :init (doom-modeline-mode t)
  :custom
  (doom-modeline-height 28)
  (doom-modeline-buffer-encoding nil))

;; Icons (required by doom-modeline)
(use-package nerd-icons
  :config
  (unless (member "Symbols Nerd Font Mono" (font-family-list))
    (nerd-icons-install-fonts t)))

;; ------------------------------------------------------------------------------
;; Minibuffer Completion (Vertico Stack)
;; ------------------------------------------------------------------------------

(use-package vertico
  :init (vertico-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :init (marginalia-mode))

(use-package consult
  :bind (("C-s"     . consult-line)
         ("C-x b"   . consult-buffer)
         ("C-x C-r" . consult-recent-file)
         ("M-g g"   . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-s r"   . consult-ripgrep)
         ("M-s f"   . consult-find)))

;; ------------------------------------------------------------------------------
;; Keybinding Help
;; ------------------------------------------------------------------------------

(use-package which-key
  :init (which-key-mode)
  :custom
  (which-key-idle-delay 0.5))

;; ------------------------------------------------------------------------------
;; Code Completion
;; ------------------------------------------------------------------------------

(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 2)
  (corfu-cycle t)
  :init (global-corfu-mode))

(use-package cape
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file))

;; ------------------------------------------------------------------------------
;; LSP (Eglot - built-in since Emacs 29)
;; ------------------------------------------------------------------------------

(use-package eglot
  :ensure nil
  :hook ((python-mode    . eglot-ensure)
         (js-mode        . eglot-ensure)
         (typescript-mode . eglot-ensure)
         (rust-mode      . eglot-ensure)
         (go-mode        . eglot-ensure)
         (c-mode         . eglot-ensure)
         (c++-mode       . eglot-ensure))
  :custom
  (eglot-autoshutdown t)
  (eglot-events-buffer-size 0))

;; ------------------------------------------------------------------------------
;; Tree-sitter (built-in since Emacs 29)
;; ------------------------------------------------------------------------------

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;; ------------------------------------------------------------------------------
;; Git
;; ------------------------------------------------------------------------------

(use-package magit
  :bind ("C-x g" . magit-status))

(use-package diff-hl
  :hook ((after-init . global-diff-hl-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh)))

;; ------------------------------------------------------------------------------
;; Project Management
;; ------------------------------------------------------------------------------

(use-package project
  :ensure nil
  :bind-keymap ("C-x p" . project-prefix-map))

;; ------------------------------------------------------------------------------
;; EditorConfig
;; ------------------------------------------------------------------------------

(use-package editorconfig
  :config (editorconfig-mode t))

;; ------------------------------------------------------------------------------
;; Terminal (vterm) - Claude Code Integration
;; ------------------------------------------------------------------------------

(use-package vterm
  :bind ("C-c t" . vterm)
  :custom
  (vterm-max-scrollback 10000)
  (vterm-shell "/bin/zsh"))

;; Toggle vterm in a bottom window
(defun my/vterm-toggle ()
  "Toggle vterm window at the bottom."
  (interactive)
  (let ((vterm-buf (get-buffer "vterm")))
    (if (and vterm-buf (get-buffer-window vterm-buf))
        (delete-window (get-buffer-window vterm-buf))
      (let ((win (split-window-below -15)))
        (select-window win)
        (if vterm-buf
            (switch-to-buffer vterm-buf)
          (vterm))))))

(global-set-key (kbd "C-c C-t") #'my/vterm-toggle)

;; ------------------------------------------------------------------------------
;; Miscellaneous
;; ------------------------------------------------------------------------------

;; Smooth scrolling (Emacs 29+)
(when (>= emacs-major-version 29)
  (pixel-scroll-precision-mode t))

;; Whitespace cleanup on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Dired
(setq dired-listing-switches "-alh --group-directories-first"
      dired-dwim-target t)

;; Org-mode basics
(setq org-startup-indented t
      org-hide-leading-stars t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
