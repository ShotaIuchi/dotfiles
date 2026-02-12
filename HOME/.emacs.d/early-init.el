;; -*- lexical-binding: t; -*-
;; ==============================================================================
;; Emacs Early Init
;; ==============================================================================
;; Loaded before init.el and before the GUI frame is created.
;; Used for startup optimization and UI suppression.

;; ------------------------------------------------------------------------------
;; Startup Performance
;; ------------------------------------------------------------------------------

;; Increase GC threshold during startup (restore later in init.el)
(setq gc-cons-threshold most-positive-fixnum)

;; Suppress file-name-handler processing during startup
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; ------------------------------------------------------------------------------
;; UI Suppression (before frame creation)
;; ------------------------------------------------------------------------------

(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message user-login-name)
(setq initial-scratch-message nil)

;; Disable UI elements before they render
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Prevent flash of unstyled mode-line
(push '(internal-border-width . 0) default-frame-alist)

;; Prevent package.el from auto-loading at startup (use-package handles this)
(setq package-enable-at-startup nil)

;; ------------------------------------------------------------------------------
;; Frame Defaults
;; ------------------------------------------------------------------------------

(push '(width . 120) default-frame-alist)
(push '(height . 40) default-frame-alist)

;; Font (UDEV Gothic NFLG - matches Ghostty/terminal config)
(push '(font . "UDEV Gothic NFLG-14") default-frame-alist)
