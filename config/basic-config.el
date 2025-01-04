;;; basic-config.el --- Basic Settings -*- lexical-binding: t; -*-
;;; Commentary:
;; This file contains essential/primary Emacs settings that are not dependent on
;; any external packages.

;;; Code:

;; =============================================================================
;; 1. Basic Settings
;; =============================================================================

;; Disable startup screen
(setq inhibit-startup-screen t)

;; Enable clipboard integration
(setq select-enable-clipboard t)
(setq select-enable-primary t)

;; macOS-specific Modifier Keys (only apply if on macOS)
(when (eq system-type 'darwin)
  (setq mac-control-modifier 'control)
  (setq mac-option-modifier 'meta)
  (setq mac-command-modifier 'super)
  (setq mac-right-option-modifier nil))

;; Always display line numbers
(global-display-line-numbers-mode t)

;; Shift + arrow keys to select text
(setq shift-select-mode t)

(provide 'basic-config)
;;; basic-config.el ends here
