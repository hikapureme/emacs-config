;;; init.el --- My Emacs Configuration -*- lexical-binding: t; -*-
;;; Commentary:
;; This is my Emacs configuration, split into multiple files for clarity and
;; consistency. Each file resides in ~/.emacs.d/config/.

;;; Code:

;; =============================================================================
;; Load splitted configuration files
;; =============================================================================

;; Add config/ to the load-path
(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))

;; Require each .el file in sequence
(require 'basic-config)       ;; 1. Basic Settings
(require 'packages-config)    ;; 2. Package Management
(require 'keybind-config)     ;; 3. Keybindings
(require 'ui-config)          ;; 4. Visual Enhancements
(require 'org-config)         ;; 5. Org Mode Configuration
(require 'knowledge-config)   ;; 6. Knowledge & Note Management
(require 'shell-config)       ;; 7. Terminal & Shell
(require 'languages-config)   ;; 8. Language Support
(require 'custom-vars)        ;; 9. Custom Variables & Faces

(provide 'init)
;;; init.el ends here
