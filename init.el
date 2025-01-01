;;; init.el --- My Emacs Configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; This is my Emacs configuration, written in English, with clear structure,
;; reduced redundancy, Lua support, and a set of keybindings for comfortable usage.

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

;; =============================================================================
;; 2. Package Management
;; =============================================================================

(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu"   . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; If use-package is not installed, install it
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; =============================================================================
;; 3. Keybindings
;; =============================================================================

;; Mac-like keybindings (Super = Command key on macOS)
(global-set-key (kbd "s-c") 'kill-ring-save)
(global-set-key (kbd "s-x") 'kill-region)
(global-set-key (kbd "s-v") 'yank)
(global-set-key (kbd "s-a") 'mark-whole-buffer)
(global-set-key (kbd "s-s") 'save-buffer)
(global-set-key (kbd "s-z") 'undo)
(global-set-key (kbd "s-n") 'make-frame-command)
(global-set-key (kbd "s-w") 'kill-this-buffer)

;; Move word by word using Alt (Option on macOS) + arrow
(global-set-key (kbd "<M-right>") 'forward-word)
(global-set-key (kbd "<M-left>") 'backward-word)

;; Custom commands to jump to non-blank lines
(defun move-to-next-non-blank-line ()
  "Move to the next non-blank line."
  (interactive)
  (forward-line 1)
  (while (and (not (eobp)) (looking-at-p "^[[:space:]]*$"))
    (forward-line 1)))

(defun move-to-previous-non-blank-line ()
  "Move to the previous non-blank line."
  (interactive)
  (forward-line -1)
  (while (and (not (bobp)) (looking-at-p "^[[:space:]]*$"))
    (forward-line -1)))

(global-set-key (kbd "<M-down>") 'move-to-next-non-blank-line)
(global-set-key (kbd "<M-up>")   'move-to-previous-non-blank-line)

;; Select while moving to non-blank lines
(defun select-to-next-non-blank-line ()
  "Select until the next non-blank line."
  (interactive)
  (let ((current-pos (point)))
    (move-to-next-non-blank-line)
    (set-mark-command nil)
    (goto-char current-pos)))

(defun select-to-previous-non-blank-line ()
  "Select until the previous non-blank line."
  (interactive)
  (let ((current-pos (point)))
    (move-to-previous-non-blank-line)
    (set-mark-command nil)
    (goto-char current-pos)))

(global-set-key (kbd "<M-S-down>") 'select-to-next-non-blank-line)
(global-set-key (kbd "<M-S-up>")   'select-to-previous-non-blank-line)

;; =============================================================================
;; 4. Visual Enhancements
;; =============================================================================

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-one t))  ;; Choose your favorite Doom theme

;; Org-bullets: pretty bullets in Org Mode
(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode)
  :config
  (setq org-bullets-bullet-list '("◉" "○" "◆" "◇" "▶")))

;; =============================================================================
;; 5. Org Mode Configuration
;; =============================================================================

(use-package org
  :ensure nil
  :config
  ;; Where to store TODOs for Org Agenda
  (setq org-agenda-files '("~/org/todo.org"))

  ;; Custom TODO keywords
  (setq org-todo-keywords
        '((sequence "TODO" "IN-PROGRESS" "DONE")))

  ;; Timestamp when marking tasks as DONE
  (setq org-log-done 'time)

  ;; Show deadlines 7 days in advance
  (setq org-deadline-warning-days 7)

  ;; Show 1 week in the Agenda view
  (setq org-agenda-span 'week)

  ;; Customize heading faces
  (custom-set-faces
   '(org-level-1 ((t (:inherit default :weight bold :height 1.3))))
   '(org-level-2 ((t (:inherit default :weight bold :height 1.2))))
   '(org-level-3 ((t (:inherit default :weight bold :height 1.1)))))

  ;; Configure LaTeX previews
  (setq org-preview-latex-default-process 'dvisvgm
        org-latex-create-formula-image-program 'dvisvgm
        org-preview-latex-image-directory "~/.emacs.d/ltximg/")
  (setq org-format-latex-options
        (plist-put org-format-latex-options :scale 1.5))
  (setq org-format-latex-options
        (plist-put org-format-latex-options :foreground "white"))

  ;; Auto refresh LaTeX fragments
  (defun my/org-refresh-latex-fragments ()
    "Automatically refresh LaTeX fragments in Org Mode."
    (when (eq major-mode 'org-mode)
      (org-preview-latex-fragment)))
  (add-hook 'post-command-hook 'my/org-refresh-latex-fragments)

  ;; Keybindings for Org
  (global-set-key (kbd "C-c a") 'org-agenda)
  (global-set-key (kbd "C-c t") 'org-todo))

;; org-download: Drag & drop images into Org buffers
(use-package org-download
  :ensure t
  :config
  (setq org-download-method 'attach))

;; =============================================================================
;; 6. Knowledge & Note Management
;; =============================================================================

;; Org-roam: advanced note-taking
(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory "~/org/roam")
  :config
  (org-roam-db-autosync-mode))

;; Deft: quick note search
(use-package deft
  :ensure t
  :custom
  (deft-directory "~/org")
  (deft-extensions '("org"))
  (deft-recursive t))

;; =============================================================================
;; 7. Terminal & Shell
;; =============================================================================

;; vterm: a powerful terminal emulator in Emacs
(use-package vterm
  :ensure t)
(global-set-key (kbd "C-c T") 'vterm)  ;; C-c T opens vterm

;; Eshell configuration
(use-package eshell
  :ensure nil
  :config
  (setq eshell-prompt-function
        (lambda ()
          (concat "[" (user-login-name) "@" (system-name) "] "
                  (abbreviate-file-name (eshell/pwd))
                  (if (= (user-uid) 0) " # " " $ "))))
  (add-hook 'eshell-mode-hook
            (lambda ()
              (eshell-alias "ll" "ls -l")
              (eshell-alias "la" "ls -a"))))

;; Default shell to Zsh (if installed)
(when (executable-find "zsh")
  (setq explicit-shell-file-name "zsh"))

;; =============================================================================
;; 8. Lua Support
;; =============================================================================

(use-package lua-mode
  :ensure t
  :mode ("\\.lua\\'" . lua-mode)
  :config
  ;; Example: set indentation level
  (setq lua-indent-level 4))

;; =============================================================================
;; 9. Custom Variables (Generated by Emacs)
;; =============================================================================

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(lua-mode deft org-roam org-download org-bullets vterm slime-volleyball slime posframe org-modern exec-path-from-shell doom-themes auctex)))

;; =============================================================================
;; Final Setup
;; =============================================================================

(provide 'init)
;;; init.el ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-level-1 ((t (:inherit default :weight bold :height 1.3))))
 '(org-level-2 ((t (:inherit default :weight bold :height 1.2))))
 '(org-level-3 ((t (:inherit default :weight bold :height 1.1)))))
