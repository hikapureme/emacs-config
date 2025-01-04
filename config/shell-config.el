;;; shell-config.el --- Terminal & Shell Configuration -*- lexical-binding: t; -*-
;;; Commentary:
;; This file handles Emacs terminal/shell settings, such as vterm or eshell.

;;; Code:

;; =============================================================================
;; 7. Terminal & Shell
;; =============================================================================

(use-package vterm
  :ensure t)
(global-set-key (kbd "C-c T") 'vterm)  ;; C-c T opens vterm

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

(provide 'shell-config)
;;; shell-config.el ends here
