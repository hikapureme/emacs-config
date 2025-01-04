;;; org-config.el --- Org Mode Configuration -*- lexical-binding: t; -*-
;;; Commentary:
;; This file handles Org Mode settings, including org-bullets, org-download, etc.

;;; Code:

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

(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode)
  :config
  (setq org-bullets-bullet-list '("◉" "○" "◆" "◇" "▶")))

(use-package org-download
  :ensure t
  :config
  (setq org-download-method 'attach))

(provide 'org-config)
;;; org-config.el ends here
