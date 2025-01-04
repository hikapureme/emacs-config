;;; knowledge-config.el --- Knowledge & Note Management -*- lexical-binding: t; -*-
;;; Commentary:
;; This file handles advanced note-taking packages such as Org-roam, Deft, etc.

;;; Code:

;; =============================================================================
;; 6. Knowledge & Note Management
;; =============================================================================

;; Org-roam
(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory "~/org/roam")
  :config
  (org-roam-db-autosync-mode))

;; Deft
(use-package deft
  :ensure t
  :custom
  (deft-directory "~/org")
  (deft-extensions '("org"))
  (deft-recursive t))

(provide 'knowledge-config)
;;; knowledge-config.el ends here
