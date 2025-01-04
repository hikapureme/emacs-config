;;; languages-config.el --- Language Support -*- lexical-binding: t; -*-
;;; Commentary:
;; This file contains configurations for various programming languages (e.g., Lua).

;;; Code:

;; =============================================================================
;; Syntax highlight by lua-mode
;; =============================================================================

(use-package lua-mode
  :ensure t
  :mode ("\\.lua\\'" . lua-mode)
  :config
  (setq lua-indent-level 4))

(provide 'languages-config)
;;; languages-config.el ends here
