;;; packages-config.el --- Package Management -*- lexical-binding: t; -*-
;;; Commentary:
;; This file manages Emacs packages, package archives, use-package, etc.

;;; Code:

;; =============================================================================
;; Package Management
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

(provide 'packages-config)
;;; packages-config.el ends here
