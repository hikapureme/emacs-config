;;; init.el --- My Emacs Configuration (Layered with subdirectories) -*- lexical-binding: t; -*-
;;; Commentary:
;; This init loads each subdirectory in ~/.emacs.d/config/ in a fixed order:
;;   01-base -> 02-prog -> 03-org -> 04-apps -> 90-custom
;;
;; Feel free to add or remove directories as needed.

;;; Code:

(defvar my-layer-dirs
  '("01-base"
    "02-prog"
    "03-org"
    "04-apps"
    "90-custom")
  "List of subdirectories in `~/.emacs.d/config/` to load in order.")

(defun my/load-layer (layer-dir)
  "Load all .el files in LAYER-DIR."
  (let* ((root (expand-file-name "config" user-emacs-directory))
         (full-dir (expand-file-name layer-dir root))
         (files (directory-files full-dir t "\\.el$")))
    (dolist (f files)
      (message "Loading %s" f)
      (load f))))

(dolist (dir my-layer-dirs)
  (my/load-layer dir))

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(lsp-mode flycheck company haskell-mode vterm slime-volleyball slime posframe org-roam org-modern org-download org-bullets lua-mode exec-path-from-shell doom-themes deft auctex)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
