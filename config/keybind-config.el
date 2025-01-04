;;; keybind-config.el --- Keybindings -*- lexical-binding: t; -*-
;;; Commentary:
;; This file defines global keybindings, including Mac-like shortcuts.

;;; Code:

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
  (while (and (not (eobp)) (looking-at-p \"^[[:space:]]*$\"))
    (forward-line 1)))

(defun move-to-previous-non-blank-line ()
  "Move to the previous non-blank line."
  (interactive)
  (forward-line -1)
  (while (and (not (bobp)) (looking-at-p \"^[[:space:]]*$\"))
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

(provide 'keybind-config)
;;; keybind-config.el ends here
