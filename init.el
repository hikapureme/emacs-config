;; -------------------------------------------------------------------
;; 基本設定 (modifierキーなど)
;; -------------------------------------------------------------------
(setq mac-control-modifier 'control)
(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'super)
(setq mac-right-option-modifier nil)
(setq select-enable-clipboard t)
(setq select-enable-primary t)
(setq inhibit-startup-screen t)
(global-display-line-numbers-mode t)

(global-set-key (kbd "s-c") 'kill-ring-save)
(global-set-key (kbd "s-x") 'kill-region)
(global-set-key (kbd "s-v") 'yank)
(global-set-key (kbd "s-a") 'mark-whole-buffer)
(global-set-key (kbd "s-s") 'save-buffer)
(global-set-key (kbd "s-z") 'undo)
(global-set-key (kbd "s-n") 'make-frame-command)
(global-set-key (kbd "s-w") 'kill-this-buffer)

;; -------------------------------------------------------------------
;; MELPAを有効化
;; -------------------------------------------------------------------
;; MELPAリポジトリを追加
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; -------------------------------------------------------------------
;; テーマ: doom-themes
;; -------------------------------------------------------------------
;; Doom Themesの読み込み
(require 'doom-themes)

;; テーマをロード
(load-theme 'doom-one t) ;; お好みのテーマを指定

;; -------------------------------------------------------------------
;; LaTeXのための設定
;; -------------------------------------------------------------------
(with-eval-after-load 'org
  ;; LaTeXフラグメントのプレビュー設定
  (setq org-preview-latex-default-process 'dvisvgm)
  (setq org-format-latex-options
        (plist-put org-format-latex-options :scale 1.5)) ;; 数式のスケール設定
  (setq org-format-latex-options
        (plist-put org-format-latex-options :foreground "white")) ;; 数式の色
  (setq org-latex-create-formula-image-program 'dvisvgm)
  (setq org-preview-latex-image-directory "~/.emacs.d/ltximg/") ;; キャッシュディレクトリ

  ;; 自動更新の設定
  (defun my/org-refresh-latex-fragments ()
    "Org-modeでLaTeXフラグメントを自動更新する。"
    (when (eq major-mode 'org-mode)
      (org-preview-latex-fragment)))
  (add-hook 'post-command-hook 'my/org-refresh-latex-fragments))
