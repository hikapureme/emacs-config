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
;; テーマ
;; -------------------------------------------------------------------
;; Doom Themesの読み込み
(require 'doom-themes)

;; テーマをロード
(load-theme 'doom-one t) ;; お好みのテーマを指定

;; -------------------------------------------------------------------
;; 視覚的な調整
;; -------------------------------------------------------------------
;; Org-modeの見出しを装飾
(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode)
  :config
  (setq org-bullets-bullet-list '("◉" "○" "◆" "◇" "▶")))

;; Org-modeで画像をドラッグ＆ドロップ
(use-package org-download
  :ensure t
  :config
  (setq org-download-method 'attach))

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

;; -------------------------------------------------------------------
;; タスク管理のための設定
;; -------------------------------------------------------------------
;; Org-modeをロード
(require 'org)

;; Org-agenda用のキーバインド設定
(global-set-key (kbd "C-c a") 'org-agenda) ;; Org-agendaを開く
(global-set-key (kbd "C-c t") 'org-todo)  ;; TODO状態を切り替える

;; Org-agendaで管理するファイルの場所を指定
(setq org-agenda-files '("~/org/todo.org")) ;; 必要に応じてファイルを追加

;; TODOキーワードの設定 (シンプルな状態管理)
(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "DONE")))

;; タスク完了時にタイムスタンプを記録
(setq org-log-done 'time)

;; 締め切り前の警告を表示
(setq org-deadline-warning-days 7)

;; 見た目の調整 (任意)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-level-1 ((t (:inherit default :weight bold :height 1.3))))
 '(org-level-2 ((t (:inherit default :weight bold :height 1.2))))
 '(org-level-3 ((t (:inherit default :weight bold :height 1.1)))))

;; Agendaビューの設定をシンプルに保つ
(setq org-agenda-span 'week) ;; Agendaビューで1週間を表示

;; -------------------------------------------------------------------
;; 知識管理とプロジェクト管理
;; -------------------------------------------------------------------
;; ノートをリンクするための設定
(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory "~/org/roam") ;; ノートの保存先
  :config
  (org-roam-db-autosync-mode))

;; ノート検索のための設定
(use-package deft
  :ensure t
  :custom
  (deft-directory "~/org")
  (deft-extensions '("org"))
  (deft-recursive t))

;; -------------------------------------------------------------------
;; Emacsでターミナル操作をするための設定
;; -------------------------------------------------------------------
(use-package vterm
  :ensure t)

(global-set-key (kbd "C-c t") 'vterm) ; C-c t でターミナルを開く

(setq explicit-shell-file-name "/bin/zsh") ; Zshを使用

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(deft org-roam org-download org-bullets vterm slime-volleyball slime posframe org-modern exec-path-from-shell doom-themes auctex)))

;; Emacs内蔵のシェルを拡張
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
