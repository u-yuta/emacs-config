;;; init.el --- Emacs init -*- lexical-binding: t -*-

;; Copyright (C) 2025 u-yuta
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;; ============================================
;; use-package設定
;; ============================================

(setopt use-package-enable-imenu-support t) ;; Must be set before loading `use-package'
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; ============================================
;; 基本設定
;; ============================================

;; 起動時に scratch buffer を表示
(setopt initial-buffer-choice t)
(setopt initial-major-mode 'lisp-interaction-mode)
(setopt initial-scratch-message "")

;; 自動リバート
(global-auto-revert-mode 1)

;; ファイル/バッファ操作
(setopt confirm-kill-emacs 'yes-or-no-p)
(setopt set-mark-command-repeat-pop t)

;; 自動保存/バックアップ設定
(let ((target-dir (expand-file-name "~/"))
      (dest-dir (expand-file-name "~/.Trash/")))
  ;; 自動保存ファイル(#*#)の作成先変更
  (add-to-list 'auto-save-file-name-transforms
               `(,(concat target-dir "\\([^/]*/\\)*\\([^/]*\\)$")
                 ,(concat dest-dir "\\2")
                 t))
  ;; バックアップファイル(*~)の作成先変更
  (add-to-list 'backup-directory-alist (cons target-dir dest-dir))
  ;; 自動保存リスト(.saves-<PID>-<HOSTNAME>)の作成先変更
  (setopt auto-save-list-file-prefix (expand-file-name ".saves-" dest-dir)))

;; custom.el 設定
(setopt custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; サーバー設定
(require 'server)
(unless (server-running-p)
  (server-start))

;; グローバルなキーバインドの設定
(global-set-key (kbd "<home>") 'beginning-of-buffer)
(global-set-key (kbd "<end>") 'end-of-buffer)
(global-set-key (kbd "C-a") 'back-to-indentation-or-beginning)

(global-set-key (kbd "M-h") 'backward-kill-word)
(global-set-key (kbd "C-c h") 'help-command)
(global-unset-key (kbd "C-x C-z"))  ;; disable suspend-frame

;; モードごとの設定より優先して設定
(bind-key* "C-h" 'backward-delete-char-untabify)

;; use built-in which-key
(which-key-mode)

;; load-path を追加
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))

;; システム判定
(setopt uy/system-linux-p (string-match-p "Linux" (shell-command-to-string "uname -o")))
(setopt uy/system-msys-p (string-match-p "Msys" (shell-command-to-string "uname -o")))
(setopt uy/system-windows-p (eq system-type 'windows-nt))
(setopt uy/os-text (cond
                  (uy/system-linux-p "Linux🐧")
                  (uy/system-msys-p "Msys")
                  (uy/system-windows-p "Windows🖥️")))
;; WSL上かどうかの判定
(setopt uy/wsl-p
      (and (string-match-p "WSL" (shell-command-to-string "uname -r"))
           (eq system-type 'gnu/linux)))


;; シェル設定
(if uy/system-msys-p
    (setopt shell-file-name "/usr/bin/bash"))
(if uy/system-linux-p
    (setopt shell-file-name "/bin/bash"))
(setopt explicit-shell-file-name shell-file-name)

;; ============================================
;; UI/外観設定
;; ============================================

;; ツールバー/メニューバー設定
(tool-bar-mode 0)
(menu-bar-mode 1)  ;; メニューは残しておく。Ctrl-右クリックでも表示できる。
(tab-bar-mode 1)
(context-menu-mode 1)  ;; 右クリックでコンテクストメニューを開く
(desktop-save-mode 1)  ;; セッションを保存する
;; セッション保存対象から除外
(setopt desktop-modes-not-to-save 
        '(compilation-mode dired-mode tags-table-mode help-mode
          eww-mode grep-mode occur-mode
          magit-mode magit-log-mode))
(setopt desktop-buffers-not-to-save "^\*.+\*$")

;; モードライン設定
(setopt frame-title-format
      '(multiple-frames "%b"
                        (" " invocation-name "@" system-name " " uy/os-text
                         (:eval (if (buffer-file-name) " %f" " %b")))))

;; 行番号表示
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; 外観設定
(require 'setup-appearance)

;; ============================================
;; 編集支援
;; ============================================

(delete-selection-mode 1)

;; 基本編集設定
(setq-default indent-tabs-mode nil)
;; (setq-default tab-width 4)  ;; Org 9.7 以降ではtab-widthが8以外になっているとエラーになる
(setq-default fill-column 80)
(setopt case-fold-search nil)

;; 括弧/引用符
(electric-pair-mode 1)
(setopt electric-pair-preserve-balance nil)

;; 警告音/ベル
(setopt visible-bell t)

;; スクロール設定
(setopt scroll-conservatively 1)
(setopt scroll-margin 5)
(setopt next-screen-context-lines 1)
(setopt scroll-preserve-screen-position nil)

(defun uy/indent-defun ()
  "Indent the current defun."
  (interactive)
  (save-excursion
    (mark-defun)
    (indent-region (region-beginning) (region-end))))

(defun uy/insert-timestamp ()
  "Insert timestamp"
  (interactive)
  (insert (format-time-string "%Y%m%dT%H%M%S")))


;; ============================================
;; 標準モジュールの設定
;; ============================================

;; info-mode設定
(use-package info
  :ensure nil
  :config
  ;; 以下は casual-info を参考にしたキーバインド
  ;; Bind h and l to navigate to previous and next nodes
  ;; Bind j and k to navigate to next and previous references
  (keymap-set Info-mode-map "h" #'Info-prev)
  (keymap-set Info-mode-map "j" #'Info-next-reference)
  (keymap-set Info-mode-map "k" #'Info-prev-reference)
  (keymap-set Info-mode-map "l" #'Info-next)
  ;; Bind / to search
  (keymap-set Info-mode-map "/" #'Info-search)
  ;; Set Bookmark
  (keymap-set Info-mode-map "B" #'bookmark-set)
  ;; Use web-browser history navigation bindings
  (keymap-set Info-mode-map "M-<left>" #'Info-history-back)
  (keymap-set Info-mode-map "M-<right>" #'Info-history-forward)

  ;; Bind p and n to paragraph navigation
  ;; casual-info にある関数を使用する
  (keymap-set Info-mode-map "p" #'casual-info-browse-backward-paragraph)
  (keymap-set Info-mode-map "n" #'casual-info-browse-forward-paragraph)

  (add-hook 'Info-mode-hook #'hl-line-mode)  ;; カーソル行をハイライト
  (add-hook 'Info-mode-hook #'scroll-lock-mode)  ;; 矢印キーでスクロール
  )

(use-package browse-url
  :ensure nil
  :config
  (setopt browse-url-browser-function 'browse-url-generic)
  (when uy/wsl-p (setopt browse-url-generic-program "wslview"))
  (when uy/system-windows-p
    (setopt browse-url-browser-function 'browse-url-default-browser))
  ;; (global-set-key (kbd "C-c u") 'browse-url-at-point)
  )

;; ibuffer
(use-package ibuffer
  :ensure nil
  :bind (("C-x C-b" . ibuffer)) ;; 標準の list-buffers の代わりに ibuffer を使用
  :init
  ;; グルーピング
  (setopt ibuffer-saved-filter-groups
          (quote (("default"
                   ("Org" ;; all org-related buffers
                    (mode . org-mode))
                   ("Dired" (mode . dired-mode))
                   ("Emacs Lisp"   (mode . emacs-lisp-mode))
                   ("Emacs" (or
                             (name . "^\\*scratch\\*$")
                             (name . "^\\*Messages\\*$")
                             (name . "^\\*Warnings*\\*$")
                             (name . "^\\*info*\\*$")
                             (name . "^\\*Help*\\*$")
                             ))
                   ("Python" (or (mode . python-mode) (mode . python-ts-mode)))
                   ("Special Buffers"
                    (or
                     (mode . magit-status-mode)
                     (mode . magit-process-mode)
                     (mode . magit-mode)
                     (mode . magit-diff-mode)
                     (mode . ediff-mode)
                     (mode . jupyter-repl-mode)))
                   ))))

  :config
  (add-hook 'ibuffer-mode-hook
            (lambda ()
              (ibuffer-switch-to-saved-filter-groups "default")))
  )

;; recentf
(use-package recentf
  :config
  (setopt recentf-max-saved-items 1000)
  (setopt recentf-max-menu-items 500)
  (recentf-mode 1)
  (run-at-time nil (* 5 60) 'recentf-save-list)
  )

;; dired
(require 'setup-dired)

;; project
(use-package project
  :ensure nil
  ;; Cannot use :hook because 'project-find-functions does not end in -hook
  ;; Cannot use :init (must use :config) because otherwise
  ;; project-find-functions is not yet initialized.
  :config
  (setopt project-vc-extra-root-markers '(".project.el"))  ;; Emacs 29.1以降で有効
  )

;; windmove
;; Windowを切り替える
(use-package windmove
  :ensure nil
  :config
  ;; Ctrl+arrow でwindmove
  (windmove-default-keybindings 'ctrl))

;; repeat-mode
;; コマンドのリピート機能を有効にする
;; 対応しているコマンドを確認するには `M-x describe-repeat-maps'
(use-package repeat
  :config
  (repeat-mode))

;; ============================================
;; 開発支援
;; ============================================

;; Eglot
(use-package eglot
  :ensure nil
  :config
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode)
                 "basedpyright-langserver" "--stdio")) ;; use `basedpyright'
  ;; Disable inlay hints
  (setopt eglot-ignored-server-capabilities '(:inlayHintProvider))
  )

;; Tree-sitter
;; 言語ごとの構文をインストールする手順は `site-lisp/init-treesitter.el' を参照。
(use-package treesit
  :config
  (setopt treesit-font-lock-level 4))

;; magit
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status))
  :config
  (remove-hook 'server-switch-hook 'magit-commit-diff)
  ;; logの日時表示フォーマット設定
  (setopt magit-log-margin '(t "%Y-%m-%d %H:%M:%S" magit-log-margin-width t 18)))


;; treesitter
(require-if-exists init-treesitter)

;; ============================================
;; 日本語環境、日本語入力
;; ============================================

(require 'setup-japanese)

;; ============================================
;; Org-mode関連
;; ============================================

;; org-mode設定
(require 'setup-org-mode)

;; org-roam設定
(require 'setup-org-roam)

(use-package htmlize :ensure t)

;; 文献引用、citationの選定
(require 'setup-citation)

;; org-noter設定
(use-package org-noter
  :ensure t
  :defer t
  :config
  (setopt org-noter-notes-search-path (list (expand-file-name "share" org-roam-directory)))
  (setopt org-noter-default-notes-file-names '("resouce.notes.org"))
  )
;; ============================================
;; Windows, WSL関連
;; ============================================

;; Windows パス と UNC パス を使えるようにするための設定 (WSL 用)
(when uy/wsl-p (require-if-exists windows-path-on-wsl))

;; ============================================
;; プログラム、マークアップ言語関連
;; ============================================

(require 'setup-python)

;; markdown-mode
(use-package markdown-mode
  :ensure t
  :mode
  ("README\\.md\\'" . gfm-mode)
  :init
  (setopt markdown-command '("pandoc" "--from=markdown" "--to=html5"))
  (setopt markdown-asymmetric-header t)
  )

;; YAML
(use-package yaml-mode :ensure t :defer t)

;; Lua
(use-package lua-mode :ensure t :defer t)

;; Major mode for editing and running Microsoft PowerShell files
(use-package powershell :ensure t)

;; Nix
(use-package nix-mode
  :ensure t
  :mode "\\.nix\\'")

;; ============================================
;; LaTeX
;; ============================================

;; AUCTeX (LaTeX 編集環境)
(use-package auctex
  :ensure t
  :defer t
  :hook ((LaTeX-mode . TeX-source-correlate-mode)
         (LaTeX-mode . TeX-PDF-mode))
  :config
  ;; デフォルトのTeXエンジンをLuaTeXに設定
  (setopt TeX-engine 'luatex)
  (setopt TeX-command-default "LuaLaTeX")
  ;; LuaLaTeXのコンパイルコマンドを追加
  (add-to-list 'TeX-command-list
               '("LuaLaTeX" "lualatex -shell-escape -interaction=nonstopmode %s"
                 TeX-run-TeX nil t :help "Run LuaLaTeX"))
  ;; PDFビューアの設定（wslviewを使用）
  (setopt TeX-view-program-selection '((output-pdf "WSLView")))
  (setopt TeX-view-program-list '(("WSLView" "wslview %o"))))

;; CDLaTeX (LaTexでの数式入力の補助)
(use-package cdlatex
  :ensure t
  :hook (LaTeX-mode . turn-on-cdlatex) ;; LaTeXモードでCDLaTeXを有効化
  :config
  (setopt cdlatex-sub-super-scripts-outside-math-mode nil)
  ;; 数式モードでCDLaTeXを有効化する
  (add-hook 'LaTeX-mode-hook 'cdlatex-mode))

;; ============================================
;; その他の設定
;; ============================================

;; FFAP設定
(ffap-bindings)

;; Back to indentation, or beginning of line.
(defun back-to-indentation-or-beginning ()
   (interactive) 
   (if (bolp) (back-to-indentation) (beginning-of-line)))

;; avy
;; 参考：文字入力後、ターゲット選択前に `?' を入力する、選択可能なAction(kill, copy, zapなど)が表示される
(use-package avy
  :ensure t
  :bind
  (("M-j" . avy-goto-char-timer)
   ("M-g g" . avy-goto-char-2)
   ("M-g M-g" . avy-goto-line))
  :commands (avy-goto-char-timer)
  :config
  
  ;; isearchの候補をavyで選択する
  (define-key isearch-mode-map (kbd "M-j") 'avy-isearch)

  ;; Avy候補表示中に選択可能なアクションの設定
  (defun avy-action-copy-whole-line (pt)
    "選択候補を含む行全体を選択する avy-action"
    (save-excursion
      (goto-char pt)
      (cl-destructuring-bind (start . end)
          (bounds-of-thing-at-point 'line)
        (copy-region-as-kill start end)))
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    t)

  (defun avy-action-yank-whole-line (pt)
    (avy-action-copy-whole-line pt)
    (save-excursion (yank))
    t)

  ;; 
  (setf (alist-get ?w avy-dispatch-alist) 'avy-action-copy
        (alist-get ?y avy-dispatch-alist) 'avy-action-yank
        (alist-get ?W avy-dispatch-alist) 'avy-action-copy-whole-line
        (alist-get ?Y avy-dispatch-alist) 'avy-action-yank-whole-line)

  ;; AvyをEmbarkと組み合わせる
  ;; 1. Avyで候補を表示した状態で `.` で embark-act を起動してアクションを選択
  ;; 2. Avy のキーで候補を選択すると、選択したアクションが実行される
  ;; 3. カーソル位置はAvy実行前に戻る
  (defun avy-action-embark (pt)
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)
  (setf (alist-get ?. avy-dispatch-alist) 'avy-action-embark) ;; `.` に割当
  )

;; Evil
(require 'setup-evil)  ;; in site-lisp

;; Denote
(require 'setup-denote)

;; Note search
(require 'setup-note-search)

;; ミニバッファ周りの設定 vertico, consult (embarkも)
(require 'setup-vertico-consult)  ;; in site-lisp

;; direnv: buffer-local direnv integration
(use-package envrc
  :ensure t
  :hook (after-init . envrc-global-mode))

;; wgrep allows you to edit a grep buffer and apply those changes to the file
;; buffer like sed interactively.
(use-package wgrep :ensure t)

(use-package rg
  ;; ripgrep をインストールしておく。
  :ensure t
  :bind ("C-c s g" . rg-menu)
  )

(use-package visual-regexp
  :ensure t
  :defer t
  :bind (("C-c e r" . vr/query-replace)
         ("M-r" . vr/query-replace)))

(use-package expand-region
  :ensure t
  :bind ("C-;" . er/expand-region))

(use-package yasnippet
  :ensure t
  :custom (yas-snippet-dirs '("~/.emacs.d/snippets"))
  :bind (:map yas-minor-mode-map
              ("C-c y i" . yas-insert-snippet)
              ("C-c y n" . yas-new-snippet)
              ("C-c y v" . yas-visit-snippet-file)
              ("C-c y l" . yas-describe-tables)
              ("C-c y g" . yas-reload-all))
  :hook (after-init . yas-global-mode)
  )

(use-package yasnippet-snippets
  :ensure t
  :vc (:url "https://github.com/u-yuta/yasnippet-snippets" :branch "main" :rev :newest)
  :after yasnippet
  :config
  (setopt yas-snippet-dirs (append yas-snippet-dirs (list yasnippet-snippets-dir))))

(require 'setup-completion)

;; pdf-tools
(use-package pdf-tools
  :ensure t
  :defer t
  :config
  ;; initialize
  (pdf-tools-install)
  (pdf-loader-install))

;; visual undo-tree
(use-package vundo
  :ensure t
  :config
  ;; Take less on-screen space.
  (setopt vundo-compact-display t))

(use-package simple-httpd :ensure t :defer t)

;; Eat: Emulate A Terminal
(use-package eat :ensure t :defer t)

;; vterm
(use-package vterm
    :ensure t)

(when uy/wsl-p 
  (use-package notmuch
    :commands notmuch-hello)
  )

(require 'setup-auth-source)
(require 'setup-ai)
(require 'setup-helpful)

;; setup transient menu
(require 'setup-transient)
(require 'pandoc-menu)
(require 'docdb)


;; Load host-specific configuration
(defun load-file-if-exists-in-user-emacs-directory (filename)
  "Load a file named FILENAME from `user-emacs-directory` if it exists."
  (let ((filepath (expand-file-name filename user-emacs-directory)))
    (if (file-exists-p filepath)
        (progn
          (load filepath)
          (message "Loaded file: %s" filepath))
      (message "File does not exist: %s" filepath))))

(defun get-init-file-for-host ()
  "Return a string 'init-host-<hostname>.el' where <hostname> is obtained from `system-name`."
  (let ((hostname (car (split-string system-name "\\.")))) ;; ドメイン部分を除去
    (format "init-host-%s.el" hostname)))

(load-file-if-exists-in-user-emacs-directory (get-init-file-for-host))
