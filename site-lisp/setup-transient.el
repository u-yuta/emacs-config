;;; setup-transient.el --- Transient menu UI configurations -*- lexical-binding: t -*

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

;; Transientを使ったUIの設定

;; transient-define-prefixの設定メモ
;; - :variable-pitch t を設定すると日本語を含む場合に表示幅が揃う

(require 'transient)

;; よく使うファイルを開く
(transient-define-prefix uy/transient-open-file-menu ()
  ["Find file"
   ["File"
    ("j" "Journal"
     (lambda () (interactive) (find-file (file-name-concat org-directory "journal/journal.org"))))
    ("a" "Agenda"
     (lambda () (interactive) (find-file (file-name-concat org-directory "journal/agenda.org"))))
    ("i" "Shared index"
     (lambda () (interactive) (find-file (file-name-concat org-directory "share/02_index_shared.org"))))
    ("h" "Home index"
     (lambda () (interactive) (find-file (file-name-concat org-directory "00_index_home.org"))))
    ("l" "Lab notes"
     (lambda () (interactive) (find-file (file-name-concat org-directory "lab-notes"
                                                           (format-time-string "%Y")
                                                           (format-time-string "%m")))))
    ("o" "Office index"
     (lambda () (interactive) (find-file (file-name-concat org-directory "01_index_office.org"))))
    ("." "Emacs init"
     (lambda () (interactive) (find-file user-init-file)))
   ]
   ["Command"
    ("f" "find-file" find-file-at-point)
    ("c" "locate" consult-locate)
    ("p" "project" project-find-file)
    ("r" "recent" recentf)
    ]
   ]
  )

(global-set-key (kbd "C-c f") 'uy/transient-open-file-menu)

;; TransientベースのUIを追加する ----------------------------------------------
(use-package casual
  :ensure t
  :bind
  ((:map Info-mode-map
         ("C-c t" . casual-info-tmenu)
         :map calc-mode-map
         ("C-c t" . casual-calc-tmenu)
         :map calc-mode-map
         ("C-c t" . casual-calc-tmenu)))
  :commands
  ;; casual-info非起動時にも使えるようにする
  (casual-info-browse-backward-paragraph casual-info-browse-forward-paragraph) 

  :config
  ;; Info
  (require 'casual-info) ; optional if using autoloaded menu

  ;; Calc
  (require 'casual-calc) ; optional if using autoloaded menu
  )

;; RegisterとBookmark関連のコマンドをまとめたTransientメニュー -----------------
(transient-define-prefix uy/transient-register-and-bookmark-menu ()
  :variable-pitch t
  ["Registers and Bookmarks\n"
   ["Resister"
    ("s" "copy-to-register リージョンをコピー" copy-to-register :transient nil)
    ("i" "insert-register 貼り付け" insert-register :transient nil)
    ("<SPC>" "point-to-register カーソル位置を登録" point-to-register :transient nil)
    ("j" "jump-to-register 登録位置へジャンプ" point-to-register :transient nil)]
   ["Bookmark"
    ("m" "bookmark-set 登録" bookmark-set :transient nil)
    ("b" "bookmark-jump ジャンプ" bookmark-jump :transient nil)
    ("l" "bookmark-bmenu-list ブックマークの一覧表示" list-bookmarks :transient nil)  ;; なぜか表示されない
    ] 
   ]
  )

(global-set-key (kbd "C-c r") 'uy/transient-register-and-bookmark-menu)

;; Evil Leader -----------------------
(with-eval-after-load 'evil
  (evil-set-leader (list 'normal 'visual) (kbd "SPC"))
  (evil-define-key 'normal 'global (kbd "<leader>a") 'uy/transient-app-map)
  (evil-define-key 'normal 'global (kbd "<leader>b") 'consult-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>h") 'help-command)
  (evil-define-key 'normal 'global (kbd "<leader>f") 'uy/transient-open-file-menu)
  (evil-define-key 'normal 'global (kbd "<leader>g") 'uy/transient-goto-map)
  (evil-define-key 'normal 'global (kbd "<leader>s") 'uy/transient-search-map)
  (evil-define-key 'normal 'global (kbd "<leader>w") 'uy/transient-window-map)
  )

;; Applications
(transient-define-prefix uy/transient-app-map ()
   ["AI"
    ("d" "aider" aidermacs-transient-menu)
    ("g" "gptel" gptel-menu)]
   ["Others"
    ("t" "vterm" vterm)]
  )

;; Goto key map
(transient-define-prefix uy/transient-goto-map ()
   ["Goto"
    ("e" "compile-error" consult-compile-error)
    ("f" "flymake" consult-flymake)
    ("g" "goto-line" consult-goto-line)
    ("j" "goto-char-timer" avy-goto-migemo-timer)
    ("o" "outline" consult-outline)
    ("m" "mark" consult-mark)
    ("k" "global mark" consult-global-mark)
    ("i" "imenu" consult-imenu)
    ("I" "imenu-multi" consult-imenu-multi)]
  )

;; Search key map
(transient-define-prefix uy/transient-search-map ()
  ["Search"
   ["In files"
    ("g" "grep" consult-grep)
    ("G" "Git grep" consult-git-grep)
    ("r" "Ripgrep" consult-ripgrep)
    ("l" "line" consult-line)
    ("L" "line-multi" consult-line-multi)
    ("R" "org-roam-ripgrep" bms/org-roam-rg-search)]
   ["Others"
    ("h" "isearch-history" consult-isearch-history)
    ]]
  )

;; Window key map
(transient-define-prefix uy/transient-window-map ()
  ["Window"
   ["Manipulate"
    ("c" "delete" evil-window-delete)
    ("o" "delete other" delete-other-windows)
    ("x" "exchange" evil-window-exchange)
    ("v" "split vertically" evil-window-vsplit)
    ("s" "split" evil-window-split)
    ("q" "quit" evil-quit)
    ]
   ["Goto"
    ("h" "left" evil-window-left)
    ("j" "down" evil-window-down)
    ("k" "up" evil-window-up)
    ("l" "right" evil-window-right)
    ("p" "previous (recent)" evil-window-mru)
    ("w" "next" evil-window-next)
    ]
   ["Size"
    ("+" "increase height" evil-window-increase-height :transient t)
    ("-" "decrease height" evil-window-decrease-height :transient t)
    (">" "increase width" evil-window-increase-width :transient t)
    ("<" "decrease width" evil-window-decrease-width :transient t)
    ("=" "balance" balance-windows)
    ]
   ]
  )

(provide 'setup-transient)

