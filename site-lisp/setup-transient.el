;; -*- lexical-binding: t -*-

;; Transientを使ったUIの設定

;; transient-define-prefixの設定メモ
;; - :variable-pitch t を設定すると日本語を含む場合に表示幅が揃う

(require 'transient)

;; 頻繁に使うコマンドのメニュー
(transient-define-prefix uy/leader-menu ()
  :variable-pitch t
  [
   ["Find"
    ("f" "find file" find-file-at-point)
    ]
   ["Search 🔍"
    ]
   ["Move"
    ("j" "avy-goto-char-2" avy-goto-char-2)
    ("J" "avy-goto-migemo-timer" avy-goto-migemo-timer)
    ]
   ]
  )

;; ;;  <SPC> だと慣れない（誤って動作させてしまう）ので別のキーに変更する 
;; (with-eval-after-load 'evil
;;   (evil-global-set-key 'normal (kbd "<SPC>") 'uy/leader-menu)
;;   (evil-global-set-key 'motion (kbd "<SPC>") 'uy/leader-menu)
;;   )

;; RegisterとBookmark関連のコマンドをまとめたTransientメニュー -----------------

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
(transient-define-prefix uy/register-and-bookmark-tmenu ()
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

(global-set-key (kbd "C-c r") 'uy/register-and-bookmark-tmenu)

(provide 'setup-transient)

