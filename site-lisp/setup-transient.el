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
     (lambda () (interactive) (find-file (uy/journal-file-name-year-month))))
    ("a" "Agenda"
     (lambda () (interactive) (find-file uy/org-agenda-file)))
    ("i" "Shared index"
     (lambda () (interactive) (find-file (file-name-concat org-directory "share-files/02_index_shared.org"))))
    ("h" "Home index"
     (lambda () (interactive) (find-file (file-name-concat org-directory "00_index_home.org"))))
    ("l" "Lab notes"
     (lambda () (interactive) (find-file (file-name-concat org-directory "lab-notes"
                                                           (format-time-string "%Y")
                                                           (format-time-string "%m")))))
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
         ("C-c C-t" . casual-info-tmenu)
         :map calc-mode-map
         ("C-c C-t" . casual-calc-tmenu)))
  :commands
  ;; casual-info非起動時にも使えるようにする
  (casual-info-browse-backward-paragraph casual-info-browse-forward-paragraph) 

  :config
  ;; Info
  (require 'casual-info) ; optional if using autoloaded menu

  ;; Calc
  (require 'casual-calc) ; optional if using autoloaded menu
  )

;; Leader key menu -----------------------
(with-eval-after-load 'evil
  (evil-define-key 'normal 'global (kbd "<SPC>") 'uy/transient-leader-menu))

(transient-define-prefix uy/transient-leader-menu ()
  ["Leader key menu"
   ("a" "App" uy/transient-app-map)
   ("b" "Buffers" consult-buffer)
   ("c" "Capture" uy/transient-capture-map)
   ;; ("d" "Dired" (lambda () (interactive) (dired default-directory)))  ;; open current dir
   ("d" "Dired" dired-at-point)  ;; open current dir
   ("h" "Help" uy/transient-help-map)  ;; defined in Helpful configuration
   ("i" "Insert" uy/transient-insert-map)
   ("f" "File" uy/transient-open-file-menu)
   ("g" "Goto" uy/transient-goto-map)
   ("s" "Search" uy/transient-search-map)
   ("w" "Window" uy/transient-window-map)]
  )

;; Help
(transient-define-prefix uy/transient-help-map ()
   ["Help"
    ("f" "function and command" helpful-callable)
    ("v" "variable" helpful-variable)
    ("k" "key" helpful-key)
    ("x" "command" helpful-command)
    ("." "at-point" helpful-at-point)])

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
    ("D" "rg dir file-pattern" uy/consult-ripgrep-in-directory)
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

;; Insert key map
(transient-define-prefix uy/transient-insert-map ()
   ["Insert"
    ("t" "timestamp" uy/insert-timestamp)
    ("y" "yasnippet" yas-insert-snippet)]
  )
(global-set-key (kbd "C-c i") 'uy/transient-insert-map)

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

;; org-mode capture key map
;; menu to access org[-roam]-capture and custom note-creation function
(transient-define-prefix uy/transient-capture-map ()
  ["Org-mode capture"
   ("j" "Journal" (lambda () (interactive) (org-capture nil "j")))
   ("l" "Lab note" uy/create-new-lab-note)
   ("w" "Work" (lambda () (interactive) (org-roam-capture nil "w")))
   ("s" "Share" (lambda () (interactive) (org-roam-capture nil "s")))
   ]
  )

(provide 'setup-transient)

