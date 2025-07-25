;;; setup-denote.el --- Denote configurations -*- lexical-binding: t -*

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

;; Denoteをテスト使用中
(use-package denote
  :ensure t
  :vc (:url "https://github.com/protesilaos/denote" :branch "main" :rev :newest)
  :hook (dired-mode . denote-dired-mode)
  :after org
  :bind
  (("C-c n n" . denote)
   ("C-c n N" . denote-subdirectory)
   ("C-c n r" . denote-rename-file)
   ("C-c n l" . denote-link)
   ("C-c n b" . denote-backlinks)
   ("C-c n d" . denote-sort-dired))
  :config
  (setopt denote-directory "~/Documents")
  ;; ノート作成時にサブディレクトリとタイトルを指定する
  (setopt denote-prompts '(subdirectory title))
  
  (setopt denote-known-keywords
          '("journal" "labnote" "moc" ;; 種類・用途
            "screenshot" "scan" ;; 種類
            "emacs" "math" "linux" "llm" "python"  ;; 話題
            "noexport"  ;; org-mode機能
            ))

  ;; Automatically rename Denote buffers when opening them so that
  ;; instead of their long file name they have, for example, a literal
  ;; "[D]" followed by the file's title.  Read the doc string of
  ;; `denote-rename-buffer-format' for how to modify this.
  (denote-rename-buffer-mode 1)

  ;; orgファイルのidentifierをorg-modeのIDとしても利用可能な書式に変更する
  (setopt denote-org-front-matter
          ":PROPERTIES:
:ID: %4$s
:END:
#+title:      %1$s
#+date:       %2$s
#+filetags:   %3$s
#+signature:  %5$s
\n")

  ;; Documents/YYYY/mm にノートを作るコマンド
  (defun uy/denote-in-documents-current-month ()
    "Create a Denote-format note in `~/Documents/YYYY/mm'."
    (interactive)
    (let ((denote-directory (format-time-string "~/Documents/%Y/%m"))
          (denote-prompts '(title)))
      (call-interactively 'denote)))

  ;; 指定したディレクトリ内で正規表現にマッチするノートを日付降順で一覧表示する
  (defun uy/denote-sort-dired-by-date-descending (denote-dir)
    "Display Denote files in dired sorted by date in descending (newest first) order,
  prompting for the directory."
    (interactive
     (list (read-directory-name "Denote directory: " default-directory)))
    (let ((denote-directory denote-dir))
      (denote-sort-dired
       (denote-files-matching-regexp-prompt)
       'identifier
       t
       nil)))

  ;; ノートを保存時に自動でリネームする https://protesilaos.com/emacs/denote
  (defun my-denote-always-rename-on-save-based-on-front-matter ()
    "Rename the current Denote file, if needed, upon saving the file.
  Rename the file based on its front matter, checking for changes in the
  title or keywords fields.
  
  Add this function to the `after-save-hook'."
    (let ((denote-rename-confirmations nil)
          (denote-save-buffers t)) ; to save again post-rename
      (when (and buffer-file-name (denote-file-is-note-p buffer-file-name))
        (ignore-errors (denote-rename-file-using-front-matter buffer-file-name))
        (message "Buffer saved; Denote file renamed"))))

  (defun my/enable-denote-rename-on-save ()
    (add-hook 'after-save-hook
              #'my-denote-always-rename-on-save-based-on-front-matter
              nil t))
  (add-hook 'denote-rename-buffer-mode-hook #'my/enable-denote-rename-on-save)
    )

(use-package denote-sequence
  :ensure t
  :vc (:url "https://github.com/protesilaos/denote-sequence" :branch "main" :rev :newest)
  :after denote
  :config
  (setopt denote-sequence-scheme 'alphanumeric)
  )

(use-package denote-org
  :ensure t
  :vc (:url "https://github.com/protesilaos/denote-org" :branch "main" :rev :newest)
  :after denote
  :config
  ;; denote-org-extract-org-subtree の動作を変更。見出しにタイムスタンプがあれば使う。
  (defun denote-org--get-heading-date ()
    "Try to return a timestamp for the current Org heading.
This can be used as the value for the DATE argument of the
`denote' command."
    (when-let* ((pos (point))
                (timestamp (or (org-entry-get pos "DATE")
                               (org-entry-get pos "CREATED")
                               (org-entry-get pos "CLOSED")
                               (uy/org--get-date-in-title)  ;; added
                               )))
      (date-to-time timestamp)))

  (defun uy/org--get-date-in-title ()
    """headingからタイムスタンプを抽出する"""
    (let ((heading (org-get-heading t t)))
      (when (string-match (org-re-timestamp 'all) heading)
        (match-string 0 heading))))

  (defun uy/denote-org-extract-org-subtree-to-current-month-dir ()
    (interactive nil org-mode)
    (let ((denote-directory (format-time-string "~/Documents/%Y/%m")))
      (denote-org-extract-org-subtree))
    )
  )

(provide 'setup-denote)
