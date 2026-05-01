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
   ("C-c n r" . denote-rename-file))
  :config
  (setopt denote-directory "~/Documents")
  ;; ノート作成時にサブディレクトリとタイトルを指定する
  (setopt denote-prompts '(subdirectory title))
  
  (setopt denote-known-keywords
          ;; 役割で分ける
          '("ref"   ;; 理解のための情報（読むもの）
            "data"  ;; 計算・再現のための入力
            "work"  ;; 処理・中間状態       
            "note"  ;; 解釈・知識
            "index" ;; 構造・接続
            ))

  ;; title の slug 規則は Denote 標準をほぼ踏襲し、差分は 2 点のみ:
  ;; 1) downcase しない（大文字小文字を保持）
  ;; 2) . は保持する（後段の共通処理で消さない）
  (defun my/denote-sluggify-title (str)
    "Make STR an appropriate slug for title.
Compared to Denote defaults, this keeps letter case (no downcase)
and is paired with custom post-processing that preserves dots."
    (denote-slug-hyphenate
     (replace-regexp-in-string
      "[][{}!@#$%^&*()+'\"?,\|;:~`‘’“”/=]*"
      ""
      str)))

  (defun my/denote-sluggify-and-apply-rules (component str)
    "Make STR an appropriate slug for file name COMPONENT.
This follows Denote's standard machinery, except that dots are kept
for the title component."
    (let* ((slug-function (alist-get component denote-file-name-slug-functions))
           (str-slug (cond
                      ((eq component 'title)
                       (funcall (or slug-function #'denote-sluggify-title) str))
                      ((eq component 'keyword)
                       (replace-regexp-in-string
                        "_"
                        ""
                        (funcall (or slug-function #'denote-sluggify-keyword) str)))
                      ((eq component 'identifier)
                       (denote--valid-identifier
                        (funcall (or slug-function #'identity) str)))
                      ((eq component 'signature)
                       (funcall (or slug-function #'denote-sluggify-signature) str)))))
      (denote--trim-right-token-characters
       (denote--replace-consecutive-token-characters
        (if (eq component 'title)
            str-slug
          (denote--remove-dot-characters str-slug))
        component)
       component)))

  (setopt denote-file-name-slug-functions
          '((identifier . identity)
            (title . my/denote-sluggify-title)
            (signature . denote-sluggify-signature)
            (keyword . denote-sluggify-keyword)))

  ;; Denote 標準では後段で . を消すため、title だけ . を残すように差し替える。
  (advice-add 'denote-sluggify-and-apply-rules :override
              #'my/denote-sluggify-and-apply-rules)

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
  (defun my/denote-in-documents-current-month ()
    "Create a Denote-format note in `~/Documents/YYYY/mm'."
    (interactive)
    (let ((denote-directory (format-time-string "~/Documents/%Y/%m"))
          (denote-prompts '(title)))
      (call-interactively 'denote)))

  ;; 指定したディレクトリ内で正規表現にマッチするノートを日付降順で一覧表示する
  (defun my/denote-sort-dired-by-date-descending (denote-dir)
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
      (when (and buffer-file-name (denote-file-has-denoted-filename-p buffer-file-name))
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
                               (my/org--get-date-in-title)  ;; added
                               )))
      (date-to-time timestamp)))

  (defun my/org--get-date-in-title ()
    """headingからタイムスタンプを抽出する"""
    (let ((heading (org-get-heading t t)))
      (when (string-match (org-re-timestamp 'all) heading)
        (match-string 0 heading))))

  (defun my/denote-org-extract-org-subtree-to-current-month-dir ()
    (interactive nil org-mode)
    (let ((denote-directory (format-time-string "~/Documents/%Y/%m")))
      (denote-org-extract-org-subtree))
    )
  )

(provide 'setup-denote)
