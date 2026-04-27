;;; setup-org-roam.el --- Org-roam configurations -*- lexical-binding: t -*

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


(use-package org-roam
  :ensure t
  :init (setopt org-roam-v2-ack t)
  :after (org)
  :custom
  (org-roam-directory org-directory)
  :bind (
         (("C-c n f" . org-roam-node-find)
          ("C-c n g" . org-roam-graph)
          ("C-c n b" . org-roam-buffer-toggle)
          ("C-c n c" . org-roam-capture)
          ("C-c n d" . org-roam-dailies-capture-today)
          )
         :map org-mode-map
         (("C-c n i" . org-roam-node-insert))
         (("C-c n I" . org-roam-insert-immediate)))
  :config
  ;; Configures display formatting for Org-roam node.
  ;; https://github.com/org-roam/org-roam/wiki/User-contributed-Tricks#filtering-by-subdirectory
  (cl-defmethod org-roam-node-directories ((node org-roam-node))
    (if-let ((dirs (file-name-directory (file-relative-name (org-roam-node-file node) org-roam-directory))))
        (format "%s/" (car (split-string dirs "/")))
      ""))
  (setopt org-roam-node-display-template "${directories:12} ${title:80} ${tags:10}"
          org-roam-node-annotation-function
          (lambda (node) (marginalia--time (org-roam-node-file-mtime node))))

  ;; Customize slug generation: spaces and non-alphanumeric chars become "-" instead of "_"
  (cl-defmethod org-roam-node-slug ((node org-roam-node))
    "Return the slug of NODE."
    (let ((title (org-roam-node-title node)))
      (require 'ucs-normalize)
      (let ((slug-trim-chars
             '(#x300 #x301 #x302 #x303 #x304 #x306 #x307
               #x308 #x309 #x30A #x30B #x30C #x31B #x323
               #x324 #x325 #x327 #x32D #x32E #x330 #x331)))
        (thread-last title
                     (ucs-normalize-NFD-string)
                     (seq-remove (lambda (char) (memq char slug-trim-chars)))
                     (apply #'string)
                     (ucs-normalize-NFC-string)
                     (replace-regexp-in-string "[^[:alnum:]]" "-")
                     (replace-regexp-in-string "--*" "-")
                     (replace-regexp-in-string "^-" "")
                     (replace-regexp-in-string "-$" "")
                     (downcase)))))

  ;; org-roam capture template
  (setopt org-roam-capture-templates
        '(("d" "default" plain "%?" :target
           (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}")
           :unnarrowed t)
          ))
  
  (setopt org-roam-file-extensions '("org" "md")) ;; enable Org-roam for markdown

  (use-package md-roam
    :ensure t
    :vc (:url "https://github.com/nobiot/md-roam" :branch "main" :rev :newest)
    :config
    (md-roam-mode 1)  ;; needs to be turned on before `org-roam-db-autosync-mode'
    (setopt md-roam-file-extension "md")
    (setopt md-roam-regex-id "\\(^identifier:[ \t]*\\)\\(.*\\)")  ;; DenoteのIDフォーマット

    ;; `yyyymmddTHHMMSS--' で始まるファイルを検索対象とする（Denoteと共通化）
    (defun md-roam--markdown-file-p (file)
      "FILE が md-roam の処理対象（指定の拡張子かつタイムスタンプ開始）か判定する。"
      (when (stringp file)
        (let ((ext-re (format "\\.%s$" (regexp-quote md-roam-file-extension)))
              ;; タイムスタンプ形式 (例: 20231027T103000--...) の正規表現
              (name-re "^[0-9]\\{8\\}T[0-9]\\{6\\}--")
              (filename (file-name-nondirectory file)))
          (and (string-match-p ext-re file)      ; 拡張子のチェック
               (string-match-p name-re filename) ; ファイル名の先頭チェック
               ))))
    )
    (org-roam-db-autosync-mode 1)
  )

(use-package org-roam-ui
  :ensure t
  :defer t
  :after org-roam
  ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
  ;;         a hookable mode anymore, you're advised to pick something yourself
  ;;         if you don't care about startup time, use
  ;;  :hook (after-init . org-roam-ui-mode)
  :config
  (setopt org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(use-package org-ql
  :ensure t
  :after org-roam  ;; org-roam-list-files を使っている
  :config
  (setopt org-ql-warn-no-heading nil)

  (defun my/find-org-entries-by-heading-in-directory ()
    "Find org entries with specific heading text in all org files in a directory."
    (interactive)
    (let* ((directory (read-directory-name "Search org files in directory: "))
           (heading-text (read-string "Heading contains: "))
           (org-files (directory-files-recursively directory "\\.org$")))
      (if org-files
          (org-ql-find org-files
                       :query-prefix (format "heading:\"%s\" " heading-text)
                       :prompt (format "Entries with \"%s\" in heading: " heading-text)
                       :widen t)
        (message "No org files found in %s" directory)))))

(provide 'setup-org-roam)
