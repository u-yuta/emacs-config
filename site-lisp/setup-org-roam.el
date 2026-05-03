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
  :commands org-roam-dailies-goto-today  ;; workaround for transient error: `already defined as something else than a generic function'
  :custom
  (org-roam-directory org-directory)
  :bind (
         (("C-c n f" . my/org-roam-node-find)
          ("C-c n g" . org-roam-graph)
          ("C-c n b" . org-roam-buffer-toggle)
          ("C-c n c" . my/org-roam-capture)
          ("C-c j" . org-roam-dailies-goto-today)
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

  ;; ノード作成時にIDのタイムスタンプとファイル名のタイムスタンプを一致させる
  (defvar my/org-roam-capture-timestamp nil)

  (defun my/org-roam-capture-timestamp ()
    "Return one timestamp reused within the current capture."
    (or my/org-roam-capture-timestamp
        (setq my/org-roam-capture-timestamp
              (format-time-string "%Y%m%dT%H%M%S"))))

  (defun my/org-roam-node-find ()
    "Run org-roam-node-find with a fresh shared timestamp."
    (interactive)
    (let ((my/org-roam-capture-timestamp nil))
      (call-interactively #'org-roam-node-find)))

  (defun my/org-roam-capture ()
    "Run org-roam-node-find with a fresh shared timestamp."
    (interactive)
    (let ((my/org-roam-capture-timestamp nil))
      (call-interactively #'org-roam-capture)))

  ;; org-roam capture template
  (setopt org-roam-capture-templates
          '(
            ("t" "task note" plain "%?" :target
             (file+head "%(format-time-string \"~/Documents/%Y/%m/\")%(my/org-roam-capture-timestamp)--${slug}.org"  ":PROPERTIES:
:ID: %(my/org-roam-capture-timestamp)
:STATUS: %^{STATUS|active|hold|done|archived}
:END:
#+title:      ${title}
#+filetags:   :task:
") :unnarrowed t)
            ("p" "project note" plain "%?" :target
             (file+head "%(format-time-string \"~/Documents/%Y/%m/\")%(my/org-roam-capture-timestamp)--${slug}.org"  ":PROPERTIES:
:ID: %(my/org-roam-capture-timestamp)
:CONTEXT_TYPE: project
:ROAM_ALIASES: %^{ROAM_ALIASES}
:END:
#+title:      ${title}
#+filetags:   :index:
") :unnarrowed t)
            ("a" "area note" plain "%?" :target
             (file+head "%(format-time-string \"~/Documents/%Y/%m/\")%(my/org-roam-capture-timestamp)--${slug}.org"  ":PROPERTIES:
:ID: %(my/org-roam-capture-timestamp)
:CONTEXT_TYPE: area
:ROAM_ALIASES: %^{ROAM_ALIASES}
:END:
#+title:      ${title}
#+filetags:   :index:
") :unnarrowed t)
            ))

  ;; org-roam dailies template
  (setopt org-roam-dailies-directory my/journal-directory)
  (setopt org-roam-dailies-capture-templates
          '(("j" "journal" entry ""
             :target (file+head "%<%Y/journal-%Y%m%d>.org"
                                "#+title: %<%Y-%m-%d %A>\n#+filetags: :journal:\n\n* 予定\n\n* やること\n\n* タスクキュー\n\n* メモ\n\n* 日報\n\n")
             :unnarrowed t)))
  
  ;; Customize slug generation: spaces and non-alphanumeric chars become "-" instead of "_"
  ;; `org-roam-node-slug' を `cl-defmethod' で再定義するため、
  ;; org-roam-node が generic 関数として定義された後に評価する。
  (require 'org-roam-node)
  (with-eval-after-load 'org-roam-node
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
                       (downcase))))))

  (defun my/org-store-file-id-link ()
    "現在位置によらずファイルレベル ID リンクを Org の保存済みリンクに登録する。"
    (interactive)
    (require 'org-id)
    (require 'ol)

    (unless (derived-mode-p 'org-mode)
      (user-error "Not in org-mode"))
    (unless buffer-file-name
      (user-error "This buffer is not visiting a file"))

    (org-with-wide-buffer
     (save-excursion
       (goto-char (point-min))

       ;; 先頭が見出しなら、見出しの前にファイルレベル property drawer を作る
       (when (eq (car-safe (org-element-at-point)) 'headline)
         (insert ":PROPERTIES:\n:END:\n")
         (goto-char (point-min)))

       ;; point-min がファイルレベル領域になっている前提で ID を作る
       (let* ((id (org-id-get-create))
              (link (format "id:%s" id))
              (desc nil))
         (org-link--add-to-stored-links link desc)
         (message "Stored: [[%s]]" link)))))
  
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
