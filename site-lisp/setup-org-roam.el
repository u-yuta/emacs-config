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
  :demand t    ;; Emacs server起動直後にagentがorg-roam DBを利用できるようにするため
  :commands org-roam-dailies-goto-today  ;; workaround for transient error: `already defined as something else than a generic function'
  :custom
  (org-roam-directory org-directory)
  :bind (
         (("C-c n f" . org-roam-node-find)
          ("C-c n g" . org-roam-graph)
          ("C-c n b" . org-roam-buffer-toggle)
          ("C-c n c" . org-roam-capture)
          ("C-c j j" . org-roam-dailies-goto-today)
          ("C-c j d" . org-roam-dailies-goto-date)
          ("C-c j y" . org-roam-dailies-goto-yesterday)
          ("C-c j p" . org-roam-dailies-goto-previous-note)
          ("C-c j n" . org-roam-dailies-goto-next-note)
          )
         :map org-mode-map
         (("C-c n i" . org-roam-node-insert))
         (("C-c n I" . org-roam-insert-immediate)))
  :config
  ;; org-roam note main
  (setopt my/org-inbox-directory
          (seq-find #'file-directory-p
                    (mapcar #'my/org-path
                            '("personal/inbox" "work/inbox"))))
  
  (setopt org-roam-directory-inbox (expand-file-name "personal/inbox" org-roam-directory))
  ;; Configures display formatting for Org-roam node.
  ;; https://github.com/org-roam/org-roam/wiki/User-contributed-Tricks#filtering-by-subdirectory
  (cl-defmethod org-roam-node-directories ((node org-roam-node))
    (if-let ((dirs (file-name-directory (file-relative-name (org-roam-node-file node) org-roam-directory))))
        (format "%s/" (car (split-string dirs "/")))
      ""))
  (setopt org-roam-node-display-template "${directories:12} ${title:80} ${tags:10}"
          org-roam-node-annotation-function
          (lambda (node) (marginalia--time (org-roam-node-file-mtime node))))

  ;; org-roam capture template
  (setopt org-roam-capture-templates
          `(
            ("t" "task" plain "%?" :target
             (file+head ,(format "%s/${id}.org" my/org-inbox-directory)  ":PROPERTIES:
:ID: ${id}
:KIND: task
:END:
#+title:      ${title}
#+filetags:   :work:
* 目的
* 作業内容
* メモ・中間結果
* 結果・知見
* リンク
") :unnarrowed t)
            ("m" "mission" plain "%?" :target
             (file+head ,(format "%s/${id}.org" my/org-inbox-directory)  ":PROPERTIES:
:ID: ${id}
:KIND: area
:END:
#+title:      ${title}
#+filetags:   :index:
* 概要
* スコープ
* 目的・背景
* 用語
") :unnarrowed t)
            ("a" "area" plain "%?" :target
             (file+head ,(format "%s/${id}.org" my/org-inbox-directory)  ":PROPERTIES:
:ID: ${id}
:KIND: area
:ROAM_ALIASES: %^{ROAM_ALIASES}
:END:
#+title:      ${title}
#+filetags:   :index:
* 概要
* スコープ
* 用語
") :unnarrowed t)
            ("w" "work note" plain "%?" :target
             (file+head ,(format "%s/${id}.org" my/org-inbox-directory)  "#+title:      ${title}
#+filetags:   :work:
") :unnarrowed t)
            ))

  ;; org-roam dailies template
  (setopt org-roam-dailies-directory my/journal-directory)
  (setopt my/org-roam-dailies-capture-template-head
          "#+title: %<%Y-%m-%d %A>\n#+filetags: :journal:\n\n* 予定\n\n* やること\n\n* メモ\n\n* 発生したタスク\n\n* サマリー\n\n")
  (setopt org-roam-dailies-capture-templates
          `(("j" "journal" entry "* %?"
             :target (file+head "%<%Y/journal-%Y%m%d>.org"
                                ,my/org-roam-dailies-capture-template-head)
             :unnarrowed t)
            ("m" "journal memo" entry "** %?"
             :target (file+head+olp "%<%Y/journal-%Y%m%d>.org"
                                    ,my/org-roam-dailies-capture-template-head
                                    ("メモ"))
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
    (save-excursion
      (goto-char (point-min))

      ;; 先頭が見出しなら、見出しの前にファイルレベル property drawer を作る
      (when (eq (car-safe (org-element-at-point)) 'headline)
        (insert ":PROPERTIES:\n:END:\n")
        (goto-char (point-min)))

      ;; point-min がファイルレベル領域になっている前提で ID を作る
      (let* ((id (org-id-get-create)))
        (call-interactively 'org-store-link))))

  (add-hook 'find-file-hook #'my/rename-buffer)

  (defun my/rename-buffer ()
    "Rename buffer to title when file is in Org-roam."
    (when (org-roam-file-p)
      (when-let* ((node (org-roam-node-at-point))
                  (title (org-roam-node-title node))
                  (id (org-roam-node-id node)))
        (rename-buffer (concat title " [" id "]")))))

  (require 'org-link-proj)
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

(use-package org-roam-ql
  :ensure t
  :after (org-roam)
  :demand t    ;; Emacs起動直後にorg-dynamic-blockから利用できるようにするため
  :bind (:map minibuffer-mode-map
         ;; Be able to add titles in queries while in minibuffer.
         ;; This is similar to `org-roam-node-insert', but adds
         ;; only title as a string.
         ("C-c n I" . org-roam-ql-insert-node-title))
  )

(provide 'setup-org-roam)
