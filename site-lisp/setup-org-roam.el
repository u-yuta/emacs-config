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
  :init (setq org-roam-v2-ack t)
  :after (org)
  :hook
  (after-init . org-roam-db-autosync-mode)
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
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:12}" 'face 'org-tag)))

  ;; org-roam capture template
  (setq org-roam-capture-templates
        '(("d" "default" plain "%?" :target
           (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}")
           :unnarrowed t)
           ("w" "work" plain "%?" :target
           (file+head "work/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}")
           :unnarrowed t)
          ("p" "personal" plain "%?" :target
           (file+head "personal/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}")
           :unnarrowed t)
          ("s" "share" plain "%?" :target
           (file+head "share/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}")
           :unnarrowed t)
          ))
  
  ;; org-roam dailies の設定
  (setq org-roam-dailies-directory "journal/")
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
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))


(defun as-windows-path (unix-path)
  "Takes a unix path and returns a matching WSL path
(e.g. \\\\wsl$\\Ubuntu-20.04\\tmp)"
  ;; substring removes the trailing \n
  (substring
   (shell-command-to-string
    (concat "wslpath -w " unix-path)) 0 -1))

(defun powershell (script)
  "executes the given script within a powershell and returns its return value"
  (call-process "powershell.exe" nil nil nil
                "-Command" (concat "& {" script "}")))

(use-package org-download
  :ensure t
  :after org
  :config
  (setq org-download-method 'attach)
  (setq org-download-screenshot-method
        "powershell.exe -Command \"(Get-Clipboard -Format image).Save('$(wslpath -w %s)')\""))

;; Dired から org-attach を使う
(add-hook 'dired-mode-hook
          (lambda ()
            (define-key dired-mode-map
              (kbd "C-c C-x a")
              #'org-attach-dired-to-subtree)))

(defun roam-sitemap (title list)
  (concat "#+OPTIONS: ^:nil author:nil html-postamble:nil\n"
          "#+SETUPFILE: ./simple_inline.theme\n"
          "#+TITLE: " title "\n\n"
          (org-list-to-org list) "\nfile:sitemap.svg"))

(setq my-publish-time 0)   ; see the next section for context
(defun roam-publication-wrapper (plist filename pubdir)
  (org-roam-graph)
  (org-html-publish-to-html plist filename pubdir)
  (setq my-publish-time (cadr (current-time))))

(setq org-publish-project-alist
  '(("roam"
     :base-directory org-directory
     :auto-sitemap t
     :sitemap-function roam-sitemap
     :sitemap-title "Roam notes"
     :publishing-function roam-publication-wrapper
     :publishing-directory "~/roam-export"
     :section-number nil
     :table-of-contents nil
     :style "<link rel=\"stylesheet\" href=\"../other/mystyle.cs\" type=\"text/css\">")))


;; org-directory/%Y/%m に 日付-連番-<title_slug>.org のノートファイルを作成する関数
(defun uy/create-new-lab-note ()
  "Create a new note file in `org-directory/lab-notes'.
The filename will be `org-directory/lab-notes/%Y/%m/%Y%m%d-${i}-${slug}.org'.
Where `i' is the smallest number+1 of `%Y%m%d-number-*.org' in the same folder.
The title is input from a minibuffer prompt, and the slug is generated from the title 
using `org-roam-node-slug'."
  (interactive)
  (let* ((date (format-time-string "%Y%m%d"))
         (year (format-time-string "%Y"))
         (month (format-time-string "%m"))
         (lab-notes-dir (expand-file-name (concat "lab-notes/" year "/" month "/") org-directory))
         (title (read-string "Note title: "))
         ;; Create a temporary org-roam-node with just the title for slugification
         (temp-node (org-roam-node-create :title title))
         (slug (org-roam-node-slug temp-node))
         (filename-pattern (concat date "-[0-9]+-.*\\.org"))
         (file-number 1))
    
    ;; Create directory if it doesn't exist
    (unless (file-exists-p lab-notes-dir)
      (make-directory lab-notes-dir t))
    
    ;; Find the next available number
    (when (file-exists-p lab-notes-dir)
      (let ((files (directory-files lab-notes-dir nil filename-pattern))
            (max-num 0))
        (dolist (file files)
          (when (string-match (concat date "-\\([0-9]+\\)-") file)
            (let ((num (string-to-number (match-string 1 file))))
              (when (> num max-num)
                (setq max-num num)))))
        (setq file-number (1+ max-num))))
    
    ;; Create the file
    (let ((file-path (expand-file-name (format "%s-%d-%s.org" date file-number slug) lab-notes-dir)))
      (find-file file-path)
      (insert (format "#+TITLE: %s\n" title))
      (insert (format "#+DATE: %s\n" (format-time-string "%Y-%m-%d")))
      (insert "#+FILETAGS: :lab:\n\n")
      (org-id-get-create)
      (goto-char (point-max)))))

;; Tableの形式をその場で変換する関数。
(defun org-table-transform-in-place ()
  "Just like `ORG-TABLE-EXPORT', but instead of exporting to a
  file, replace table with data formatted according to user's
  choice, where the format choices are the same as
  org-table-export.
  https://stackoverflow.com/a/38277039"
  (interactive)
  (unless (org-at-table-p) (user-error "No table at point"))
  (org-table-align)
  (let* ((format
      (completing-read "Transform table function: "
               '("orgtbl-to-tsv" "orgtbl-to-csv" "orgtbl-to-latex"
                 "orgtbl-to-html" "orgtbl-to-generic"
                 "orgtbl-to-texinfo" "orgtbl-to-orgtbl"
                 "orgtbl-to-unicode")))
     (curr-point (point)))
    (if (string-match "\\([^ \t\r\n]+\\)\\( +.*\\)?" format)
    (let ((transform (intern (match-string 1 format)))
          (params (and (match-end 2)
               (read (concat "(" (match-string 2 format) ")"))))
          (table (org-table-to-lisp
              (buffer-substring-no-properties
               (org-table-begin) (org-table-end)))))
      (unless (fboundp transform)
        (user-error "No such transformation function %s" transform))
      (save-restriction
        (with-output-to-string
          (delete-region (org-table-begin) (org-table-end))
          (insert (funcall transform table params) "\n")))
      (goto-char curr-point)
      (beginning-of-line)
      (message "Tranformation done."))
      (user-error "Table export format invalid"))))

;; org-modeのエクスポート時に、マルチバイト文字間では、
;; 改行箇所に挿入されるスペースを削除する。
;; 以下のサイトの情報を参考にした。
;; Bug: ODT export of Chinese text inserts spaces for line breaks
;; https://yhetil.org/orgmode/sbhnlv$4t1$1@ciao.gmane.io/T/
(defun eh-org-wash-text (text backend _info)
  "导出 org file 时，删除中文之间不必要的空格。"
  (when (or (org-export-derived-backend-p backend 'html)
            (org-export-derived-backend-p backend 'odt)
            (org-export-derived-backend-p backend 'pandoc)
            (org-export-derived-backend-p backend 'latex))
    (let ((regexp "[[:multibyte:]]")
          (string text))
      ;; org-mode 默认将一个换行符转换为空格，但中文不需要这个空格，删除。
      (setq string
            (replace-regexp-in-string
             (format "\\(%s\\) *\n *\\(%s\\)" regexp regexp)
             "\\1\\2" string))
      ;; 删除粗体之后的空格
      (dolist (str '("</b>" "</code>" "</del>" "</i>"))
        (setq string
              (replace-regexp-in-string
               (format "\\(%s\\)\\(%s\\)[ ]+\\(%s\\)" regexp str regexp)
               "\\1\\2\\3" string)))
      ;; 删除粗体之前的空格
      (dolist (str '("<b>" "<code>" "<del>" "<i>" "<span class=\"underline\">"))
        (setq string
              (replace-regexp-in-string
               (format "\\(%s\\)[ ]+\\(%s\\)\\(%s\\)" regexp str regexp)
               "\\1\\2\\3" string)))
      string)))

(add-hook 'org-export-filter-headline-functions #'eh-org-wash-text)
(add-hook 'org-export-filter-paragraph-functions #'eh-org-wash-text)

(provide 'setup-org-roam)
