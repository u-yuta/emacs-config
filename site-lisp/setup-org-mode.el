;;; setup-org-mode.el --- Org-mode configurations -*- lexical-binding: t -*

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


(use-package org
  ;; :mode (("\\.txt$" . org-mode))

  ;; use Nix emacsPackages.org
  :ensure t
  :bind (("C-c c" . org-capture)
         ("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c C-x C-j" . org-clock-goto)
         :map org-mode-map
         ("C-c ." . org-time-stamp-inactive)
         ("C-c !" . org-time-stamp)
         ("C-c [" . nil)  ;; unbind org-agenda-file-to-front
         )

  :init
  ;; 保存先
  (setopt org-directory "~/Documents/org/")
  (setopt uy/journal-directory (file-name-concat org-directory "p1-journal"))
  (setopt org-agenda-files '("s0-agenda/shared-agenda.org"))  ;; shared directory
  (setopt org-startup-folded 'nofold)

  :config  
  ;; 基本設定
  ;; Hide the first N-1 stars in a headline : nil --> t
  (setopt org-hide-leading-stars t)
  ;; RET will follow the link : nil --> t
  (setopt org-return-follows-link t)
  ;; Todo 完了日時を記録
  (setopt org-log-done 'time)
  ;; 見出し直後のみインデント調整をする
  (setopt org-adapt-indentation nil)
  ;; ordered サブタスクの先頭のみを表示する
  (setopt org-enforce-todo-dependencies t)
  ;; ^, _ による上付き化、下付き化を{}で囲んだ文字列に対してのみ有効にする 
  (setopt org-use-sub-superscripts '{})

  ;; IDをタイムスタンプにする
  (setopt org-id-method 'ts)
  (setopt org-id-ts-format "%Y%m%dT%H%M%S")  ;; 秒の小数点以下は省略（denote）
  (setopt org-attach-id-to-path-function-list
        '(org-attach-id-ts-folder-format
          org-attach-id-uuid-folder-format
          org-attach-id-fallback-folder-format))

  (setopt org-agenda-dim-blocked-tasks 'invisible)
  ;; org-captureを呼び出したときに不要なIDが追加されるのを避ける
  (setopt org-id-link-to-org-use-id 'create-if-interactive)
  
  ;; org-indent-modeをtにすると、見出しレベルに合わせてインデント表示する。
  ;;;; invalid face reference
  (require 'org-indent)
  (setopt org-indent-indentation-per-level 1)

  ;; Font size control of LateX previews in Org files
  (plist-put org-format-latex-options :scale 1.25)
  
  ;; Archiveの保存先ファイル： `<元ファイル名>_archive`, 見出し： datetree。
  ;; datetree の日付は CLOSED があればその日になる。なければ現在の日付になる。
  (setopt org-archive-location "%s_archive::datetree/")

  ;; リンクを開くプログラムの指定
  (add-to-list 'org-file-apps '("\\.xlsx?\\'" . "open %s"))
  (add-to-list 'org-file-apps '("\\.docx?\\'" . "open %s"))
  (add-to-list 'org-file-apps '("\\.pptx?\\'" . "open %s"))
  
  (setopt org-todo-keywords
          '((sequence "TODO(t)" "ONGO(o)" "NEXT(n)" "|" "DONE(d)")
            (sequence "WAITING(w!)" "HOLD(h!)" "|" "SOMEDAY(s!)" "CANCELLED(c!)")))

  (setopt org-tag-persistent-alist
        '((:startgroup . nil)
          (:startgrouptag)
          ("project") (:grouptags) ("PJ@.+")  ;; tag hierarchy, 
          (:endgrouptag)
          (:endgroup . nil)
          (:startgroup . nil)
          ("mtg" . ?m) ("book" . ?b) ("chore" . ?c)
          (:endgroup . nil)
          (:newline)
          (:startgroup . nil)
          ("@home" . ?h) ("@errand" . ?e) ("@work" . ?w)
          (:endgroup . nil)
          (:startgroup . nil)
          ("noexport" . ?n)
          (:endgroup . nil)
          ))

  (defun uy/journal-file-name-year-month ()
    "Return a string representing the journal file path in the formatting
    '<journal-directory>/journal-YYYY-mm.org' using current year and month."
    (file-name-concat uy/journal-directory (format-time-string "journal-%Y-%m.org")))

  ;; org-modeのcapture template
  (setopt org-capture-templates
        '(("j" "Journal" entry
           (file+olp+datetree (lambda () (uy/journal-file-name-year-month)))
           "* %U %?\n")
          ))

  (setopt myroamfiles (directory-files org-directory t "org$"))
  (defun uy/org-files-list-except-journal ()
    "Return a list of all org files except those starting with 'journal'."
    (let ((org-files (org-files-list)))
      (seq-filter (lambda (file)
                    (not (string-match-p "/journal[^/]*\\.org\\'" file)))
                  org-files)))
  (setopt org-refile-targets (quote ((org-agenda-files :maxlevel . 3)
                                   (uy/org-files-list-except-journal :maxlevel . 4)  ;; all agenda and opened files
                                   (myroamfiles :maxlevel . 4)
                                   )))
  ;; add file name to refile target path list
  (setopt org-outline-path-complete-in-steps nil)
  (setopt org-refile-use-outline-path 'file)

  ;; org-agendaでのアイテム表示のカスタマイズ
  (setopt org-agenda-prefix-format
          '((agenda . " %i %-12:c%?-12t% s")
            (todo . "%-12.12c %i%-40.40b ")  ;; breadcrumbs を表示
            (tags . " %i %-12:c")
            (search . " %i %-12:c")))

  ;; リファイルするとともにattachmentsもリファイル先に移動する
  (defun uy/org-refile-with-attachments-keep-id ()
    "Refile current heading and move attachments to the new location, keeping the same ID."
    (interactive)
    ;; TODO attach-dir が存在しない場合はIDの追加などを行わないようにする
    (let* ((old-id (org-id-get nil t)) ;; ヘッダーのIDを取得または生成
           (old-attach-dir (org-attach-dir)))
      (message "old-attach-dir:%s" old-attach-dir)
      ;; org-refileを実行し、リファイル先に移動
      (message "do refile")
      (org-refile)  ;; 通常の `org-refile` を実行
      (message "refile done")
      (org-refile-goto-last-stored)  ;; リファイル先に移動
      (when old-attach-dir
        ;; リファイル後の新しいAttachmentディレクトリを取得
        (let* ((new-attach-dir (org-attach-dir t)))
          (message "new-attach-dir (after refile):%s" new-attach-dir)
          (when (and old-attach-dir new-attach-dir
                     (not (string= old-attach-dir new-attach-dir))
                     (file-exists-p old-attach-dir))
            ;; 新しいディレクトリが存在しない場合は作成
            (unless (file-exists-p new-attach-dir)
              (make-directory new-attach-dir t))
            ;; 古いAttachmentディレクトリの中身を新しいディレクトリに移動
            (dolist (file (directory-files old-attach-dir t "\\(?:[^.]\\|\\.[^.]\\|\\.\\..\\)"))
              (rename-file file
                           (expand-file-name (file-name-nondirectory file) new-attach-dir)
                           t))
            ;; 古いディレクトリを削除
            (delete-directory old-attach-dir)
            (message "Attachments moved from %s to %s" old-attach-dir new-attach-dir))))))

  (setopt org-agenda-window-setup 'current-window)
  (setopt org-agenda-skip-scheduled-if-done t)
  (setopt org-return-follows-link t)  ;; RET to follow link
  (setopt org-agenda-columns-add-appointments-to-effort-sum t)
  (setopt org-agenda-time-grid
        '((daily today require-timed)
          (0900 1000 1200 1300 1500 1700) "......" "----------------"))
  (setopt org-columns-default-format
        "%50ITEM(Todo) %TODO %1PRIORITY %5Effort(Effort){:} %5CLOCKSUM(Spent){:} %8TAGS")

  (add-hook 'org-agenda-mode-hook
            (lambda ()
              (local-set-key (kbd "S") 'org-save-all-org-buffers)))

  (setopt org-use-speed-commands t)
  (setopt org-image-actual-width nil)
  
  ;; active Babel languages
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t)
     (dot . t)
     (latex . t)
     (python . t)
     (mermaid . t)
     (plantuml . t)
     (d2 . t)
     ))

  ;; Export
  (setopt org-export-default-language "ja")
  (setopt org-export-with-creator nil)
  
  ;; save the clock history across Emacs sessions
  (setopt org-clock-persist 'history)
  (org-clock-persistence-insinuate)

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
        (setopt string
              (replace-regexp-in-string
               (format "\\(%s\\) *\n *\\(%s\\)" regexp regexp)
               "\\1\\2" string))
        ;; 删除粗体之后的空格
        (dolist (str '("</b>" "</code>" "</del>" "</i>"))
          (setopt string
                (replace-regexp-in-string
                 (format "\\(%s\\)\\(%s\\)[ ]+\\(%s\\)" regexp str regexp)
                 "\\1\\2\\3" string)))
        ;; 删除粗体之前的空格
        (dolist (str '("<b>" "<code>" "<del>" "<i>" "<span class=\"underline\">"))
          (setopt string
                (replace-regexp-in-string
                 (format "\\(%s\\)[ ]+\\(%s\\)\\(%s\\)" regexp str regexp)
                 "\\1\\2\\3" string)))
        string)))

  (add-hook 'org-export-filter-headline-functions #'eh-org-wash-text)
  (add-hook 'org-export-filter-paragraph-functions #'eh-org-wash-text)

  (use-package ox-pandoc
    :ensure t
    :config
    (with-eval-after-load 'ox
      (require 'ox-pandoc)))

  ;; https://git.sr.ht/~bzg/org-contrib
  (use-package org-contrib :ensure t)
  
  (setopt org-latex-default-class "my:lualatex")
  (setopt org-latex-pdf-process   '("latexmk -lualatex -shell-escape %f"))
  (setopt org-latex-classes
        '(("my:lualatex"
           "\\documentclass{ltjsarticle}
\\usepackage {svg}
        "
           ("\\section\{%s\}" . "\\section*\{%s\}")
           ("\\subsection\{%s\}" . "\\subsection*\{%s\}")
           ("\\subsubsection\{%s\}" . "\\subsubsection*\{%s\}"))
          ("beamer"
           "\\documentclass[unicode,12pt]{beamer}
\\usepackage{luatexja}
\\usepackage[yu-win]{luatexja-preset}
\\usepackage {svg}
\\renewcommand{\\kanjifamilydefault}{\\gtdefault}% 既定をゴシック体に"
           ("\\section\{%s\}" . "\\section*\{%s\}")
           ("\\subsection\{%s\}" . "\\subsection*\{%s\}")
           ("\\subsubsection\{%s\}" . "\\subsubsection*\{%s\}"))))

  (setopt org-edit-src-content-indentation 0)
  
  ;; インデントを気にせずに yank/copy する
  ;; https://emacs.stackexchange.com/questions/31646/how-to-paste-with-indent より転載
  (defun yank-with-indent ()
    (interactive)
    (let ((indent
           (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
      (message indent)
      (yank)
      (save-excursion
        (save-restriction
          (narrow-to-region (mark t) (point))
          (pop-to-mark-command)
          (replace-string "\n" (concat "\n" indent))
          (widen)))))  
  (define-key org-mode-map (kbd "C-c C-y") 'yank-with-indent)

  ;; https://emacs.stackexchange.com/questions/34966/copy-region-without-leading-indentation より転載
  (defun my-copy-region-unindented (pad beginning end)
    "Copy the region, un-indented by the length of its minimum indent.
     
     If numeric prefix argument PAD is supplied, indent the resulting
     text by that amount."
    (interactive "P\nr")
    (let ((buf (current-buffer))
          (itm indent-tabs-mode)
          (tw tab-width)
          (st (syntax-table))
          (indent nil))
      (with-temp-buffer
        (setopt indent-tabs-mode itm
              tab-width tw)
        (set-syntax-table st)
        (insert-buffer-substring buf beginning end)
        ;; Establish the minimum level of indentation.
        (goto-char (point-min))
        (while (and (re-search-forward "^[[:space:]\n]*" nil :noerror)
                    (not (eobp)))
          (let ((length (current-column)))
            (when (or (not indent) (< length indent))
              (setopt indent length)))
          (forward-line 1))
        (if (not indent)
            (error "Region is entirely whitespace")
          ;; Un-indent the buffer contents by the length of the minimum
          ;; indent level, and copy to the kill ring.
          (when pad
            (setopt indent (- indent (prefix-numeric-value pad))))
          (indent-rigidly (point-min) (point-max) (- indent))
          (copy-region-as-kill (point-min) (point-max))))))
  (define-key org-mode-map (kbd "C-c M-w") 'my-copy-region-unindented)

  ;; 現在の行の上に見出しを挿入する関数
  ;; https://emacs.stackexchange.com/questions/37851/how-do-i-insert-a-heading-above-the-current-one-in-org-mode
  (defun org-insert-heading-above ()
    (interactive)
    (move-beginning-of-line nil)
    (org-insert-heading))


  ;; Dired から org-attach を使う
  (add-hook 'dired-mode-hook
            (lambda ()
              (define-key dired-mode-map
                          (kbd "C-c C-x a")
                          #'org-attach-dired-to-subtree)))

  (use-package ox-reveal
    :ensure t
    :vc (:url "https://github.com/yjwen/org-reveal")
    :config
    (setopt org-reveal-root "file://d:/app/reveal.js"))

  ;; PlantUMLの設定
  ;; （PlangUMLのJARファイルをローカルで実行する。Java環境が必要。）
  (setopt org-plantuml-exec-mode 'plantuml)
  (setopt org-plantuml-jar-path "~/.local/bin/plantuml.jar")
  )

(use-package org-bullets
  :ensure t
  :commands (org-bullets-mode)
  :init (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  (setopt org-bullets-bullet-list '("◉" "●" "◆" "►" "▸" "•"))
  )

;; org-bookmark-heading
;; Use the standard Emacs bookmark commands, C-x r m
(use-package org-bookmark-heading :ensure t)

(use-package org-download
  :ensure t
  :after org
  :init
  (setopt org-download-screenshot-basename "__screenshot.png")
  :config
  ;; org-modeのattachment機能を使わず、カレントディレクトリにファイル保存する
  (setopt org-download-method 'directory)  
  (setopt org-download-image-dir nil)  ;; current directory
  (setopt org-download-heading-lvl nil)  ;; no subdirectories
  (setopt org-download-timestamp "")

  ;; org-download-screenshot でクリップボードの画像を保存する
  (setq org-download-screenshot-method
        "powershell.exe -Command \"(Get-Clipboard -Format image).Save('$(wslpath -w %s)')\"")

  ;; Prompt file name
  (setopt org-download-file-format-function 'uy/org-download-file-format)
  (defun uy/org-download-file-format (filename)
    (read-file-name
     "File name: "
     nil nil nil
     (concat
      (format-time-string "%Y%m%dT%H%M%S--")
      filename
      )))
  )

;; el-easydraw
(use-package edraw-org
  :ensure t
  :after org
  :vc (:url "https://github.com/misohena/el-easydraw" :rev :newest)
  :config
  (edraw-org-setup-default))

;; When using the org-export-in-background option (when using the
;; asynchronous export function), the following settings are
;; required. This is because Emacs started in a separate process does
;; not load org.el but only ox.el.
(with-eval-after-load "ox"
  (require 'edraw-org)
  (edraw-org-setup-exporter))

;; ob-mermaid
(use-package ob-mermaid
  :ensure t
  :config
  (setopt ob-mermaid-cli-path "mmdc"))

;; ob-d2
(use-package ob-d2
  :vc (:url "https://github.com/dmacvicar/ob-d2" :rev :newest)
  :ensure t
  :defer t)

;; d2-mode
(use-package d2-mode
  :ensure t)

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

(provide 'setup-org-mode)
