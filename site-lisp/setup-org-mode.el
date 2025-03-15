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

  ;; <user-emacs-directory>/elpa/org-mode にあるバージョンを使用する
  ;; インストール手順は以下
  ;; https://orgmode.org/manual/Installation.html
  ;; $ cd ~/.emacs.d/elpa
  ;; $ git clone https://git.savannah.gnu.org/git/emacs/org-mode.git
  ;; $ cd org-mode/
  ;; $ make autoloads
  :ensure t
  :load-path "elpa/org-mode/lisp/"
  :bind (("C-c c" . org-capture)
         ("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c i t" . org-time-stamp-inactive)
         ("C-c i i" . org-id-get-create)
         ("C-c [" . nil)
         )

  :init
  ;; 保存先
  (setq org-directory "~/org-roam/")
  (setq org-agenda-files (list (file-name-concat org-directory "journal/agenda.org")))
  (setq org-startup-folded nil)

  :config  
  ;; 基本設定
  ;; Hide the first N-1 stars in a headline : nil --> t
  (setq org-hide-leading-stars t)
  ;; RET will follow the link : nil --> t
  (setq org-return-follows-link t)
  ;; Todo 完了日時を記録
  (setq org-log-done 'time)
  ;; 見出し直後のみインデント調整をする
  (setq org-adapt-indentation nil)
  ;; ordered サブタスクの先頭のみを表示する
  (setq org-enforce-todo-dependencies t)
  ;; ^, _ による上付き化、下付き化を{}で囲んだ文字列に対してのみ有効にする 
  (setq org-use-sub-superscripts "{}")

  ;; IDをタイムスタンプにする
  (setq org-id-method 'ts)
  (setq org-id-ts-format "%Y%m%dT%H%M%S")  ;; 秒の小数点以下は省略（denote）
  (setq org-attach-id-to-path-function-list
        '(org-attach-id-ts-folder-format
          org-attach-id-uuid-folder-format
          org-attach-id-fallback-folder-format))

  (setq org-agenda-dim-blocked-tasks 'invisible)
  ;; org-captureを呼び出したときに不要なIDが追加されるのを避ける
  (setq org-id-link-to-org-use-id 'create-if-interactive)
  
  ;; org-indent-modeをtにすると、見出しレベルに合わせてインデント表示する。
  ;;;; invalid face reference
  (require 'org-indent)
  (setq org-indent-indentation-per-level 1)

  ;; Font size control of LateX previews in Org files
  (plist-put org-format-latex-options :scale 1.25)
  
  ;; リンクを開くプログラムの指定
  ;; Windowsの関連付けでファイルを開くために wslview を使う。
  ;; （wslviewは wslutilities/wslu https://github.com/wslutilities/wslu に含まれる）
  (add-to-list 'org-file-apps '("\\.xlsx?\\'" . "wslview %s"))
  (add-to-list 'org-file-apps '("\\.docx?\\'" . "wslview %s"))
  (add-to-list 'org-file-apps '("\\.pptx?\\'" . "wslview %s"))
  
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
          (sequence "WAITING(w!)" "HOLD(h!)" "|" "SOMEDAY(s!)" "CANCELLED(c!)")))

  (setq org-tag-persistent-alist
        '((:startgroup . nil)
          (:startgrouptag)
          ("Project") (:grouptags) ("PJ@.+")  ;; tag hierarchy, 
          (:endgrouptag)
          (:endgroup . nil)
          (:startgroup . nil)
          ("mtg" . ?m) ("book" . ?b)
          (:endgroup . nil)
          (:newline)
          (:startgroup . nil)
          ("SemiBiz") ("Optics") ("Simulation") ("Emacs") ("Visualization")
          (:endgroup . nil)
          (:startgroup . nil)
          ("today" . ?t) ("thisweek" . ?w) ("thismonth")
          (:endgroup . nil)
          (:startgroup . nil)
          ("@home" . ?h) ("@errand" . ?e) ("@office" . ?o)
          (:endgroup . nil)
          (:startgroup . nil)
          ("noexport" . ?n)
          (:endgroup . nil)
          ))

  ;; org-modeのcapture template
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline "journal/agenda.org" "Inbox")
           "* TODO %?\n\n\n" :prepend t)
          ("j" "Journal" entry
           (file+olp+datetree "journal/journal.org")
           "* %U %?\n")))

  (setq myroamfiles (directory-files org-directory t "org$"))
  (defun uy/org-files-list-except-journal ()
    "Return a list of all org files except those starting with 'journal'."
    (let ((org-files (org-files-list)))
      (seq-filter (lambda (file)
                    (not (string-match-p "/journal[^/]*\\.org\\'" file)))
                  org-files)))
  (setq org-refile-targets (quote ((org-agenda-files :maxlevel . 3)
                                   (uy/org-files-list-except-journal :maxlevel . 4)  ;; all agenda and opened files
                                   (myroamfiles :maxlevel . 4)
                                   )))
  ;; add file name to refile target path list
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-use-outline-path 'file)

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

  (setq org-agenda-window-setup 'current-window)
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-return-follows-link t)  ;; RET to follow link
  (setq org-agenda-columns-add-appointments-to-effort-sum t)
  (setq org-agenda-time-grid
        '((daily today require-timed)
          (0900 1000 1200 1300 1500 1700) "......" "----------------"))
  (setq org-columns-default-format
        "%50ITEM(Todo) %TODO %1PRIORITY %5Effort(Effort){:} %5CLOCKSUM(Spent){:} %8TAGS")

  (add-hook 'org-agenda-mode-hook
            (lambda ()
              (local-set-key (kbd "S") 'org-save-all-org-buffers)))

  (setq org-use-speed-commands t)
  (setq org-image-actual-width nil)
  
  ;; active Babel languages
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t)
     (dot . t)
     (latex . t)
     (python . t)
     (plantuml . t)
     ))

  ;; Export
  (setq org-export-with-creator nil)
  
  ;; save the clock history across Emacs sessions
  (setq org-clock-persist 'history)
  (org-clock-persistence-insinuate)

  (use-package ox-pandoc
    :ensure t
    :config
    (with-eval-after-load 'ox
      (require 'ox-pandoc)))

  ;; https://git.sr.ht/~bzg/org-contrib
  (use-package org-contrib :ensure t)
  
  (setq org-latex-default-class "my:lualatex")
  (setq org-latex-pdf-process   '("latexmk -lualatex %f"))
  (setq org-latex-classes
        '(("my:lualatex"
           "\\documentclass{ltjsarticle}
        "
           ("\\section\{%s\}" . "\\section*\{%s\}")
           ("\\subsection\{%s\}" . "\\subsection*\{%s\}")
           ("\\subsubsection\{%s\}" . "\\subsubsection*\{%s\}"))
          ("my:beamer"
           "\\documentclass[unicode,12pt]{beamer}
\\usepackage{luatexja}
\\usepackage[yu-win]{luatexja-preset}
\\renewcommand{\\kanjifamilydefault}{\\gtdefault}% 既定をゴシック体に"
           ("\\section\{%s\}" . "\\section*\{%s\}")
           ("\\subsection\{%s\}" . "\\subsection*\{%s\}")
           ("\\subsubsection\{%s\}" . "\\subsubsection*\{%s\}"))))

  (setq org-edit-src-content-indentation 0)
  
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
        (setq indent-tabs-mode itm
              tab-width tw)
        (set-syntax-table st)
        (insert-buffer-substring buf beginning end)
        ;; Establish the minimum level of indentation.
        (goto-char (point-min))
        (while (and (re-search-forward "^[[:space:]\n]*" nil :noerror)
                    (not (eobp)))
          (let ((length (current-column)))
            (when (or (not indent) (< length indent))
              (setq indent length)))
          (forward-line 1))
        (if (not indent)
            (error "Region is entirely whitespace")
          ;; Un-indent the buffer contents by the length of the minimum
          ;; indent level, and copy to the kill ring.
          (when pad
            (setq indent (- indent (prefix-numeric-value pad))))
          (indent-rigidly (point-min) (point-max) (- indent))
          (copy-region-as-kill (point-min) (point-max))))))
  (define-key org-mode-map (kbd "C-c M-w") 'my-copy-region-unindented)

  ;; 現在の行の上に見出しを挿入する関数
  ;; https://emacs.stackexchange.com/questions/37851/how-do-i-insert-a-heading-above-the-current-one-in-org-mode
  (defun org-insert-heading-above ()
    (interactive)
    (move-beginning-of-line nil)
    (org-insert-heading))

  (use-package ox-reveal
    :ensure t
    :vc (:url "https://github.com/yjwen/org-reveal")
    :config
    (setq org-reveal-root "file://d:/app/reveal.js"))

  ;; PlantUMLの設定
  ;; （PlangUMLのJARファイルをローカルで実行する。Java環境が必要。）
  (setq org-plantuml-exec-mode 'jar)
  (setq org-plantuml-jar-path "~/.local/bin/plantuml.jar")
  )

(use-package org-bullets
  :ensure t
  :commands (org-bullets-mode)
  :init (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  (setq org-bullets-bullet-list '("◉" "●" "◆" "►" "▸" "•"))
  )

;; org-bookmark-heading
;; Use the standard Emacs bookmark commands, C-x r m
(use-package org-bookmark-heading :ensure t)

(provide 'setup-org-mode)
