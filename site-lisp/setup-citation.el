;;; setup-citation.el --- Citation configurations -*- lexical-binding: t -*

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



;; Bibliography files
(setopt my/bib-files '("~/doc_local/bibliography/references.bib"
                     "~/doc_local/bibliography/00Share.bib"))

(with-eval-after-load 'org
  ;; Org Cite library
  (setopt org-cite-global-bibliography my/bib-files)
  (setopt org-cite-export-processors
        '((md . (csl "chicago-fullnote-bibliography.csl"))   ; Footnote reliant
          ;;(latex . biblatex)                                 ; For humanities
          (odt . (csl "chicago-fullnote-bibliography.csl"))  ; Footnote reliant
          (t . (csl "modern-language-association.csl"))      ; Fallback
          ))
  (setopt org-cite-csl-styles-dir "~/Zotero/styles")  ; installed by Linux Zotero
  (require 'oc-csl)
  (use-package citeproc :ensure t)  ; oc-cslで使われる
  )

;; Citar (org-cite のインターフェース拡張)
(use-package citar
  :ensure t
  :after oc
  :custom
  (citar-bibliography my/bib-files)
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-notes-paths
   (list (expand-file-name "org/w3-area" org-directory)
         (expand-file-name "org/s3-area" org-directory)
         (expand-file-name "org/p3-area" org-directory)))
  :hook
  (LaTeX-mode . citar-capf-setup)
  (org-mode . citar-capf-setup)

  :config
  ;; WindowsのZoteroで作成した文献リストのfileフィールドをWSLのパスに変換する
  (defun my/citar-file--parser-windows-path-to-wsl (file-field)
    "Split FILE-FIELD by `;' and convert Windows paths to WSL paths.
  Each filename in FILE-FIELD is converted using `wslpath -u <filename>`.
  Returns a list of original and converted paths."
    (mapcan
     (lambda (filename)
       (let* ((trimmed (string-trim filename))
              (wslpath (string-trim (shell-command-to-string (format "wslpath -u '%s'" trimmed)))))
         ;; Include both the original and converted path if they differ
         (if (string-empty-p trimmed)
             nil
           (if (string= trimmed wslpath)
               (list trimmed)
             (list trimmed wslpath)))))
     (citar-file--split-escaped-string file-field ?\;)))

  (when my/wsl-p
    ;; Add the custom parser to citar-file-parser-functions
    (add-to-list 'citar-file-parser-functions 'my/citar-file--parser-windows-path-to-wsl))

  ;; 文献リストのUIにEmojiを表示する
  (with-eval-after-load 'emojify
    (defvar citar-indicator-notes-icons
      (citar-indicator-create
       :symbol "📓"
       :function #'citar-has-notes
       :padding "  "
       :tag "has:notes"))
    (defvar citar-indicator-links-icons
      (citar-indicator-create
       :symbol "🔗"
       :function #'citar-has-links
       :padding "  "
       :tag "has:links"))
    (defvar citar-indicator-files-icons
      (citar-indicator-create
       :symbol "📁"
       :function #'citar-has-files
       :padding "  "
       :tag "has:files"))
    (setopt citar-indicators
            (list citar-indicator-files-icons
                  citar-indicator-notes-icons
                  citar-indicator-links-icons))))

;; citar-org-roam: provide tighter Citar and Org-Roam integration
(use-package citar-org-roam
  :ensure t
  :after (citar org-roam)
  :custom
  (citar-org-roam-subdir "s3-area")
  :config
  (citar-org-roam-mode)
  (setopt citar-org-roam-note-title-template "${author} (${year}) -- ${title}"))

;; citar-denote
;; 主要なコマンド
;; - `citar-denote-open-note`：ノートが紐づいた文献だけに絞った Citar メニューを開き、既存の文献ノートを開く。
;; - `citar-denote-find-citation`：Denote コレクション内で「引用されている」文献を一覧し、該当ノートを開く。
;; - `citar-denote-dwim`：現在の Denote バッファの参照情報に基づき、関連する添付・リンク・ノート等を“適切に”開く入口。
;; - `citar-denote-open-reference-entry`：参照（reference）として指定した文献の元エントリ（BibTeX/CSL 等）を開いて編集できるようにする。
;; - `citar-denote-add-reference`：現在のノートに citation key を参照行として追加し、必要なら通常ノートを文献ノートへ変換する。
;; - `citar-denote-remove-reference`：現在の文献ノートから参照（citation key）を削除し、参照がなくなればタグ除去やリネームも行う。
;; - `citar-denote-find-reference`：現在のバッファの参照に関連して、他ノート側で参照／引用しているノートを探す。
;; - `citar-denote-link-reference`：ノートが存在する文献を選び、現在の Denote バッファへ該当ノートへのリンクを挿入する。
(use-package citar-denote
  :ensure t
  :demand t
  :after (:any citar denote)
  :custom
  (citar-denote-file-type 'org)
  (citar-denote-keyword "bib")
  (citar-denote-signature nil)
  (citar-denote-subdir t)
  (citar-denote-template nil)
  (citar-denote-title-format "author-year-title")
  (citar-denote-title-format-andstr "and")
  (citar-denote-title-format-authors 1)
  (citar-denote-use-bib-keywords nil)
  :init
  (citar-denote-mode))


(provide 'setup-citation)
