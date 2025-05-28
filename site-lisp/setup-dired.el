;;; setup-dired.el --- Dired configurations -*- lexical-binding: t -*

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


(use-package dired
  :ensure nil
  :config 
  ;; ディレクトリがファイルの前に表示される。
  ;; 隠しファイルも表示される。
  ;; ファイルサイズは1K, 1.2Mのように読みやすく表示される。
  ;; ファイル名にバージョン番号がある場合、自然順で並べ替えられる。
  (setq dired-listing-switches "-laGh1v --group-directories-first")

  (setq dired-dwim-target t)
  )

;; dired WSL用の設定
(when uy/wsl-p
  (use-package dired
    :ensure nil
    :bind (:map dired-mode-map
                ("C-c o" . dired-open-file-on-windows)
                )
    :config
    (setf dired-kill-when-opening-new-dired-buffer t)

    (defun dired-open-file-on-windows (arg)
      "Open file (or its parent directory with prefix) on Windows using wslview.
       With prefix ARG (C-u), open the parent directory instead."
      ;; wslview は wslutilities/wslu https://github.com/wslutilities/wslu に含まれる
      (interactive "P")
      (let* ((path (dired-get-filename))
             (target (if arg (file-name-directory path) path)))
        (message "Opening on Windows: %s..." target)
        (shell-command (mapconcat #'shell-quote-argument
                                  (list "wslview" target)
                                  " "))
        (message "Opened on Windows: %s." target)))

    )
  )

;; dired-preview
(use-package dired-preview
  :ensure t
  :config
  (setopt dired-preview-ignored-extensions-regexp
          (concat
           "\\.\\(mkv\\|webm\\|mp4\\|mp3\\|ogg\\|m4a\\|flac\\|wav\\|gz\\|zst\\|tar\\|xz\\|rar\\|zip\\|iso\\|epub\\|pdf"  ;; default value
           "\\|docx\\|xlsx\\|pptx"  ;; MS Office
           "\\)"))
  (setopt dired-preview-delay 0.3)
  )

;; dired-hacks
(use-package dired-hacks
  :ensure t
  :defer t
  :vc (:url "https://github.com/Fuco1/dired-hacks" :branch "main" :rev :newest)
  )


(provide 'setup-dired)
