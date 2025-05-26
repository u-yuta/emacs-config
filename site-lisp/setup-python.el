;;; setup-python.el --- Python configurations -*- lexical-binding: t -*-

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


;; Built-in Python utilities
(use-package python
  :ensure nil
  :config
  ;; Remove guess indent python message
  (setq python-indent-guess-indent-offset-verbose nil)

  ;; REPLで補完文字列がバッファに挿入される問題の対策
  (with-eval-after-load 'corfu
  (add-hook 'inferior-python-mode-hook
            (lambda ()
              ;; バッファローカル変数として corfu-auto を nil に設定
              (setq-local corfu-auto nil))))

  ;; Pythonインタープリターを `uv run python' で起動するコマンド
  (defun uy/run-python-uv ()
    "Run an inferior Python process with shell interpreter command `uv run python'."
    (interactive
     (let ((python-shell-interpreter "uv")
           (python-shell-interpreter-args "run python -i"))
       (call-interactively 'run-python))))

  ;; uv環境用の設定
  ;; TODO ipythonがない場合は python にフォールバックする。u
  (setopt python-shell-interpreter "uv")
  (setopt python-shell-interpreter-args "run ipython --simple-prompt --classic")

  ;; Pythonのインタープリターにてbuffer-file を `__file__' に設定するコマンド
  (defun uy/python-shell-set-buffer-path-as-dunder-file ()
    """Set buffer-file path as __file__ in Python interpreter."""
    (interactive)
    (let ((cmd (concat "from pathlib import Path; __file__ = Path(\"" (buffer-file-name) "\").resolve()")))
      (python-shell-send-string cmd)
      (message cmd)))
  )

;; REPLで画像・数式表示
(use-package comint-mime
  :ensure t
  :config

  ;; 注： pythonのinterperterがipythonに設定されている必要がある (ipython は ipykernel の依存関係に含まれている)
  (add-hook 'inferior-python-mode-hook 'comint-mime-setup)
  )

;; code-cells.el — Lightweight notebooks in Emacs
;; Efficiently navigate, edit and execute code split into cells according to certain magic comments
;; like `# %% Optional title'.
(use-package code-cells
  :ensure t
  :config
  (add-hook 'python-mode-hook 'code-cells-mode-maybe)
  (add-hook 'python-ts-mode-hook 'code-cells-mode-maybe)
  (define-key code-cells-mode-map (kbd "M-p") 'code-cells-backward-cell)
  (define-key code-cells-mode-map (kbd "M-n") 'code-cells-forward-cell)
  (define-key code-cells-mode-map (kbd "C-c C-c") 'code-cells-eval)
  (define-key code-cells-mode-map (kbd "<C-return>") 'code-cells-eval-and-step)
  (define-key code-cells-mode-map (kbd "<S-return>") 'code-cells-eval-and-step)
  ;; Overriding other minor mode bindings requires some insistence...
  (define-key map [remap jupyter-eval-line-or-region] 'code-cells-eval))

;; jupyter
;; `jupyter-run-repl' でJupyter REPLのバッファを開く
;; カレントバッファを既存のJupyter REPLバッファに関連付けるには `jupyter-repl-associate-buffer'
(use-package jupyter
  :ensure t
  :bind (:map jupyter-repl-interaction-mode-map ("C-c C-c" . jupyter-eval-line-or-region))
  :config
  (with-eval-after-load 'jupyter-org-client
    (unbind-key "C-c h" 'jupyter-org-interaction-mode-map))
  (with-eval-after-load 'jupyter-org-extensions
    (unbind-key "C-c h" 'jupyter-org-interaction-mode-map))

  ;; jupyter kernelspec list --json に余計な文字列が混ざる問題への対処
  ;; https://github.com/emacs-jupyter/jupyter/issues/446
  (setenv "PYDEVD_DISABLE_FILE_VALIDATION" "1")

  ;; jupyterを追加
  ;; Note, jupyter should be added as the last element when loading languages since it depends
  ;; on the values of variables such as org-src-lang-modes and org-babel-tangle-lang-exts
  (org-babel-do-load-languages
   'org-babel-load-languages
   (append org-babel-load-languages '((jupyter . t))))

  ;; jupyterのバッファを、ウィンドウが1つの場合は右側に表示し、既に分割されている場合は下側に表示する
  ;; カスタム表示関数
  (defun my-jupyter-display-buffer-function (buffer alist)
    "Display BUFFER according to ALIST.
  If the frame has only one window, display the buffer on the right side.
  If the frame is already split horizontally, display the buffer below."
    (let* ((window (selected-window))
           (windows (window-list))
           (split-type (if (= (length windows) 1)
                           'right
                         'below)))
      (display-buffer-in-direction
       buffer
       (append alist
               `((direction . ,split-type)
                 (window-height . 0.3)  ; 下に表示する場合の高さ (30%)
                 (window-width . 0.5)   ; 右に表示する場合の幅 (50%)
                 )))))

  ;; jupyter-display-current-buffer-reuse-window の動作をカスタマイズ
  (defun my-jupyter-display-current-buffer-reuse-window (&optional msg-type alist &rest _actions)
    "Modified version of jupyter-display-current-buffer-reuse-window.
  Displays buffer on the right if frame has one window, below if already split."
    (let* ((jupyter-pop-up-frame (jupyter-pop-up-frame-p msg-type))
           (pop-up-frames (and jupyter-pop-up-frame 'graphic-only))
           (pop-up-windows (not jupyter-pop-up-frame))
           (display-buffer-base-action
            (cons
             '(display-buffer-reuse-window
               my-jupyter-display-buffer-function)
             alist)))
      (display-buffer (current-buffer))))

  ;; アドバイス関数として設定
  (advice-add 'jupyter-display-current-buffer-reuse-window
              :override #'my-jupyter-display-current-buffer-reuse-window)

  )

;; Ruff formatter/linter
(use-package lazy-ruff
  :ensure t
  :bind (("C-c e f" . lazy-ruff-lint-format-dwim)) ;; keybinding
  :config
  (lazy-ruff-global-mode t)) ;; Enable the lazy-ruff minor mode globally

;; pythonic
;; Pythonの仮想環境設定コマンド（pythonic-activate, pythonic-deactivate）を提供する 
(use-package pythonic
  :ensure t)

;; Automatically insert NumPy style docstrings in Python function definitions.
(use-package numpydoc
  :ensure t
  :defer t
  :bind (:map python-mode-map
              ("C-c C-n" . numpydoc-generate)))


(provide 'setup-python)
