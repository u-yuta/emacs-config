;; -*- lexical-binding: t -*-

;; Built-in Python utilities
(use-package python
  :ensure nil
  :bind ;(:map python-mode-map ("C-c C-c" . python-shell-send-region))
  :config
  ;; Remove guess indent python message
  (setq python-indent-guess-indent-offset-verbose nil)
  ;; Use IPython when available or fall back to regular Python 
  (cond
   ((executable-find "ipython")
    (progn
      (setq python-shell-buffer-name "IPython")
      (setq python-shell-interpreter "ipython")
      (setq python-shell-interpreter-args "-i --simple-prompt")))
   ((executable-find "python3")
    (setq python-shell-interpreter "python3"))
   ((executable-find "python2")
    (setq python-shell-interpreter "python2"))
   (t
    (setq python-shell-interpreter "python")))
  
  ;; REPLで補完文字列がバッファに挿入される問題の対策
  (with-eval-after-load 'corfu
  (add-hook 'inferior-python-mode-hook
            (lambda ()
              ;; バッファローカル変数として corfu-auto を nil に設定
              (setq-local corfu-auto nil))))
  )

;; REPLで画像・数式表示
(use-package comint-mime
  :ensure t
  :config
  ;; (when (executable-find "ipython3")
  ;;   (setq python-shell-interpreter "ipython3"
  ;;         python-shell-interpreter-args "--simple-prompt --classic"))
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
  (with-eval-after-load 'code-cells
    (let ((map code-cells-mode-map))
      (define-key map (kbd "M-p") 'code-cells-backward-cell)
      (define-key map (kbd "M-n") 'code-cells-forward-cell)
      (define-key map (kbd "C-c C-c") 'code-cells-eval)
      (define-key map (kbd "<C-return>") 'code-cells-eval-and-step)
      ;; Overriding other minor mode bindings requires some insistence...
      (define-key map [remap jupyter-eval-line-or-region] 'code-cells-eval))))

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

;; pyvenv-activate queries the user for a virtual environment directory
;; to activate
;; 
;; pyvenv-workon queries for a virtual environment in $WORKON_HOME (from
;; virtualenvwrapper.sh)
(use-package pyvenv
  :ensure t
  :config
  (if (file-directory-p "~/.pyenv/versions")
      (setenv "WORKON_HOME" "~/.pyenv/versions")
    )
  (defun uy/pyvenv-activate-project-venv ()
    "Activate `projet-root/.venv` with pyvenv-activate."
    (interactive)
    (let
        ((project-venv-directory (expand-file-name ".venv" (project-root (project-current)))))
      (progn
        (pyvenv-activate project-venv-directory)
        (message (concat "Activate venv at " project-venv-directory))))))

;; Automatically insert NumPy style docstrings in Python function definitions.
(use-package numpydoc
  :ensure t
  :defer t
  :bind (:map python-mode-map
              ("C-c C-n" . numpydoc-generate)))


(provide 'setup-python)
