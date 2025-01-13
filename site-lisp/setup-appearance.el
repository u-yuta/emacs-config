;; -*- lexical-binding: t -*-

;; テキストスケール変更の刻みを設定する（標準は 1.2）
(setq text-scale-mode-step 1.1)

;; 左右のマージン幅
(setopt left-margin-width 5)
(setopt right-margin-width 5)

(use-package ef-themes
  :ensure t
  :config
  (setq ef-themes-to-toggle '(ef-owl ef-cyprus))
  (setq ef-themes-mixed-fonts t
        ef-themes-variable-pitch-ui t)
  (setq ef-themes-headings ; read the manual's entry or the doc string
        '((0 variable-pitch 1.2)
          (1 variable-pitch 1.2)
          (2 variable-pitch 1.1)
          (3 variable-pitch 1.1) ; absence of weight means `bold'
          (t variable-pitch 1.05)
          ))

  ;; マウスポインターの色をテーマに合わせて変更する
  (defun set-mouse-pointer-color-to-cursor-color ()
    "マウスカーソルの色をカーソルの色と同じにする。"
    (let ((cursor-color (face-attribute 'cursor :background nil t)))
      (when cursor-color
        (set-mouse-color cursor-color))))

  ;; ef-themesのロード後に実行されるようにフックを設定
  (add-hook 'ef-themes-post-load-hook #'set-mouse-pointer-color-to-cursor-color)

  (ef-themes-select 'ef-owl)
  )

;; modeline 
(use-package moody
  :ensure t
  :config
  (moody-replace-mode-line-front-space)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

;; modelineのマイナーモードの表示をまとめてシンプルにする
(use-package minions
  :ensure t
  :config
  (minions-mode))

;; nerd-icons (all-the-iconsから移行)
;; 1. Nerd Fontsをシステムにインストールする https://github.com/rainstormstudio/nerd-icons.el#installing-fonts
;; 2. nerd-icons をインストールする (下記、 (use-package nerd-icons))
(use-package nerd-icons
  :ensure t
  ;; :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  ;; (nerd-icons-font-family "Symbols Nerd Font Mono")
  )

(use-package nerd-icons-dired
  :ensure t
  :hook
  (dired-mode . nerd-icons-dired-mode))

;; colorful icons for completion
(use-package kind-icon
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; Display emojis
(use-package emojify
  :ensure t
  :hook (after-init . global-emojify-mode)
  :config
  ;; unicode, github を対象にする（asciiは除外） 
  (emojify-set-emoji-styles '(unicode github)))

(use-package volatile-highlights
  :ensure t
  :hook
  (after-init . volatile-highlights-mode)
  ;; :custom-face
  ;; (vhl/default-face ((nil (:foreground "#FF3333" :background "#FFCDCD"))))
  )

(use-package paren
  :ensure t
  :hook
  (after-init . show-paren-mode)
  :custom-face
  ;; (show-paren-match ((nil (:background "#44475a" :foreground "#f1fa8c")))) ;; :box t
  :custom
  (show-paren-style 'mixed)
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t))

;; Visually highlight the selected buffer
(use-package dimmer
  :ensure t
  :init
  (dimmer-configure-which-key)
  :config
  (dimmer-mode t))

(use-package beacon
  :ensure t
  :custom
  (beacon-color "#f1fa8c")
  :hook (after-init . beacon-mode))

(use-package rainbow-delimiters
  :ensure t
  :hook
  (prog-mode . rainbow-delimiters-mode))

(when (not uy/system-windows-p)  ;; Windowsでは遅くなるのでオフ
  (use-package git-gutter
    :ensure t
    :custom
    (git-gutter:modified-sign "~")
    (git-gutter:added-sign    "+")
    (git-gutter:deleted-sign  "-")
    (git-gutter:update-interval 1)
    :config
    (global-git-gutter-mode +1)
    ))

(provide 'setup-appearance)
