;;; init-treesitter.el --- Tree-sitter configurations -*- lexical-binding: t -*-

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


;; M-x treesit-install-language-grammar でインストールする language grammer の設定
;; https://gist.github.com/slewsys/4ee95a67577e4df64d5f716c30420555
;;
;; 以下のコマンドを実行すると、treesit-language-source-alist のすべての要素について
;; treesit-install-language-grammar が実行される
;; (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))

(setq treesit-language-source-alist
   '((bash       "https://github.com/tree-sitter/tree-sitter-bash")
     (c          "https://github.com/tree-sitter/tree-sitter-c/" "master" "src")
     (clojure    "https://github.com/sogaiu/tree-sitter-clojure" "master" "src")
     (cpp        "https://github.com/tree-sitter/tree-sitter-cpp/" "master" "src")
     (cmake      "https://github.com/uyha/tree-sitter-cmake")
     (css        "https://github.com/tree-sitter/tree-sitter-css")
     (elisp      "https://github.com/Wilfred/tree-sitter-elisp")
     (elixir     "https://github.com/elixir-lang/tree-sitter-elixir" "main" "src")
     (erlang     "https://github.com/WhatsApp/tree-sitter-erlang" "main" "src")
     (go         "https://github.com/tree-sitter/tree-sitter-go")
     (haskell    "https://github.com/tree-sitter/tree-sitter-haskell" "master" "src")
     (html       "https://github.com/tree-sitter/tree-sitter-html")
     (java       "https://github.com/tree-sitter/tree-sitter-java" "master" "src")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json       "https://github.com/tree-sitter/tree-sitter-json")
     (julia      "https://github.com/tree-sitter/tree-sitter-julia" "master" "src")
     (lua        "https://github.com/MunifTanjim/tree-sitter-lua" "main" "src")
     (make       "https://github.com/alemuller/tree-sitter-make")
     (markdown   "https://github.com/ikatyang/tree-sitter-markdown")
     (meson      "https://github.com/Decodetalkers/tree-sitter-meson" "master" "src")
     (python     "https://github.com/tree-sitter/tree-sitter-python")
     (ruby       "https://github.com/tree-sitter/tree-sitter-ruby" "master" "src")
     (rust       "https://github.com/tree-sitter/tree-sitter-rust" "master" "src")
     (toml       "https://github.com/tree-sitter/tree-sitter-toml")
     (tsx        "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (yaml       "https://github.com/ikatyang/tree-sitter-yaml")))

;; dockerfile用のlanguage grammerのインストール
;; cd ~/repos
;; git clone https://github.com/camdencheek/tree-sitter-dockerfile
;; make -C tree-sitter-dockerfile
;; cp tree-sitter-dockerfile/libtree-sitter-dockerfile.so ~/.emacs.d/tree-sitter/

;; major-modeを -ts-mode に置き換える設定
(setopt major-mode-remap-alist
        '((yaml-mode . yaml-ts-mode)
          (bash-mode . bash-ts-mode)
          (js2-mode . js-ts-mode)
          (typescript-mode . typescript-ts-mode)
          (json-mode . json-ts-mode)
          (css-mode . css-ts-mode)
          (python-mode . python-ts-mode)
          (conf-toml-mode . toml-ts-mode)))

(provide 'init-treesitter)
