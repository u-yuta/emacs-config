;;; setup-ai.el --- AI tools and utilities configurations -*- lexical-binding: t -*

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

;; agent-shell
;; A native Emacs shell to interact with LLM agents powered by ACP (Agent Client Protocol)
;; `agent-shell' depends on `shell-maker' and `acp'
(use-package acp
  :ensure t)
(use-package shell-maker
  :ensure t)
(use-package agent-shell
  :ensure t
  :vc (:url "https://github.com/xenodium/agent-shell" :rev "6eae9d8")
  :config
  (setopt agent-shell-openai-codex-environment
          (agent-shell-make-environment-variables :inherit-env t))
  (setopt agent-shell-prefer-viewport-interaction t)
  )

;; mcp.el
(use-package mcp-hub
  :ensure t
  :vc (:url "https://github.com/lizqwerscott/mcp.el" :rev :newest)
  :config
  (setq mcp-hub-servers
        `(("filesystem"
           . (:command "npx" :args ("-y" "@modelcontextprotocol/server-filesystem" ,(file-name-concat (getenv "HOME") "Documents" "AI"))))
          ("fetch" . (:command "uvx" :args ("mcp-server-fetch")))
          ("mcp-pandoc" . (:command "uvx" :args ("mcp-pandoc")))
          ("duckduckgo" . (:command "uvx" :args ("duckduckgo-mcp-server")))
          ("context7" . (:command "npx" :args ("-y" "@upstash/context7-mcp@latest")))
          ("markitdown" .
           (:command "docker"
                     :args ("run" "--rm" "-i" "-v"
                            ,(concat (file-name-concat (getenv "HOME") "Documents/AI") ":/workdir")
                            "markitdown-mcp:latest")))
          ("holoviz" .
           (:command "uvx"
                     :args ("--from"
                            "git+https://github.com/MarcSkovMadsen/holoviz-mcp[panel-extensions]"
                            "holoviz-mcp" ))
           )))
  )

(provide 'setup-ai)
