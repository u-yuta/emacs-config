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

;; GPTel
(use-package gptel
  :ensure t
  :vc (:url "https://github.com/karthink/gptel" :rev :newest)
  :bind ("C-c <return>" . gptel-menu)  ;; 確認のためgptel-sendではなくgptel-menuを割当
  :config
  (setopt gptel-model 'gpt-4.1-mini)  ;; default model

  ;; OpenAIのモデルはデフォルトで ChatGPT:<model> として使える 
  ;; API key は gptelのマニュアルの Securing API keys with authinfo に従って設定

  ;; Gemini
  (gptel-make-gemini "Gemini"
    :key #'(lambda () (uy/get-auth-secret "generativelanguage.googleapis.com"))
    :stream t)
  ;; DeepSeek
  (gptel-make-openai "DeepSeek"       ;Any name you want
    :host "api.deepseek.com"
    :endpoint "/chat/completions"
    :stream t
    :key #'(lambda () (uy/get-auth-secret "api.deepseek.com"))
    :models '(deepseek-chat deepseek-coder))
  ;; Novita AI
  (gptel-make-openai "Novita"     ;Any name you want
    :host "api.novita.ai"
    :endpoint "/v3/openai/chat/completions"
    :stream t
    :key #'(lambda () (uy/get-auth-secret "novita.ai"))
    :models '(qwen/qwen3-4b-fp8
              qwen/qwen3-235b-a22b-fp8
              deepseek/deepseek-v3-0324
              meta-llama/llama-3.1-8b-instruct
              meta-llama/llama-3.3-70b-instruct
              meta-llama/llama-4-scout-17b-16e-instruct
              ))
  ;; Ollama
  (gptel-make-ollama "Ollama"             ;Any name of your choosing
    :host "localhost:11434"               ;Where it's running
    :stream t                             ;Stream responses
    ;; List of models
    :models '(gemma:2b
              hf.co/alfredplpl/gemma-2-baku-2b-it-gguf
              Llama-3.1-Swallow-Instruct
              qwen2.5-coder-instruct
              )
    )

  ;; MCP利用
  (require 'gptel-integrations)

  ;; ディレクティブ
  (setq gptel-directives
        '((default     . "You are a large language model living in Emacs and a helpful assistant. Respond concisely.")
          (programming . "You are a large language model and a careful programmer. Provide code and only code as output without any additional text, prompt or note.")
          (writing     . "You are a large language model and a writing assistant. Respond concisely.")
          (chat        . "You are a large language model and a conversation partner. Respond concisely.")
          
          ;; 日本語応答用ディレクティブ
          (default-ja  . "あなたはEmacsに組み込まれた大規模言語モデルであり、役立つアシスタントです。常に日本語で簡潔に応答してください。")
          (summarize-ja . "あなたはEmacsに組み込まれた大規模言語モデルであり、要約のスペシャリストである。提供されたテキストの要点と主要な構造を保ちながら、常に日本語で簡潔に要約すること。である調（常体）で応答すること。")
          (writing-ja  . "あなたはEmacsに組み込まれた大規模言語モデルであり、文章作成のアシスタントである。常に日本語でである調（常体）を使用し、簡潔に応答すること。")
          (brainstorm-ja . '("あなたはEmacsに組み込まれた大規模言語モデルであり、創造的なブレインストーミングを手伝うアシスタントです。常に日本語で応答してください。"
                             "アイデアをブレインストーミングしましょう。"
                             "創造的なアイデア生成のお手伝いをします。今日はどのようなテーマについて考えていきましょうか？"))
          
          ;; 翻訳用ディレクティブ
          (translate-to-ja . "あなたはEmacsに組み込まれた大規模言語モデルであり、優れた翻訳者です。入力されたテキストを日本語に翻訳してください。原文の意味とニュアンスを正確に保ちながら、自然で流暢な日本語に翻訳してください。翻訳以外の説明は不要です。")
          (translate-to-en . "You are a large language model living in Emacs and a skilled translator. Translate the input text into English. Preserve the original meaning and nuances while producing natural and fluent English. Provide only the translation without explanations.")
          
          ;; 専門家ディレクティブ（日本語のみ）
          (scientist-ja . "あなたはEmacsに組み込まれた大規模言語モデルであり、物理学、数学、工学など複数の科学分野に深い知識を持つ科学者として振る舞います。現在の科学的コンセンサスを反映した正確かつ証拠に基づいた回答を日本語で提供してください。適切な場合は正確な科学用語を使用し、複雑な概念も明確に説明してください。競合する科学理論がある場合はそれらを認識し、特定の科学的事実について不確かな場合は限界を認めてください。")
          (engineer-ja . "あなたはEmacsに組み込まれた大規模言語モデルであり、機械工学、電気工学、精密工学、ソフトウェア工学など様々な工学分野に精通した経験豊富なエンジニアとして振る舞います。実際の制約、基準、ベストプラクティスを考慮した実用的で技術的に正確な回答を日本語で提供してください。適切な工学用語と単位を使用し、回答では安全性、効率性、実現可能性を考慮してください。適切な場合は、異なる工学的アプローチ間のトレードオフについても言及してください。")))
  )

;; aidermacs
(use-package aidermacs
  :ensure t
  :vc (:url "https://github.com/u-yuta/aidermacs" :rev :newest)
  :bind (("C-c d" . aidermacs-transient-menu))

  :config
  ; Enable minor mode for Aider files
  (aidermacs-setup-minor-mode)

  (setopt aidermacs-auto-commits nil)
  (setopt aidermacs-use-architect-mode t)
  (setopt aidermacs-default-model "deepseek/deepseek-chat")

  ;; Use vterm backend
  (setopt aidermacs-backend 'vterm)
  (setopt aidermacs-vterm-multiline-newline-key "S-<return>")
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
          ("markitdown" .
           (:command "docker"
            :args ("run" "--rm" "-i" "-v"
                            ,(concat (file-name-concat (getenv "HOME") "Documents/AI") ":/workdir")
                            "markitdown-mcp:latest")))))
  )

(provide 'setup-ai)
