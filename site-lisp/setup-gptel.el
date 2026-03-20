;;; setup-gptel.el --- gptel.el configurations -*- lexical-binding: t -*

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

;;; gptel tool setup functions
(defun my/gptel-setup-journaling-tools ()
  (gptel-make-tool
   :name "get_current_datetime"
   :function (lambda () (current-time-string))
   :description "Get the current local time."
   :category "journal"))

;; Macher: A project-aware LLM editing toolset for Emacs, built on gptel.
(use-package macher
  :ensure t
  :custom
  ;; The org UI has structured navigation and nice content folding.
  (macher-action-buffer-ui 'org)

  :config
  ;; Recommended - register macher tools and presets with gptel.
  (macher-install)

  ;; Recommended - enable macher infrastructure for tools/prompts in
  ;; any buffer.  (Actions and presets will still work without this.)
  (macher-enable)

  ;; Adjust buffer positioning to taste.
  ;; (add-to-list
  ;;  'display-buffer-alist
  ;;  '("\\*macher:.*\\*"
  ;;    (display-buffer-in-side-window)
  ;;    (side . bottom)))
  ;; (add-to-list
  ;;  'display-buffer-alist
  ;;  '("\\*macher-patch:.*\\*"
  ;;    (display-buffer-in-side-window)
  ;;    (side . right)))
  )

;; gptel: Interact with LLMs
(use-package gptel
  :ensure t
  :vc (:url "https://github.com/karthink/gptel" :rev "b34a135") ;; 2026-02-12 latest
  :bind ("C-c <return>" . gptel-menu)  ;; 確認のためgptel-sendではなくgptel-menuを割当
  :config
  (require 'macher nil t)
  (setopt gptel-model 'Novita:openai/gpt-oss-120b)  ;; default model
  (setopt gptel-default-mode 'org-mode)  ;; default model
  (setopt gptel-track-media t)
  (setopt gptel-expert-commands t) ;; display additional options in the transient menu 

  ;; gptel-modeでRETしても送信しない
  (define-key gptel-mode-map (kbd "RET") nil)

  ;; OpenAIのモデルはデフォルトで ChatGPT:<model> として使える 
  ;; API key は gptelのマニュアルの Securing API keys with authinfo に従って設定

  ;; Gemini
  (gptel-make-gemini "Gemini"
    :key #'(lambda () (my/get-auth-secret "generativelanguage.googleapis.com"))
    :stream t)
  ;; DeepSeek
  (gptel-make-openai "DeepSeek"       ;Any name you want
    :host "api.deepseek.com"
    :endpoint "/chat/completions"
    :stream t
    :key #'(lambda () (my/get-auth-secret "api.deepseek.com"))
    :models '(deepseek-chat deepseek-coder))
  ;; OpenRouter
  (gptel-make-openai "OpenRouter"  ;; Any name you want
    :host "openrouter.ai"
    :endpoint "/api/v1/chat/completions"
    :stream t
    :key #'(lambda () (my/get-auth-secret "openrouter.ai"))
    :models '(google/gemini-2.5-pro
              google/gemini-2.5-flash
              openai/gpt-5.2 openai/gpt-5.3-codex
              qwen/qwen3-coder  ;; Qwen3-Coder-480B-A35B-Instruct
              z-ai/glm-4.5-air:free
              ))

  ;; Novita AI
  (gptel-make-openai "Novita"     ;Any name you want
    :host "api.novita.ai"
    :endpoint "/v3/openai/chat/completions"
    :stream t
    :key #'(lambda () (my/get-auth-secret "novita.ai"))
    :models '(
              qwen/qwen3.5-397b-a17b
              qwen/qwen3.5-35b-a3b
              qwen/qwen3-coder-next
              qwen/qwen3-235b-a22b-instruct-2507
              qwen/qwen3-next-80b-a3b-instruct
              deepseek/deepseek-v3-0324
              deepseek/deepseek-v3.2
              deepseek/deepseek-r1-0528
              minimax/minimax-m2.5
              openai/gpt-oss-120b
              openai/gpt-oss-20b
              zai-org/glm-5
              ))
  ;; Novita AI (Qwen 3.5 non-thinking mode)
  ;; https://huggingface.co/Qwen/Qwen3.5-397B-A17B#instruct-or-non-thinking-mode
  (gptel-make-openai "Novita(nothink)"
    :host "api.novita.ai"
    :endpoint "/v3/openai/chat/completions"
    :stream t
    :key #'(lambda () (my/get-auth-secret "novita.ai"))
    :models '(
              qwen/qwen3.5-397b-a17b
              qwen/qwen3.5-35b-a3b
              zai-org/glm-5
              )
    :request-params '(:enable_thinking :json-false))

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
          ))

  ;; Setup gptel tools
  (my/gptel-setup-journaling-tools))

;; gptel-agent
(use-package gptel-agent
  :vc (:url "https://github.com/karthink/gptel-agent" :rev "2e6ba04")  ;; 2026-02-12 latest
  :ensure t
  :config
  (add-to-list 'gptel-agent-dirs "~/.emacs.d/agents")
  (gptel-agent-update)  ;; Read files from agents directories
  )         

;; gptel-quick
;; 
;; Show a short summary or explanation of the word at point, or an active region, in a popup.
;; When the popup is active,
;; - press + to get a longer summary,
;; - M-w (or kill-ring-save) to copy the response,
;; - or C-g (or keyboard-quit) to clear it.
(use-package gptel-quick
  :ensure t
  :vc (:url "https://github.com/karthink/gptel-quick" :rev :newest)
  :config
  (with-eval-after-load 'embark (keymap-set embark-general-map "?" #'gptel-quick))
  (setopt gptel-quick-system-message
          (lambda (count)
            (format "日本語で%d語以内で説明せよ。ですます調ではなく常体で答えること。" count)))

  
  (defun gptel-quick (query-text &optional count)
    "Explain or summarize region or thing at point with an LLM.
  
  QUERY-TEXT is the text being explained.  COUNT is the approximate
  word count of the response."
    (interactive
     (list (cond
            ((use-region-p) (buffer-substring-no-properties (region-beginning)
                                                            (region-end)))
            ((and (derived-mode-p 'pdf-view-mode)
                  (pdf-view-active-region-p))
             (mapconcat #'identity (pdf-view-active-region-text) "\n\n"))
            (t (thing-at-point 'sexp)))
           current-prefix-arg))
    
    (when (xor gptel-quick-backend gptel-quick-model)
      (error "gptel-quick-backend and gptel-quick-model must be both set or unset"))
    
    (let* ((count (or count gptel-quick-word-count))
           (gptel-max-tokens (floor (+ (sqrt (length query-text))
                                       (* count 2.5))))
           (gptel-use-curl t)  ;; Multibyte text によるエラー回避のため、 gptel-use-curl t とする
           (gptel-use-context (and gptel-quick-use-context 'system))
           (gptel-backend (or gptel-quick-backend gptel-backend))
           (gptel-model (or gptel-quick-model gptel-model)))
      (gptel-request query-text
        :system (funcall gptel-quick-system-message count)
        :context (list query-text count
                       (posn-at-point (and (use-region-p) (region-beginning))))
        :callback #'gptel-quick--callback-posframe)))
  )

;; gptel-magit
(use-package gptel-magit
  :ensure t
  :hook (magit-mode . gptel-magit-install)
  :after markdown-mode
  :config
  ;; 自動でフォーマット（fill-region） が行われるのを防ぐ
  (defun gptel-magit--format-commit-message (message)
    "Bypass formatting"
    ;; message をそのまま返す
    message)
  )


(provide 'setup-gptel)
