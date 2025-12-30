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

;; gptel: Interact with LLMs
(use-package gptel
  :ensure t
  :vc (:url "https://github.com/karthink/gptel" :rev "b672649") ;; 2025-12-28 latest
  :bind ("C-c <return>" . gptel-menu)  ;; 確認のためgptel-sendではなくgptel-menuを割当
  :config
  (setopt gptel-model 'Novita:openai/gpt-oss-120b)  ;; default model
  (setopt gptel-default-mode 'org-mode)  ;; default model

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
  ;; OpenRouter
  (gptel-make-openai "OpenRouter"  ;; Any name you want
    :host "openrouter.ai"
    :endpoint "/api/v1/chat/completions"
    :stream t
    :key #'(lambda () (uy/get-auth-secret "openrouter.ai"))
    :models '(google/gemini-2.5-pro
              google/gemini-2.5-flash
              openai/gpt-5
              qwen/qwen3-coder  ;; Qwen3-Coder-480B-A35B-Instruct
              z-ai/glm-4.5-air:free
              ))
  ;; Novita AI
  (gptel-make-openai "Novita"     ;Any name you want
    :host "api.novita.ai"
    :endpoint "/v3/openai/chat/completions"
    :stream t
    :key #'(lambda () (uy/get-auth-secret "novita.ai"))
    :models '(;; qwen/qwen3-4b-fp8
              qwen/qwen3-235b-a22b-instruct-2507
              qwen/qwen3-coder-480b-a35b-instruct
              deepseek/deepseek-v3-0324
              deepseek/deepseek-r1-0528
              ;; deepseek/deepseek-r1-distill-llama-8b
              meta-llama/llama-4-scout-17b-16e-instruct
              openai/gpt-oss-120b
              openai/gpt-oss-20b
              zai-org/glm-4.5
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
          ))

  ;; gptel tool: file/Read file contents
  (gptel-make-tool
   :name "read_file"
   :function (lambda (filepath)
               (let ((expanded-path (expand-file-name filepath)))
                 (unless (file-exists-p expanded-path)
                   (error "File does not exist: %s" expanded-path))
                 (with-temp-buffer
                   (insert-file-contents expanded-path)
                   (buffer-string))))
   :description "Read and return the contents of a file"
   :args (list '(:name "filepath"
                       :type string
                       :description "The full path to the file to read"))
   :category "filesystem")
  ;; gptel tool: file/List directory
  (gptel-make-tool
   :function (lambda (directory)
	       (mapconcat #'identity
			  (directory-files directory)
			  "\n"))
   :name "list_directory"
   :description "List the contents of a given directory"
   :args (list '(:name "directory"
		       :type string
		       :description "The path to the directory to list"))
   :category "filesystem")

  ;; gptel tool: file/Create new file
  (gptel-make-tool
   :name "create_file"
   :function (lambda (path filename content)
               (let ((full-path (expand-file-name filename path)))
                 (when (file-exists-p full-path)
                   (error "File already exists: %s" full-path))
                 (unless (file-directory-p path)
                   (make-directory path t))
                 (with-temp-buffer
                   (insert content)
                   (write-file full-path))
                 (format "Successfully created file %s in %s" filename path)))
   :description "Create a new file with the specified content"
   :args (list '(:name "path"
                       :type string
                       :description "The directory where the file will be created")
               '(:name "filename"
                       :type string
                       :description "The name of the file to create")
               '(:name "content"
                       :type string
                       :description "The content to write to the file"))
   :category "filesystem")

  ;; gptel tool: file/Edit existing file
  (gptel-make-tool
   :name "edit_file"
   :function (lambda (filepath content)
               (let ((expanded-path (expand-file-name filepath)))
                 (unless (file-exists-p expanded-path)
                   (error "File does not exist: %s" expanded-path))
                 (with-temp-buffer
                   (insert content)
                   (write-file expanded-path))
                 (format "Successfully edited file: %s" expanded-path)))
   :description "Edit an existing file by replacing its content"
   :args (list '(:name "filepath"
                       :type string
                       :description "The full path to the file to edit")
               '(:name "content"
                       :type string
                       :description "The new content to replace the file with"))
   :category "filesystem")

  ;; gptel tool: web/Read webpage
  ;; Returns plain text content extracted from HTML
  (gptel-make-tool
   :name "read_webpage"
   :function (lambda (url &optional max-chars)
               (condition-case err
                   (let* ((max-length (or max-chars 3000))
                          (buffer (url-retrieve-synchronously url t t 15)))
                     
                     (if (not buffer)
                         (format "Error: Failed to retrieve content from %s" url)
                       
                       (with-current-buffer buffer
                         (goto-char (point-min))
                         
                         ;; Check HTTP status
                         (unless (re-search-forward "^HTTP/[0-9.]+ 200" nil t)
                           (kill-buffer)
                           (error "HTTP request failed for %s" url))
                         
                         ;; Skip to body
                         (re-search-forward "\n\n" nil t)
                         
                         ;; Parse HTML and render as text
                         (let* ((dom (libxml-parse-html-region (point) (point-max)))
                                (rendered-buffer (generate-new-buffer " *shr-render*"))
                                (content ""))
                           
                           ;; Render DOM to plain text using shr
                           (with-current-buffer rendered-buffer
                             (shr-insert-document dom)
                             (setq content (buffer-substring-no-properties 
                                            (point-min) 
                                            (min (+ (point-min) max-length) 
                                                 (point-max))))
                             (kill-buffer))
                           
                           (kill-buffer buffer)
                           
                           (if (< (length content) 50)
                               (format "Error: Retrieved content from %s is too short or empty" url)
                             (format "Content from %s (first %d characters):\n\n%s\n\n[Content truncated. Full page at: %s]"
                                     url
                                     (length content)
                                     content
                                     url))))))
                 
                 (error (format "Error reading webpage %s: %s" url (error-message-string err)))))
   
   :description "Fetch and read webpage content as plain text (HTML tags removed)"
   :args (list '(:name "url"
                       :type string
                       :description "URL of the webpage to read")
               '(:name "max-chars"
                       :type integer
                       :description "Maximum characters to return (default 3000)"
                       :optional t))
   :category "web")

  ;; gptel tool: web/Fetch raw HTML
  (gptel-make-tool
   :name "fetch_webpage_html"
   :function (lambda (url &optional max-chars)
               (condition-case err
                   (let* ((max-length (or max-chars 2000))
                          (buffer (url-retrieve-synchronously url t t 15)))
                     
                     (if (not buffer)
                         (format "Error: Failed to retrieve HTML from %s" url)
                       
                       (with-current-buffer buffer
                         (goto-char (point-min))
                         
                         ;; Check HTTP status
                         (unless (re-search-forward "^HTTP/[0-9.]+ 200" nil t)
                           (kill-buffer)
                           (error "HTTP request failed for %s" url))
                         
                         ;; Skip to body and get HTML
                         (re-search-forward "\n\n" nil t)
                         (let ((html (buffer-substring-no-properties 
                                      (point) 
                                      (min (+ (point) max-length) (point-max)))))
                           (kill-buffer)
                           
                           (format "HTML from %s (first %d characters):\n\n%s\n\n[HTML truncated]"
                                   url
                                   (length html)
                                   html)))))
                 
                 (error (format "Error fetching HTML from %s: %s" url (error-message-string err)))))
   
   :description "Fetch raw HTML content from a webpage (not rendered, includes HTML tags)"
   :args (list '(:name "url"
                       :type string
                       :description "URL of the webpage to fetch")
               '(:name "max-chars"
                       :type integer
                       :description "Maximum characters to return (default 2000)"
                       :optional t))
   :category "web")

  ;; gptel tool: web/Search Wikipedia
  (gptel-make-tool
   :name "search_wikipedia"
   :function (lambda (query)
               (condition-case err
                   (let* ((search-url (format "https://en.wikipedia.org/w/api.php?action=opensearch&search=%s&limit=5&format=json" 
                                              (url-hexify-string query)))
                          (buffer (url-retrieve-synchronously search-url t t 10)))
                     (if (not buffer)
                         (format "Error: Failed to retrieve Wikipedia results for '%s'" query)
                       (with-current-buffer buffer
                         (goto-char (point-min))
                         (re-search-forward "\n\n" nil t)
                         (let* ((json-object-type 'plist)
                                (json-array-type 'list)
                                (json-key-type 'keyword)
                                (data (json-read))
                                (titles (nth 1 data))
                                (descriptions (nth 2 data))
                                (urls (nth 3 data))
                                (results ""))
                           (kill-buffer)
                           (dotimes (i (min 5 (length titles)))
                             (setq results 
                                   (concat results
                                           (format "%d. %s\n   %s\n   %s\n\n"
                                                   (1+ i)
                                                   (nth i titles)
                                                   (nth i descriptions)
                                                   (nth i urls)))))
                           (if (string-empty-p results)
                               (format "No Wikipedia results found for '%s'" query)
                             (format "Wikipedia search results for '%s':\n\n%s" query results))))))
                 (error (format "Error during Wikipedia search: %s" (error-message-string err)))))
   :description "Search Wikipedia and return formatted results with titles, descriptions and URLs"
   :args (list '(:name "query"
                       :type string
                       :description "Search query for Wikipedia"))
   :category "web")

  ;; gptel tool: Get full Wikipedia article content
  (gptel-make-tool
   :name "get_wikipedia_article"
   :function (lambda (title)
               (condition-case err
                   (let* ((api-url (format "https://en.wikipedia.org/w/api.php?action=query&prop=extracts&exintro=1&explaintext=1&titles=%s&format=json"
                                           (url-hexify-string title)))
                          (buffer (url-retrieve-synchronously api-url t t 10)))
                     (if (not buffer)
                         (format "Error: Failed to retrieve Wikipedia article for '%s'" title)
                       (with-current-buffer buffer
                         (goto-char (point-min))
                         (re-search-forward "\n\n" nil t)
                         (let* ((json-object-type 'plist)
                                (json-array-type 'list)
                                (json-key-type 'keyword)
                                (data (json-read))
                                (pages (plist-get (plist-get data :query) :pages))
                                (page (cadr pages))  ; Get first page
                                (extract (plist-get page :extract))
                                (page-title (plist-get page :title)))
                           (kill-buffer)
                           (if (not extract)
                               (format "No article found for '%s'" title)
                             (format "Wikipedia article: %s\n\n%s\n\n(This is the introduction section only. For full article, visit: https://en.wikipedia.org/wiki/%s)"
                                     page-title
                                     (substring extract 0 (min 2000 (length extract)))
                                     (url-hexify-string title)))))))
                 (error (format "Error retrieving Wikipedia article: %s" (error-message-string err)))))
   :description "Get the full introduction text of a Wikipedia article by title"
   :args (list '(:name "title"
                       :type string
                       :description "Exact title of the Wikipedia article to retrieve"))
   :category "web")

  ;; gptel tool: Emacs document
  (gptel-make-tool
   :name "get_emacs_function_doc"
   :function (lambda (function-name)
               (save-window-excursion  ;; keep window layout
                 (describe-function (intern function-name))
                 (with-current-buffer "*Help*"
                   (let ((result (buffer-string)))
                     (kill-buffer)
                     result))))
   :description "Get the documentation for an Emacs Lisp function/commands."
   :args (list '(:name "function-name"
                       :type string
                       :description "Name of the Emacs Lisp function/commands to look up"))
   :category "emacs")
  (gptel-make-tool
   :name "get_emacs_variable_doc"
   :function (lambda (variable-name)
               (save-window-excursion  ;; keep window layout
                 (describe-variable (intern variable-name))
                 (with-current-buffer "*Help*"
                   (let ((result (buffer-string)))
                     (kill-buffer)
                     result))))
   :description "Get the documentation for an Emacs Lisp variable."
   :args (list '(:name "variable-name"
                       :type string
                       :description "Name of the Emacs Lisp variable to look up"))
   :category "emacs")
  (gptel-make-tool
   :name "search_emacs_symbol"
   :function (lambda (pattern-regexp)
               (save-window-excursion  ;; keep window layout
                 (apropos pattern-regexp)
                 (with-current-buffer "*Apropos*"
                   (let ((result (buffer-string)))
                     (kill-buffer)
                     result))))
   :description "Search for Emasc Lisp symbols that matches PATTERN-REGEXP,
and get the brief despriptions.
Useful for discovering functions/commands/variables related to specific topic or feature."
   :args (list '(:name "pattern-regexp"
                       :type string
                       :description "regexp pattern to search Emacs lisp symbols"))
   :category "emacs")
  (gptel-make-tool
   :name "search_emacs_function"
   :function (lambda (pattern-regexp)
               (save-window-excursion  ;; keep window layout
                 (apropos-function pattern-regexp)
                 (with-current-buffer "*Apropos*"
                   (let ((result (buffer-string)))
                     (kill-buffer)
                     result))))
   :description "Search for Emasc Lisp functions that matches PATTERN-REGEXP,
and get the brief despriptions.
Useful for discovering functions/commands related to specific topic or feature."
   :args (list '(:name "pattern-regexp"
                       :type string
                       :description "regexp pattern to search Emacs lisp symbols"))
   :category "emacs")
  (gptel-make-tool
   :name "search_emacs_variable"
   :function (lambda (pattern-regexp)
               (save-window-excursion  ;; keep window layout
                 (apropos-variable pattern-regexp)
                 (with-current-buffer "*Apropos*"
                   (let ((result (buffer-string)))
                     (kill-buffer)
                     result))))
   :description "Search for Emasc Lisp variables that matches PATTERN-REGEXP,
and get the brief despriptions.
Useful for discovering variables related to specific topic or feature."
   :args (list '(:name "pattern-regexp"
                       :type string
                       :description "regexp pattern to search Emacs lisp symbols"))
   :category "emacs")
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

;; aidermacs
(use-package aidermacs
  :ensure t
  :vc (:url "https://github.com/u-yuta/aidermacs" :rev :newest)
  :bind (("C-c d" . aidermacs-transient-menu))

  :config
  ;; Enable minor mode for Aider files
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
