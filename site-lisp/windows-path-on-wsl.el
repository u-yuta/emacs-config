;; -*- lexical-binding: t -*-

(defun uy/wsl-to-windows-path (path)  
  "WSL2上でのパスをWindowsのパスに変換する（コマンドwslpathがインストール済みとする）"
  (string-trim (shell-command-to-string (concat "wslpath -w " path))))

;; Windowsパス と UNCパス を使えるようにするための設定（WSL 設定編）- NTEmacs
;; https://w.atwiki.jp/ntemacs/pages/74.html

(provide 'windows-path-on-wsl)

(require 'cl-lib)

(defun set-drvfs-alist ()
  (interactive)
  (setq drvfs-alist
        (mapcar (lambda (x)
                  (setq x (replace-regexp-in-string "|/.\\."
                                                    (lambda (y)
                                                      (format "|//%o." (string-to-char (substring y 2 3))))
                                                    x))
                  (when (string-match "\\(.*\\)|\\(.*?\\)/?$" x)
                    (cons (match-string 1 x) (match-string 2 x))))
                (split-string (concat
                               ;; //wsl$ パス情報の追加
                               (when (>= (string-to-number (nth 1 (split-string operating-system-release "-"))) 18362)
                                 (concat "/|" (shell-command-to-string "wslpath -m /")))
                               (shell-command-to-string
                                "mount | grep 'type drvfs' | sed -r 's/(.*) on (.*) type drvfs .*/\\2\\|\\1/' | sed 's!\\\\!/!g'"))
                              "\n" t))))

(set-drvfs-alist)

(defconst windows-path-style-regexp "\\`\\(.*/\\)?\\([a-zA-Z]:\\\\.*\\|[a-zA-Z]:/.*\\|\\\\\\\\.*\\|//.*\\)")

(defun windows-path-convert-file-name (name)
  (setq name (replace-regexp-in-string windows-path-style-regexp "\\2" name t nil))
  (setq name (replace-regexp-in-string "\\\\" "/" name))
  (let ((case-fold-search t))
    (cl-loop for (mountpoint . source) in drvfs-alist
             if (string-match (concat "^\\(" (regexp-quote source) "\\)\\($\\|/\\)") name)
             return (replace-regexp-in-string "^//" "/" (replace-match mountpoint t t name 1))
             finally return name)))

(defun windows-path-run-real-handler (operation args)
  "Run OPERATION with ARGS."
  (let ((inhibit-file-name-handlers
         (cons 'windows-path-map-drive-hook-function
               (and (eq inhibit-file-name-operation operation)
                    inhibit-file-name-handlers)))
        (inhibit-file-name-operation operation))
    (apply operation args)))

(defun windows-path-map-drive-hook-function (operation name &rest args)
  "Run OPERATION on cygwin NAME with ARGS."
  (windows-path-run-real-handler
   operation
   (cons (windows-path-convert-file-name name)
         (if (stringp (car args))
             (cons (windows-path-convert-file-name (car args))
                   (cdr args))
           args))))

(add-to-list 'file-name-handler-alist
             (cons windows-path-style-regexp
                   'windows-path-map-drive-hook-function))

