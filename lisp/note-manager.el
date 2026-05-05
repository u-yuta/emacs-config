(require 'org-roam-ql)
(require 'cl-lib)

;; 検索クエリ
(defconst note-manager-query-context
  '(or (properties "KIND" "mission") (properties "KIND" "area"))
  "Query for context nodes (mission or area).")

(defconst note-manager-query-mission
  '(properties "KIND" "mission")
  "Query for mission nodes.")

(defconst note-manager-query-area
  '(properties "KIND" "area")
  "Query for area nodes.")

(defconst note-manager-query-task
  '(properties "KIND" "task")
  "Query for task nodes.")

(defconst note-manager-query-active-task
  '(and (properties "KIND" "task") (properties "STATUS" "active"))
  "Query for active task nodes.")


;; org-roamのノード検索を行い、ID、タイトルと指定したプロパティをplistあるいはJSONで返す
(defun note-manager--property-name->keyword (property-name)
  "Convert PROPERTY-NAME (string) to keyword symbol for plist keys."
  (intern (concat ":"
                  (downcase
                   (replace-regexp-in-string "[^[:alnum:]]+" "-" property-name)))))

(defun note-manager-nodes-id-title-and-properties (source-or-query properties)
  "Return plist rows of id, title and PROPERTIES from SOURCE-OR-QUERY.

SOURCE-OR-QUERY is resolved with `org-roam-ql-nodes'.
PROPERTIES should be a list of property names (strings/symbols).

Return value format:
  ((:id ... :title ... :properties (:prop1 ... :prop2 ...)) ...)."
  (let ((property-names (mapcar (lambda (it) (if (symbolp it) (symbol-name it) it))
                                properties)))
    (mapcar
     (lambda (node)
       (let ((node-properties (org-roam-node-properties node))
             (properties-plist nil))
         (dolist (prop property-names)
           (setq properties-plist
                 (plist-put properties-plist
                            (note-manager--property-name->keyword prop)
                            (alist-get (upcase prop)
                                       node-properties
                                       nil nil #'string-equal))))
         (list :id (org-roam-node-id node)
               :title (org-roam-node-title node)
               :properties properties-plist)))
     (org-roam-ql-nodes source-or-query))))

(defun note-manager-nodes-id-title-and-properties-json (source-or-query properties)
  "Return JSON string from `note-manager-nodes-id-title-and-properties'."
  (require 'json)
  ;; `json-encode' treats a top-level list as an object candidate.
  ;; Convert to vector to force JSON array output.
  (json-encode
   (vconcat
    (note-manager-nodes-id-title-and-properties source-or-query properties))))

(defun note-manager--node-link (node)
  "Return org id link string for NODE."
  (format "[[id:%s][%s]]" (org-roam-node-id node) (org-roam-node-title node)))

(defun note-manager--node-parent-id (node)
  "Return PARENT_ID property string from NODE, or nil."
  (alist-get "PARENT_ID" (org-roam-node-properties node) nil nil #'string-equal))

(defun note-manager--nodes-by-parent-id (parent-id)
  "Return nodes whose PARENT_ID equals PARENT-ID."
  (org-roam-ql-nodes `(properties "PARENT_ID" ,parent-id)))

(defun note-manager--ancestor-nodes (node)
  "Return ancestor nodes of NODE by recursively following PARENT_ID.
Nearest parent comes first. Raise user error when parent cycle is detected."
  (let* ((start-id (org-roam-node-id node))
         (current node)
         (seen (make-hash-table :test #'equal))
         ancestors)
    (while current
      (let* ((parent-id (note-manager--node-parent-id current))
             (parent (and parent-id (org-roam-node-from-id parent-id))))
        (cond
         ((null parent)
          (setq current nil))
         ((or (gethash (org-roam-node-id parent) seen)
              (string= (org-roam-node-id parent) start-id))
          (user-error "parent-cycle-detected: %s" (org-roam-node-id parent)))
         (t
          (puthash (org-roam-node-id parent) t seen)
          (push parent ancestors)
          (setq current parent)))))
    (nreverse ancestors)))

(defun note-manager--insert-node-list (title nodes)
  "Insert org section TITLE and bullet list of NODES."
  (insert (format "** %s\n" title))
  (if nodes
      (dolist (node nodes)
        (insert (format "- %s\n" (note-manager--node-link node))))
    (insert "- (none)\n"))
  (insert "\n"))

(defun note-manager--collect-structure (node)
  "Collect minimal structure data around NODE.

Return plist:
  (:node NODE :ancestors ... :parent ... :siblings ... :children ...)."
  (let* ((node-id (org-roam-node-id node))
         (parent-id (note-manager--node-parent-id node))
         (parent (and parent-id (org-roam-node-from-id parent-id)))
         (children (note-manager--nodes-by-parent-id node-id))
         (siblings (if parent-id
                       (seq-filter
                        (lambda (n) (not (string= (org-roam-node-id n) node-id)))
                        (note-manager--nodes-by-parent-id parent-id))
                     nil))
         (ancestors (note-manager--ancestor-nodes node)))
    (list :node node
          :ancestors ancestors
          :parent parent
          :siblings siblings
          :children children)))

(defun note-manager--render-structure (structure)
  "Render STRUCTURE (from `note-manager--collect-structure') to temp buffer."
  (let* ((node (plist-get structure :node))
         (ancestors (plist-get structure :ancestors))
         (parent (plist-get structure :parent))
         (siblings (plist-get structure :siblings))
         (children (plist-get structure :children))
         (buf (get-buffer-create "*note-manager-structure*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-mode)
        (insert (format "* Note structure: %s\n\n" (note-manager--node-link node)))
        (note-manager--insert-node-list "Ancestors (recursive PARENT_ID)" ancestors)
        (note-manager--insert-node-list "Parent" (and parent (list parent)))
        (note-manager--insert-node-list "Siblings" siblings)
        (note-manager--insert-node-list "Children" children)
        (goto-char (point-min))))
    (pop-to-buffer buf)))

(defun note-manager-show-structure ()
  "Show minimal note structure around current org-roam node.

Display parent, children, siblings, and all ancestors in a temporary org buffer."
  (interactive)
  (let* ((node (org-roam-node-at-point))
         (node-id (and node (org-roam-node-id node))))
    (unless node-id
      (user-error "Current buffer/point is not on an org-roam node"))
    (note-manager--render-structure
     (note-manager--collect-structure node))))

(defun note-manager-set-mission-id ()
  "Select a mission node and set file-level MISSION_ID property."
  (interactive)
  (let* ((nodes (org-roam-ql-nodes note-manager-query-mission))
         (candidates
          (mapcar (lambda (node)
                    (cons (format "%s [%s]"
                                  (org-roam-node-title node)
                                  (org-roam-node-id node))
                          node))
                  nodes))
         (selected
          (completing-read "Mission: "
                           (mapcar #'car candidates)
                           nil t))
         (selected-node (cdr (assoc selected candidates)))
         (mission-id (org-roam-node-id selected-node)))
    (save-excursion
      (goto-char (point-min))
      (org-entry-put (point) "MISSION_ID" mission-id))))

(defconst note-manager-valid-kinds '("task" "mission" "area")
  "Allowed KIND values for rev4 notes.")
(defconst note-manager-valid-filetags '("work" "note" "index" "ref" "journal" "data")
  "Allowed filetags for rev4 minimal validation.")
(defconst note-manager-valid-task-status '("inbox" "active" "hold" "done" "archived")
  "Allowed STATUS values for task notes.")
(defconst note-manager-valid-mission-status '("active" "hold" "done" "archived")
  "Allowed STATUS values for mission notes.")
(defconst note-manager-task-required-headings
  '("目的" "完了条件" "作業内容" "次のアクション" "メモ・中間結果" "結果・知見" "リンク")
  "Required level-1 headings for task notes.")
(defconst note-manager-mission-required-headings
  '("目的" "背景" "スコープ" "成功条件" "制約" "用語")
  "Required level-1 headings for mission notes.")
(defconst note-manager-area-required-headings
  '("概要" "スコープ" "用語")
  "Required level-1 headings for area notes.")

(defun note-manager--normalize-symbol-or-string (value)
  "Return downcased string for VALUE (symbol/string), else nil.
Treat nil as nil (not as symbol "nil")."
  (cond ((null value) nil)
        ((symbolp value) (downcase (symbol-name value)))
        ((stringp value) (downcase value))
        (t nil)))

(defun note-manager--current-node-or-error ()
  "Return current org-roam node or raise user error."
  (or (org-roam-node-at-point)
      (let* ((id (org-entry-get (point-min) "ID"))
             (node (and id (org-roam-node-from-id id))))
        (or node
            (user-error "Current buffer is not an org-roam note with ID")))))

(defun note-manager--current-filetags ()
  "Return current buffer filetags as lowercase string list."
  (let* ((entry (assoc "FILETAGS" (org-collect-keywords '("FILETAGS"))))
         (val (cadr entry))
         (raw (cond ((stringp val) val)
                    ((and (listp val) (stringp (car val))) (car val))
                    (t nil))))
    (if raw
        (mapcar #'downcase (split-string raw ":" t))
      nil)))

(defun note-manager-current-note-metadata ()
  "Return current note metadata plist for rev4 minimal API."
  (interactive)
  (let* ((node (note-manager--current-node-or-error))
         (props (org-roam-node-properties node))
         (meta (list :id (org-roam-node-id node)
                     :kind (note-manager--normalize-symbol-or-string
                            (alist-get "KIND" props nil nil #'string-equal))
                     :status (note-manager--normalize-symbol-or-string
                              (alist-get "STATUS" props nil nil #'string-equal))
                     :parent-id (alist-get "PARENT_ID" props nil nil #'string-equal)
                     :mission-id (alist-get "MISSION_ID" props nil nil #'string-equal)
                     :filetags (note-manager--current-filetags)
                     :title (org-roam-node-title node)
                     :path (org-roam-node-file node))))
    (when (called-interactively-p 'interactive)
      (message "%S" meta))
    meta))

(cl-defun note-manager-find-notes (&key kind status)
  "Find notes by KIND and STATUS with simple filtering.
KIND/STATUS accept symbol or string."
  (let* ((kind-str (note-manager--normalize-symbol-or-string kind))
         (status-str (note-manager--normalize-symbol-or-string status))
         (query (cond
                 ((and kind-str status-str)
                  `(and (properties "KIND" ,kind-str)
                        (properties "STATUS" ,status-str)))
                 (kind-str `(properties "KIND" ,kind-str))
                 (status-str `(properties "STATUS" ,status-str))
                 (t t))))
    (mapcar
     (lambda (node)
       (with-current-buffer (find-file-noselect (org-roam-node-file node))
         (save-excursion
           (goto-char (point-min))
           (list :id (org-roam-node-id node)
                 :kind (note-manager--normalize-symbol-or-string
                        (alist-get "KIND" (org-roam-node-properties node) nil nil #'string-equal))
                 :status (note-manager--normalize-symbol-or-string
                          (alist-get "STATUS" (org-roam-node-properties node) nil nil #'string-equal))
                 :filetags (note-manager--current-filetags)
                 :title (org-roam-node-title node)
                 :path (org-roam-node-file node)))))
     (org-roam-ql-nodes query))))

(defun note-manager-get-ancestors (id)
  "Return ancestor ID list for note ID by recursively following PARENT_ID.
Raise error when parent cycle is detected."
  (let ((node (org-roam-node-from-id id)))
    (unless node
      (user-error "Node not found for ID: %s" id))
    (let ((current node)
          (seen (make-hash-table :test #'equal))
          ancestors)
      (while current
        (let* ((parent-id (note-manager--node-parent-id current))
               (parent (and parent-id (org-roam-node-from-id parent-id))))
          (cond
           ((null parent)
            (setq current nil))
           ((or (gethash (org-roam-node-id parent) seen)
                (string= (org-roam-node-id parent) id))
            (user-error "parent-cycle-detected: %s" (org-roam-node-id parent)))
           (t
            (puthash (org-roam-node-id parent) t seen)
            (push (org-roam-node-id parent) ancestors)
            (setq current parent)))))
      (nreverse ancestors))))

(defun note-manager-resolve-mission (id)
  "Resolve mission note from task ID via MISSION_ID."
  (let* ((node (org-roam-node-from-id id))
         (props (and node (org-roam-node-properties node)))
         (mission-id (and props (alist-get "MISSION_ID" props nil nil #'string-equal)))
         (mission (and mission-id (org-roam-node-from-id mission-id))))
    (unless node
      (user-error "Node not found for ID: %s" id))
    (unless mission-id
      (user-error "MISSION_ID is not set on task: %s" id))
    (unless mission
      (user-error "unresolved-mission: %s" mission-id))
    (list :id (org-roam-node-id mission)
          :kind "mission"
          :title (org-roam-node-title mission)
          :path (org-roam-node-file mission))))

(defun note-manager--task-heading-titles ()
  "Return level-1 heading titles in current org buffer."
  (org-element-map (org-element-parse-buffer) 'headline
    (lambda (h)
      (when (= (org-element-property :level h) 1)
        (org-element-property :raw-value h)))))

(defun note-manager-validate-current-note ()
  "Validate current note against rev4 minimal constraints.
Return diagnostics list: (:code <symbol> :level <symbol> :message <string>)."
  (interactive)
  (let* ((meta (note-manager-current-note-metadata))
         (id (plist-get meta :id))
         (kind (plist-get meta :kind))
         (status (plist-get meta :status))
         (parent-id (plist-get meta :parent-id))
         (mission-id (plist-get meta :mission-id))
         (filetags (plist-get meta :filetags))
         diags)
    (unless id
      (push '(:code missing-id :level error :message "ID is missing") diags))
    (unless filetags
      (push '(:code missing-filetags :level error :message "filetags is missing") diags))
    (when (and kind (not (member kind note-manager-valid-kinds)))
      (push `(:code invalid-kind :level error :message ,(format "Invalid KIND: %s" kind)) diags))
    (dolist (tag filetags)
      (unless (member tag note-manager-valid-filetags)
        (push `(:code invalid-filetag :level error :message ,(format "Invalid filetag: %s" tag)) diags)))
    (when (and (equal kind "task") (not status))
      (push '(:code missing-task-status :level error :message "STATUS is missing for task") diags))
    (when status
      (let ((ok (cond
                 ((equal kind "task") (member status note-manager-valid-task-status))
                 ((equal kind "mission") (member status note-manager-valid-mission-status))
                 (t t))))
        (unless ok
          (push `(:code invalid-status :level error :message ,(format "Invalid STATUS: %s" status)) diags))))
    (when parent-id
      (unless (org-roam-node-from-id parent-id)
        (push `(:code unresolved-parent :level error :message ,(format "PARENT_ID not found: %s" parent-id)) diags)))
    (when mission-id
      (unless (org-roam-node-from-id mission-id)
        (push `(:code unresolved-mission :level error :message ,(format "MISSION_ID not found: %s" mission-id)) diags)))
    (let* ((present (delq nil (note-manager--task-heading-titles)))
           (required (cond
                      ((equal kind "task") note-manager-task-required-headings)
                      ((equal kind "mission") note-manager-mission-required-headings)
                      ((equal kind "area") note-manager-area-required-headings)
                      (t nil)))
           (missing (and required
                         (seq-remove (lambda (h) (member h present)) required))))
      (when missing
        (push `(:code missing-required-headings :level warn
                      :message ,(format "Missing required headings(%s): %s"
                                        kind
                                        (string-join missing ", ")))
              diags)))
    (when (member kind note-manager-valid-kinds)
      (condition-case err
          (note-manager-get-ancestors id)
        (error
         (when (string-match-p "parent-cycle-detected" (error-message-string err))
           (push `(:code parent-cycle-detected :level error :message ,(error-message-string err)) diags)))))
    (setq diags (nreverse diags))
    (when (called-interactively-p 'interactive)
      (if diags
          (message "%S" diags)
        (message "Validation OK")))
    diags))

(defun note-manager-set-status (status)
  "Set file-level STATUS property.
STATUS accepts symbol/string and is validated by KIND."
  (interactive
   (let* ((meta (note-manager-current-note-metadata))
          (kind (plist-get meta :kind))
          (choices (if (equal kind "mission")
                       note-manager-valid-mission-status
                     note-manager-valid-task-status)))
     (list (completing-read "STATUS: " choices nil t))))
  (let* ((meta (note-manager-current-note-metadata))
         (kind (plist-get meta :kind))
         (status-str (note-manager--normalize-symbol-or-string status))
         (valid (if (equal kind "mission")
                    note-manager-valid-mission-status
                  note-manager-valid-task-status)))
    (unless (member status-str valid)
      (user-error "Invalid STATUS for kind=%s: %s" kind status-str))
    (save-excursion
      (goto-char (point-min))
      (org-entry-put (point) "STATUS" status-str))
    status-str))

