(require 'org-roam-ql)

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
Nearest parent comes first."
  (let ((current node)
        (seen (make-hash-table :test #'equal))
        ancestors)
    (while current
      (let* ((parent-id (note-manager--node-parent-id current))
             (parent (and parent-id (org-roam-node-from-id parent-id))))
        (if (or (null parent)
                (gethash (org-roam-node-id parent) seen))
            (setq current nil)
          (puthash (org-roam-node-id parent) t seen)
          (push parent ancestors)
          (setq current parent))))
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



