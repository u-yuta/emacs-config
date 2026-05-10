(require 'org-roam-ql)
(require 'cl-lib)
(require 'dash)
(require 'hierarchy)

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

(defun note-manager--format-node-display (node)
  "Format NODE like `${title:80} ${context:30} ${tags:10} ${kind:10}`."
  (let* ((title (or (org-roam-node-title node) ""))
         (context (or (org-roam-node-context node) ""))
         (tags (mapconcat #'identity (org-roam-node-tags node) ","))
         (kind (or (org-roam-node-kind node) "")))
    (format "%-80s %-30s %-10s %-10s"
            (truncate-string-to-width title 80 nil nil t)
            (truncate-string-to-width context 30 nil nil t)
            (truncate-string-to-width tags 10 nil nil t)
            (truncate-string-to-width kind 10 nil nil t))))

(defun note-manager--format-node-display-with-id (node)
  "Format NODE display with ID suffix."
  (format "%s [%s]"
          (note-manager--format-node-display node)
          (org-roam-node-id node)))

;; org-roamのノード検索を行い、ID、タイトルと指定したプロパティをplistあるいはJSONで返す
(defun note-manager--property-name->keyword (property-name)
  "Convert PROPERTY-NAME (string) to keyword symbol for plist keys."
  (intern (concat ":"
                  (downcase
                   (replace-regexp-in-string "[^[:alnum:]]+" "-" property-name)))))

(defun note-manager--node-p (obj)
  "Return non-nil when OBJ is an org-roam node object."
  (eq (type-of obj) 'org-roam-node))

(defun note-manager--resolve-nodes (source-or-query)
  "Resolve SOURCE-OR-QUERY to a node list.

- nil: all nodes
- node list: returned as is
- otherwise: resolved with `org-roam-ql-nodes'"
  (cond
   ((null source-or-query) (org-roam-node-list))
   ((and (listp source-or-query)
         (or (null source-or-query)
             (note-manager--node-p (car source-or-query))))
    source-or-query)
   (t
    (org-roam-ql-nodes source-or-query))))

(defun note-manager-nodes-id-title-and-properties (source-or-query properties)
  "Return plist rows of id, title and PROPERTIES from SOURCE-OR-QUERY.

SOURCE-OR-QUERY accepts nil, a node list, or an org-roam-ql query.
PROPERTIES should be a list of property names (strings/symbols).

Return value format:
  ((:id ... :title ... :properties (:prop1 ... :prop2 ...)) ...)."
  (let ((property-names (--map (if (symbolp it) (symbol-name it) it)
                               properties)))
    (--map
     (let ((node-properties (org-roam-node-properties it))
           (properties-plist nil))
       (dolist (prop property-names)
         (setq properties-plist
               (plist-put properties-plist
                          (note-manager--property-name->keyword prop)
                          (alist-get (upcase prop)
                                     node-properties
                                     nil nil #'string-equal))))
       (list :id (org-roam-node-id it)
             :title (org-roam-node-title it)
             :properties properties-plist))
     (note-manager--resolve-nodes source-or-query))))

(defun note-manager-nodes-id-title-and-properties-json (source-or-query properties)
  "Return JSON string from `note-manager-nodes-id-title-and-properties'."
  (require 'json)
  ;; `json-encode' treats a top-level list as an object candidate.
  ;; Convert to vector to force JSON array output.
  (json-encode
   (vconcat
    (note-manager-nodes-id-title-and-properties source-or-query properties))))

;; org-roam-node-display-template用
(cl-defmethod org-roam-node-context ((node org-roam-node))
  (when-let* ((context (cdr (assoc-string "CONTEXT" (org-roam-node-properties node)))))
    (concat "#" context)))
(cl-defmethod org-roam-node-kind ((node org-roam-node))
  (when-let* ((kind (cdr (assoc-string "KIND" (org-roam-node-properties node)))))
    (concat "#" kind)))

;; nodeを開くUI
(defun note-manager-find ()
  "Find and visit an existing Org-roam node (no capture)."
  (interactive)
  (when-let* ((org-roam-node-display-template "${title:80} ${context:30} ${tags:10} ${kind:10}")
              (node (org-roam-node-read nil nil nil t "Node: ")))
    (org-roam-node-visit node)))

(defun note-manager-find-active-task ()
  "Find and visit an active task node (no capture)."
  (interactive)
  (let* ((nodes (org-roam-ql-nodes note-manager-query-active-task))
         (candidates
          (--map (cons (note-manager--format-node-display it)
                       it)
                 nodes))
         (selected
          (completing-read "Active task: "
                           (-map #'car candidates)
                           nil t))
         (selected-node (cdr (assoc selected candidates))))
    (when selected-node
      (org-roam-node-visit selected-node))))

(defun note-manager--node-link (node)
  "Return org id link string for NODE."
  (format "[[id:%s][%s]]" (org-roam-node-id node) (org-roam-node-title node)))

(defun note-manager--node-parent-id (node)
  "Return PARENT_ID property string from NODE, or nil."
  (alist-get "PARENT_ID" (org-roam-node-properties node) nil nil #'string-equal))

(defun note-manager--nodes-by-parent-id (parent-id)
  "Return nodes whose PARENT_ID equals PARENT-ID."
  (org-roam-ql-nodes `(properties "PARENT_ID" ,parent-id)))

(defun note-manager--assert-no-parent-cycle (nodes)
  "Raise `user-error' if NODES contain a PARENT_ID cycle."
  (let* ((id->node (-group-by #'org-roam-node-id nodes))
         (parent-of (lambda (id)
                      (when-let* ((node (car (alist-get id id->node nil nil #'string-equal)))
                                  (pid (note-manager--node-parent-id node))
                                  (present (alist-get pid id->node nil nil #'string-equal)))
                        (declare (ignore present))
                        pid))))
    (dolist (node nodes)
      (let ((start (org-roam-node-id node))
            (seen (make-hash-table :test #'equal))
            (cur (org-roam-node-id node)))
        (while cur
          (setq cur (funcall parent-of cur))
          (when cur
            (when (or (gethash cur seen) (string= cur start))
              (user-error "parent-cycle-detected: %s" cur))
            (puthash cur t seen)))))))

(defun note-manager--build-hierarchy (nodes)
  "Build and return hierarchy object from NODES using PARENT_ID."
  (note-manager--assert-no-parent-cycle nodes)
  (let ((h (hierarchy-new))
        (id-set (-map #'org-roam-node-id nodes)))
    (hierarchy-add-trees
     h nodes
     (lambda (n)
       (let ((pid (note-manager--node-parent-id n)))
         (when (member pid id-set)
           (org-roam-node-from-id pid))))
     nil)
    h))

(defun note-manager--ancestor-nodes (node &optional hierarchy)
  "Return ancestor nodes of NODE by recursively following PARENT_ID.
Nearest parent comes first. Raise user error when parent cycle is detected.
When HIERARCHY is non-nil, use it instead of rebuilding one."
  (let* ((ancestors nil)
         (h (or hierarchy
                (note-manager--build-hierarchy (org-roam-node-list))))
         (cur node))
    (while (setq cur (hierarchy-parent h cur))
      (push cur ancestors))
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
  (:node NODE :ancestors ... :siblings ... :children ... :missions ...)."
  (let* ((node-id (org-roam-node-id node))
         (all (org-roam-node-list))
         (h (note-manager--build-hierarchy all))
         (parent (hierarchy-parent h node))
         (children (hierarchy-children h node))
         (siblings (if parent
                       (--remove (string= (org-roam-node-id it) node-id)
                                 (hierarchy-children h parent))
                     nil))
         (ancestors (note-manager--ancestor-nodes node h))
         (missions (delq nil
                         (mapcar #'org-roam-node-from-id
                                 (note-manager-resolve-mission-ids-from-lineage
                                  node-id)))))
    (list :node node
          :ancestors ancestors
          :siblings siblings
          :children children
          :missions missions)))

(defun note-manager--render-structure (structure)
  "Render STRUCTURE (from `note-manager--collect-structure') to temp buffer."
  (let* ((node (plist-get structure :node))
         (ancestors (plist-get structure :ancestors))
         (siblings (plist-get structure :siblings))
         (children (plist-get structure :children))
         (missions (plist-get structure :missions))
         (buf (get-buffer-create "*note-manager-structure*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-mode)
        (insert (format "* Note structure: %s\n\n" (note-manager--node-link node)))
        (note-manager--insert-node-list "Ancestors (recursive PARENT_ID)" ancestors)
        (note-manager--insert-node-list "Children" children)
        (note-manager--insert-node-list "Siblings" siblings)
        (note-manager--insert-node-list "Missions" missions)
        (goto-char (point-min))
        (read-only-mode 1)))
    (pop-to-buffer buf)))

(defun note-manager-show-structure ()
  "Show minimal note structure around current org-roam node.

Display children, siblings, ancestors, and missions in a temporary org buffer."
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
          (--map (cons (note-manager--format-node-display-with-id it)
                       it)
                 nodes))
         (selected
          (completing-read "Mission: "
                           (-map #'car candidates)
                           nil t))
         (selected-node (cdr (assoc selected candidates)))
         (mission-id (org-roam-node-id selected-node)))
    (save-excursion
      (goto-char (point-min))
      (org-entry-put (point) "MISSION_ID" mission-id))))

(defun note-manager--descendant-ids (node hierarchy)
  "Return all descendant IDs of NODE in HIERARCHY."
  (let ((stack (hierarchy-children hierarchy node))
        (ids nil))
    (while stack
      (let ((cur (pop stack)))
        (push (org-roam-node-id cur) ids)
        (setq stack (append (hierarchy-children hierarchy cur) stack))))
    ids))

(defun note-manager-set-parent-id ()
  "Select a parent node and set file-level PARENT_ID property.

Exclude current node and all its descendants from candidates."
  (interactive)
  (let* ((current (note-manager--current-node-or-error))
         (tasks (org-roam-ql-nodes note-manager-query-task))
         (h (note-manager--build-hierarchy tasks))
         (exclude-ids (cons (org-roam-node-id current)
                            (note-manager--descendant-ids current h)))
         (nodes (--remove (member (org-roam-node-id it) exclude-ids) tasks))
         (candidates (--map (cons (note-manager--format-node-display-with-id it) it)
                            nodes)))
    (unless candidates
      (user-error "No valid parent candidates"))
    (let* ((selected (completing-read "Parent: "
                                      (-map #'car candidates)
                                      nil t))
           (selected-node (cdr (assoc selected candidates)))
           (parent-id (org-roam-node-id selected-node)))
      (save-excursion
        (goto-char (point-min))
        (org-set-property "PARENT_ID" parent-id)))))

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
      (if-let* ((id (org-entry-get (point-min) "ID"))
                (node (org-roam-node-from-id id)))
          node
        (user-error "Current buffer is not an org-roam note with ID"))))

(defun note-manager--current-filetags ()
  "Return current buffer filetags as lowercase string list."
  (if-let* ((entry (assoc "FILETAGS" (org-collect-keywords '("FILETAGS"))))
            (val (cadr entry))
            (raw (cond ((stringp val) val)
                       ((and (listp val) (stringp (car val))) (car val))
                       (t nil))))
      (mapcar #'downcase (split-string raw ":" t))
    nil))

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
    (--map
     (with-current-buffer (find-file-noselect (org-roam-node-file it))
       (save-excursion
         (goto-char (point-min))
         (list :id (org-roam-node-id it)
               :kind (note-manager--normalize-symbol-or-string
                      (alist-get "KIND" (org-roam-node-properties it) nil nil #'string-equal))
               :status (note-manager--normalize-symbol-or-string
                        (alist-get "STATUS" (org-roam-node-properties it) nil nil #'string-equal))
               :filetags (note-manager--current-filetags)
               :title (org-roam-node-title it)
               :path (org-roam-node-file it))))
     (org-roam-ql-nodes query))))

(defun note-manager-get-ancestors (id)
  "Return ancestor ID list for note ID by recursively following PARENT_ID.
Raise error when parent cycle is detected."
  (let ((node (org-roam-node-from-id id)))
    (unless node
      (user-error "Node not found for ID: %s" id))
    (-map #'org-roam-node-id
          (note-manager--ancestor-nodes node))))

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

(defun note-manager-resolve-mission-ids-from-lineage (id)
  "Return deduplicated MISSION_ID list from current node + ancestors for ID.

Keep nearest lineage entries first. Unresolved IDs are not filtered here."
  (let ((node (org-roam-node-from-id id)))
    (unless node
      (user-error "Node not found for ID: %s" id))
    (let* ((lineage (cons node (note-manager--ancestor-nodes node)))
           (mission-ids
            (delq nil
                  (mapcar (lambda (n)
                            (let ((mid (alist-get "MISSION_ID"
                                                  (org-roam-node-properties n)
                                                  nil nil #'string-equal)))
                              (unless (or (null mid) (string= mid ""))
                                mid)))
                          lineage))))
      (cl-remove-duplicates mission-ids :test #'string-equal))))

(defun note-manager-consult-open-related-from-current ()
  "Open a related node selected via consult.

Targets include ancestors, siblings, children, and missions resolved from
current node lineage. Each candidate is prefixed with its relation label."
  (interactive)
  (unless (require 'consult nil t)
    (user-error "consult is required"))
  (let* ((node (note-manager--current-node-or-error))
         (structure (note-manager--collect-structure node))
         (seen (make-hash-table :test #'equal))
         (pairs nil))
    (dolist (entry `(("ancestor" . ,(plist-get structure :ancestors))
                     ("child"    . ,(plist-get structure :children))
                     ("sibling"  . ,(plist-get structure :siblings))
                     ("mission"  . ,(plist-get structure :missions))))
      (let ((label (car entry))
            (nodes (cdr entry)))
        (dolist (n nodes)
          (let ((id (org-roam-node-id n)))
            (unless (gethash id seen)
              (puthash id t seen)
              (push (cons (format "[%s] %s"
                                  label
                                  (note-manager--format-node-display n))
                          n)
                    pairs))))))
    (setq pairs (nreverse pairs))
    (unless pairs
      (user-error "No related node found from current context"))
    (let* ((selected (consult--read (mapcar #'car pairs)
                                    :prompt "Related: "
                                    :require-match t
                                    :sort nil))
           (target (cdr (assoc selected pairs))))
      (when target
        (org-roam-node-visit target)))))

(defalias 'note-manager-consult-open-mission-from-current
  #'note-manager-consult-open-related-from-current)

(defun note-manager--task-active-p (node)
  "Return non-nil when NODE is an active task."
  (string-equal
   (downcase (or (alist-get "STATUS" (org-roam-node-properties node) nil nil #'string-equal)
                 ""))
   "active"))

(defun note-manager--build-task-forest-data ()
  "Build intermediate forest data from KIND=task nodes.

Return plist:
  :nodes            list of task org-roam nodes
  :node-by-id       hash table (id -> node)
  :children-by-id   hash table (parent-id -> child-id list)
  :root-ids         list of root node ids"
  (let* ((nodes (org-roam-ql-nodes note-manager-query-task))
         (node-by-id (make-hash-table :test #'equal))
         (children-by-id (make-hash-table :test #'equal))
         (root-ids nil))
    (dolist (node nodes)
      (puthash (org-roam-node-id node) node node-by-id))
    (dolist (node nodes)
      (let* ((id (org-roam-node-id node))
             (pid (note-manager--node-parent-id node))
             (parent-in-task-set (and pid (gethash pid node-by-id))))
        (if parent-in-task-set
            (puthash pid
                     (append (gethash pid children-by-id) (list id))
                     children-by-id)
          (push id root-ids))))
    (list :nodes nodes
          :node-by-id node-by-id
          :children-by-id children-by-id
          :root-ids (nreverse root-ids))))

(defun note-manager--tree-has-active-task-p (root-id forest-data)
  "Return non-nil when subtree ROOT-ID in FOREST-DATA has any active task."
  (let* ((node-by-id (plist-get forest-data :node-by-id))
         (children-by-id (plist-get forest-data :children-by-id))
         (stack (list root-id))
         (found nil))
    (while (and stack (not found))
      (let* ((id (pop stack))
             (node (gethash id node-by-id)))
        (when (and node (note-manager--task-active-p node))
          (setq found t))
        (setq stack (append (gethash id children-by-id) stack))))
    found))

(defun note-manager--render-task-forest-outline (forest-data)
  "Render FOREST-DATA as text outline.

Only trees that contain at least one active task are rendered."
  (let* ((node-by-id (plist-get forest-data :node-by-id))
         (children-by-id (plist-get forest-data :children-by-id))
         (root-ids (plist-get forest-data :root-ids))
         (lines nil))
    (cl-labels
        ((render-subtree (id depth)
           (when-let* ((node (gethash id node-by-id)))
             (push (format "%s- %s"
                           (make-string (* 2 depth) ? )
                           (note-manager--node-link node))
                   lines)
             (dolist (child-id (gethash id children-by-id))
               (render-subtree child-id (1+ depth))))))
      (dolist (root-id root-ids)
        (when (note-manager--tree-has-active-task-p root-id forest-data)
          (render-subtree root-id 0))))
    (if lines
        (mapconcat #'identity (nreverse lines) "\n")
      "")))

(defun note-manager-task-outline ()
  "Return task forest outline text for KIND=task nodes.

Trees where all nodes are non-active tasks are excluded."
  (note-manager--render-task-forest-outline
   (note-manager--build-task-forest-data)))

(require 'note-manager-validate)

;; Keybindings
(global-set-key (kbd "C-c m f") #'note-manager-find)
(global-set-key (kbd "C-c m s") #'note-manager-show-structure)
(global-set-key (kbd "C-c m t") #'note-manager-find-active-task)
(global-set-key (kbd "C-c m m") #'note-manager-set-mission-id)
(global-set-key (kbd "C-c m r") #'note-manager-consult-open-related-from-current)

(provide 'note-manager)
