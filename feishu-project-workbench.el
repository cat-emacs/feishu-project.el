;;; feishu-project-workbench.el --- Feishu Project workbench -*- lexical-binding: t; -*-

(require 'json)
(require 'url-http)
(require 'feishu-project)
(require 'feishu-project-export)
(defvar url-http-response-status)
(defvar url-http-end-of-headers)
(defvar feishu-project--detail-generation)
(defvar feishu-project--detail-identity)
(defvar feishu-project--detail-sections)
(defvar feishu-project--detail-section-data)
(defvar feishu-project--filters)

(defcustom feishu-project-saved-queries nil
  "Versioned saved query descriptors."
  :type '(alist :key-type string :value-type sexp)
  :group 'feishu-project)

(defun feishu-project-workbench--require-action ()
  "Require an action-capable backend."
  (unless (plist-get (feishu-project--backend) :action)
    (user-error "Feishu Project action requires an action-capable backend")))

(defun feishu-project-workbench--call (action payload success &optional failure backend)
  "Call ACTION with callbacks through the generic backend seam."
  (feishu-project--action action payload success
                          (or failure (lambda (error) (user-error "%s" error))) backend))

(defun feishu-project-workbench--item ()
  "Return current detail item or list item at point."
  (if (derived-mode-p 'feishu-project-detail-mode)
      (or (car feishu-project--items) (user-error "No Feishu Project detail item"))
    (feishu-project-item-at-point)))

(defun feishu-project-workbench--payload (item)
  "Return locator payload for ITEM."
  (list :project_key (feishu-project--item-project item)
        :work_item_id (feishu-project--item-id item)))

(defun feishu-project-workbench--current-p (buffer generation identity)
  "Return non-nil if detail BUFFER retains GENERATION and IDENTITY."
  (and (buffer-live-p buffer) (with-current-buffer buffer
                                (and (= generation feishu-project--detail-generation)
                                     (equal identity feishu-project--detail-identity)))))

(defun feishu-project-workbench--schema-configs (data)
  "Return field configurations from DATA, accepting single config shapes."
  (let ((configs (or (alist-get 'list data) (alist-get 'fields data) '())))
    (if (and (consp configs) (symbolp (caar configs))) (list configs) configs)))

(defun feishu-project-workbench--schema-has-field-p (data key)
  "Return non-nil when DATA has exact field KEY."
  (cl-some (lambda (entry) (equal key (or (alist-get 'field_key entry) (alist-get 'key entry))))
           (feishu-project-workbench--schema-configs data)))

;;;###autoload
(defun feishu-project-save-query (name)
  "Save current view as NAME."
  (interactive "sSave query as: ")
  (unless feishu-project--query (user-error "Current view has no query"))
  (setf (alist-get name feishu-project-saved-queries nil nil #'equal)
        (list :version 1 :backend feishu-project--backend-name :project feishu-project--project-key
              :kind feishu-project--query-kind :type-keys feishu-project--type-spec
              :query feishu-project--query :filters feishu-project--filters)))

;;;###autoload
(defun feishu-project-load-query (name)
  "Load saved query NAME."
  (interactive (list (completing-read "Query: " (mapcar #'car feishu-project-saved-queries) nil t)))
  (let ((descriptor (alist-get name feishu-project-saved-queries nil nil #'equal)))
    (unless descriptor (user-error "Unknown query %s" name))
    (let ((feishu-project-backend (plist-get descriptor :backend)))
      (feishu-project--display (plist-get descriptor :project) (plist-get descriptor :type-keys)
                               (plist-get descriptor :kind) (plist-get descriptor :query)
                               (plist-get descriptor :filters)))))

;;;###autoload
(defun feishu-project-filter (type status assignee creator)
  "Apply TYPE STATUS ASSIGNEE CREATOR filters to a MCP list." 
  (interactive (list (read-string "Type key/name: " (plist-get feishu-project--filters :type))
                     (read-string "Status: " (plist-get feishu-project--filters :status))
                     (read-string "Assignee: " (plist-get feishu-project--filters :assignee))
                     (read-string "Creator: " (plist-get feishu-project--filters :creator))))
  (feishu-project-workbench--require-action)
  (unless (eq feishu-project--query-kind 'list) (user-error "Use MQL filters for MQL views"))
  (setq-local feishu-project--filters
              (let ((value (list :type (unless (string-empty-p type) type)
                                 :status (unless (string-empty-p status) status)
                                 :assignee (unless (string-empty-p assignee) assignee)
                                 :creator (unless (string-empty-p creator) creator))))
                (and (seq-some #'identity (cl-loop for key in '(:type :status :assignee :creator) collect (plist-get value key))) value)))
  (feishu-project--request-page nil nil))

;;;###autoload
(defun feishu-project-clear-filters ()
  "Clear list filters." (interactive)
  (setq-local feishu-project--filters nil) (feishu-project--request-page nil nil))

;;;###autoload
(defun feishu-project-validate-project (locator)
  "Resolve project LOCATOR through MCP and return its project key."
  (interactive (list (read-string "Project key or URL: ")))
  (feishu-project-workbench--require-action)
  (feishu-project-workbench--call
   'project (if (string-match-p "\\`https?://" locator)
                (list :url locator)
              (list :project_key locator))
   (lambda (result)
     (let* ((data (plist-get result :data))
            (key (or (alist-get 'project_key data)
                     (alist-get 'project_key (car (alist-get 'list data))))))
       (unless (stringp key) (user-error "Project lookup did not return a project key"))
       (when (called-interactively-p 'interactive) (message "Validated project %s" key))
       key))))

;;;###autoload
(defun feishu-project-switch-project (project)
  "Validate PROJECT or configured alias and reload current list descriptor."
  (interactive (list (completing-read "Project: "
                                      (append (mapcar #'car feishu-project-project-aliases)
                                              (list feishu-project--project-key)) nil nil)))
  (let ((locator (or (alist-get project feishu-project-project-aliases nil nil #'equal) project)))
    (feishu-project-workbench--call
     'project (if (string-match-p "\\`https?://" locator)
                  (list :url locator)
                (list :project_key locator))
     (lambda (result)
       (let ((key (or (alist-get 'project_key (plist-get result :data)) locator)))
         (feishu-project--display key feishu-project--type-spec feishu-project--query-kind
                                  feishu-project--query feishu-project--filters))))))

;;;###autoload
(defun feishu-project-columns (titles)
  "Select visible column TITLES and rerender current list."
  (interactive (list (completing-read-multiple "Columns: "
                                               (mapcar (lambda (column) (plist-get (feishu-project--column-normalize column) :title))
                                                       feishu-project-list-columns) nil t)))
  (setq-local feishu-project--active-columns
              (cl-remove-if-not (lambda (column) (member (plist-get (feishu-project--column-normalize column) :title) titles))
                                feishu-project-list-columns))
  (setq tabulated-list-format
        (vconcat (mapcar (lambda (column) (let ((c (feishu-project--column-normalize column)))
                                             (list (plist-get c :title) (plist-get c :width) t)))
                         (feishu-project--columns)))
        tabulated-list-sort-key nil)
  (feishu-project--render-list))

;;;###autoload
(defun feishu-project-reset-columns ()
  "Restore default columns and rerender." (interactive)
  (setq-local feishu-project--active-columns nil) (feishu-project-list-mode) (feishu-project--render-list))

;;;###autoload
(defun feishu-project-find (locator)
  "Find LOCATOR and render only newest result callback."
  (interactive "sID, name, or Feishu URL: ")
  (feishu-project-workbench--require-action)
  (let* ((backend feishu-project--backend-name) (buffer (get-buffer-create "*Feishu Project Find*"))
         (url-p (string-match-p "\\`https?://" locator)) (numeric-p (string-match-p "\\`[0-9]+\\'" locator))
         (project (unless url-p (feishu-project--read-project-key)))
         (payload (cond (url-p (list :url locator :fields (vector "_all")))
                        (numeric-p (list :project_key project :work_item_id locator :fields (vector "_all")))
                        (t (list :project_key project :name locator :fields (vector "_all"))))) generation identity)
    (with-current-buffer buffer
      (unless (derived-mode-p 'feishu-project-detail-mode) (feishu-project-detail-mode))
      (setq-local feishu-project--backend-name backend feishu-project--detail-identity (list backend project "find" locator))
      (setq generation (cl-incf feishu-project--detail-generation) identity feishu-project--detail-identity))
    (pop-to-buffer buffer)
    (feishu-project-workbench--call
     'find payload
     (lambda (result)
       (when (feishu-project-workbench--current-p buffer generation identity)
         (let ((item (plist-get result :item)))
           (with-current-buffer buffer
             (setq-local feishu-project--detail-identity (feishu-project--row-identity item backend)
                         feishu-project--items (list item))
             (feishu-project--render-detail item))))) nil backend)))

;;;###autoload
(defun feishu-project-detail-refresh ()
  "Refresh current detail through its pinned backend." (interactive)
  (let ((item (feishu-project-workbench--item)))
    (feishu-project--request-detail item feishu-project--backend-name (current-buffer))))

(defun feishu-project-workbench--section-lines (data)
  "Render DATA without printing raw remote structures."
  (let ((rows (or (alist-get 'list data) (alist-get 'items data) (list data))))
    (mapcar (lambda (row) (format "%s" (or (alist-get 'content row) (alist-get 'name row)
                                             (alist-get 'title row) (alist-get 'id row) "Item"))) rows)))

(defun feishu-project-workbench--load-section (action title &optional extra)
  "Fetch ACTION section only if current detail remains unchanged."
  (feishu-project-workbench--require-action)
  (let* ((item (feishu-project-workbench--item)) (buffer (current-buffer))
         (generation feishu-project--detail-generation) (identity feishu-project--detail-identity)
         (backend feishu-project--backend-name))
    (feishu-project-workbench--call action (append (feishu-project-workbench--payload item) extra)
     (lambda (result)
       (when (feishu-project-workbench--current-p buffer generation identity)
         (with-current-buffer buffer
           (let ((lines (feishu-project-workbench--section-lines (plist-get result :data))))
             (setf (alist-get title feishu-project--detail-sections nil nil #'equal) lines)
             (setf (alist-get title feishu-project--detail-section-data nil nil #'equal) (plist-get result :data))
             (feishu-project--render-detail item))))) nil backend)))

;;;###autoload
(defun feishu-project-show-comments () (interactive) (feishu-project-workbench--load-section 'comments "Comments"))
;;;###autoload
(defun feishu-project-show-related () (interactive) (feishu-project-workbench--load-section 'related "Related items" (list :page_size 50)))
;;;###autoload
(defun feishu-project-show-history () (interactive) (feishu-project-workbench--load-section 'history "History"))

;;;###autoload
(defun feishu-project-edit-field (key value)
  "Update exact schema KEY with VALUE after confirmation."
  (interactive "sField key: \nsRaw JSON or literal value: ")
  (feishu-project-workbench--require-action)
  (let ((item (feishu-project-workbench--item)))
    (feishu-project-workbench--call
     'field-schema (list :project_key (feishu-project--item-project item)
                         :work_item_type (feishu-project--item-type-key item) :field_keys (vector key))
     (lambda (schema)
       (unless (feishu-project-workbench--schema-has-field-p (plist-get schema :data) key)
         (user-error "Field %s is unavailable for this work item type" key))
       (unless (yes-or-no-p (format "Update %s? " key)) (user-error "Update cancelled"))
       (feishu-project-workbench--call
        'update-field (append (feishu-project-workbench--payload item)
                              (list :fields (vector (list :field_key key :field_value value))))
        (lambda (_result) (message "Feishu Project field updated")))))))

;;;###autoload
(defun feishu-project-transition-state (transition-id)
  "Transition current item using a verified TRANSITION-ID."
  (interactive "sTransition ID: ")
  (feishu-project-workbench--require-action)
  (let ((item (feishu-project-workbench--item)))
    (feishu-project-workbench--call
     'transition-states (feishu-project-workbench--payload item)
     (lambda (states)
       (let ((state (cl-find transition-id (or (alist-get 'list (plist-get states :data)) '())
                             :key (lambda (entry) (format "%s" (or (alist-get 'transition_id entry)
                                                                    (alist-get 'id entry)))) :test #'equal)))
         (unless state (user-error "Transition %s is unavailable" transition-id))
         (feishu-project-workbench--call
          'transition-required
          (append (feishu-project-workbench--payload item)
                  (list :transition_id transition-id
                        :state_key (or (alist-get 'state_key state)
                                       (alist-get 'key state))))
          (lambda (required)
            (when (seq-some (lambda (field) (not (alist-get 'optional field)))
                            (or (alist-get 'list (plist-get required :data)) '()))
              (user-error "Transition requires fields; edit them before transition"))
            (unless (yes-or-no-p (format "Transition to %s? " transition-id))
              (user-error "Transition cancelled"))
            (feishu-project-workbench--call
             'transition
             (append (feishu-project-workbench--payload item) (list :transition_id transition-id))
             (lambda (_result) (message "Feishu Project state transitioned"))))))))))

(defun feishu-project-workbench--save-comment (action content &optional comment-id)
  "Save CONTENT as comment ACTION, optionally updating COMMENT-ID."
  (feishu-project-workbench--require-action)
  (let ((item (feishu-project-workbench--item)))
    (unless (yes-or-no-p (format "%s comment? " (capitalize (symbol-name action))))
      (user-error "Comment cancelled"))
    (feishu-project-workbench--call
     'comment-save
     (append (feishu-project-workbench--payload item)
             (list :action (symbol-name action) :content content)
             (and comment-id (list :comment_id comment-id)))
     (lambda (_result) (message "Feishu Project comment saved")))))

;;;###autoload
(defun feishu-project-add-comment (content)
  "Create a comment with CONTENT." (interactive "sComment: ")
  (feishu-project-workbench--save-comment 'create content))

;;;###autoload
(defun feishu-project-update-comment (comment-id content)
  "Update COMMENT-ID with CONTENT." (interactive "sComment ID: \nsComment: ")
  (feishu-project-workbench--save-comment 'update content comment-id))

(defun feishu-project-workbench--meta-records (data)
  "Return official FieldConfList records from field-meta DATA."
  (let ((records (alist-get 'FieldConfList data)))
    (unless (listp records) (user-error "Field metadata response lacks FieldConfList")) records))

(defun feishu-project-workbench--option-records (options)
  "Normalize official OPTIONS to a list of option alists."
  (cond ((null options) nil)
        ((and (listp options) (assq 'option_id options)) (list options))
        ((listp options) options)
        (t (user-error "Template schema option data is invalid"))))

(defun feishu-project-workbench--template-valid-p (schema template)
  "Return non-nil if template schema option_id equals TEMPLATE and is enabled."
  (let ((config (cl-find-if (lambda (x) (equal "template" (alist-get 'field_key x)))
                            (feishu-project-workbench--schema-configs schema))))
    (cl-some (lambda (option) (and (equal template (format "%s" (alist-get 'option_id option)))
                                   (not (alist-get 'disabled option))))
             (feishu-project-workbench--option-records (alist-get 'option config)))))

(defun feishu-project-workbench--parse-extra (text)
  "Parse TEXT as field_key/field_value string objects."
  (if (string-empty-p text)
      nil
    (let ((value (json-parse-string text :object-type 'alist :array-type 'list)))
      (unless (and (listp value)
                   (cl-every (lambda (entry)
                               (and (stringp (alist-get 'field_key entry))
                                    (stringp (alist-get 'field_value entry))))
                             value))
        (user-error "Extra fields must be JSON objects with string field_key and field_value"))
      value)))

;;;###autoload
(defun feishu-project-create-item (project type template name extra-text)
  "Create only after exact schema and aggregate field metadata validation."
  (interactive (list (feishu-project--read-project-key) (read-string "Type: ") (read-string "Template ID: ")
                     (read-string "Name: ") (read-string "Extra fields JSON: ")))
  (feishu-project-workbench--require-action)
  (let ((extra (feishu-project-workbench--parse-extra extra-text)))
    (feishu-project-workbench--call
     'field-schema (list :project_key project :work_item_type type :field_keys (vector "template" "name"))
     (lambda (schema)
       (let ((data (plist-get schema :data)))
         (unless (and (feishu-project-workbench--schema-has-field-p data "template")
                      (feishu-project-workbench--schema-has-field-p data "name")
                      (feishu-project-workbench--template-valid-p data template))
           (user-error "Template/name schema or selected template option is invalid"))
         (feishu-project-workbench--call
          'field-meta (list :project_key project :work_item_type type)
          (lambda (meta)
            (let* ((required (cl-loop for record in (feishu-project-workbench--meta-records (plist-get meta :data))
                                      when (alist-get 'is_required record) collect (alist-get 'field_key record)))
                   (provided (append '("template" "name") (mapcar (lambda (x) (alist-get 'field_key x)) extra)))
                   (missing (seq-remove (lambda (key) (member key provided)) required)))
              (when missing (user-error "Create missing required fields: %s" (string-join missing ", ")))
              (unless (yes-or-no-p (format "Create %s in %s? " type project)) (user-error "Create cancelled"))
              (feishu-project-workbench--call
               'create (list :project_key project :work_item_type type
                             :fields (vconcat (append (list `((field_key . "template") (field_value . ,template))
                                                            `((field_key . "name") (field_value . ,name))) extra)))
               (lambda (_result) (message "Feishu Project item created")))))))))))

(defun feishu-project-workbench--metadata (data)
  "Extract official URL/sign/multipart metadata from DATA."
  (let ((data (or (alist-get 'data data) data)))
    (list :url (or (alist-get 'url data) (alist-get 'upload_url data) (alist-get 'download_url data))
          :sign (or (alist-get 'sign data) (alist-get 'file_sign data))
          :multipart (alist-get 'is_multipart data))))

(defun feishu-project-workbench--part-url (url)
  "Replace literal :part_number in URL with first single-part index."
  (replace-regexp-in-string ":part_number" "0" url t t))

(defun feishu-project-workbench--transfer (url sign method mime &optional body destination)
  "Transfer URL with dynamic sign header; never log credentials."
  (let ((url-request-method method) (url-request-data body)
        (url-request-extra-headers (append `(("X-Meego-File-Sign" . ,sign))
                                           (and mime `(("Content-Type" . ,mime))))))
    (let ((response (url-retrieve-synchronously url t t 60)))
      (unless response (user-error "Feishu attachment transfer failed"))
      (unwind-protect (with-current-buffer response
                        (unless (<= 200 (or url-http-response-status 500) 299)
                          (user-error "Feishu attachment transfer failed"))
                        (when destination (write-region (or url-http-end-of-headers (point-min)) (point-max) destination nil 'silent)))
        (kill-buffer response)))))

;;;###autoload
(defun feishu-project-upload-attachment (source mime field-key)
  "Upload SOURCE to attachment FIELD-KEY via official resource type 15 metadata."
  (interactive (list (read-file-name "Upload file: " nil nil t)
                     (read-string "MIME type: " "application/octet-stream")
                     (read-string "Attachment field key: ")))
  (feishu-project-workbench--require-action)
  (let ((item (feishu-project-workbench--item)))
    (unless (yes-or-no-p (format "Upload %s? " (file-name-nondirectory source))) (user-error "Upload cancelled"))
    (if (feishu-project--backend-capable-p 'attachment-shortcuts)
        (feishu-project-workbench--call
         'upload-file
         (append (feishu-project-workbench--payload item)
                 (list :source source :mime_type mime :field_key field-key))
         (lambda (_result) (message "Feishu Project file uploaded")))
      (feishu-project-workbench--call
       'upload-metadata
       (append (feishu-project-workbench--payload item)
               (list :resource_type 15 :file_name (file-name-nondirectory source)
                     :mime_type mime :size (file-attribute-size (file-attributes source)))
               (unless (string-empty-p field-key) (list :field_key field-key)))
       (lambda (result)
         (pcase-let ((`(:url ,url :sign ,sign :multipart ,multipart) (feishu-project-workbench--metadata (plist-get result :data))))
           (when multipart (user-error "Multipart attachment upload is unsupported"))
           (unless (and (stringp url) (stringp sign)) (user-error "Upload metadata is incomplete"))
           (feishu-project-workbench--transfer (feishu-project-workbench--part-url url) sign "POST" mime
                                               (with-temp-buffer (insert-file-contents-literally source) (buffer-string)))
           (message "Feishu Project file uploaded; attachment field association was not changed")))))))

;;;###autoload
(defun feishu-project-download-attachment (file-url destination)
  "Download official FILE-URL to DESTINATION for current work item."
  (interactive "sAttachment file URL: \nFDownload to: ")
  (feishu-project-workbench--require-action)
  (let ((item (feishu-project-workbench--item)))
    (unless (yes-or-no-p (format "Download attachment to %s? " destination)) (user-error "Download cancelled"))
    (if (feishu-project--backend-capable-p 'attachment-shortcuts)
        (feishu-project-workbench--call
         'download-file
         (append (feishu-project-workbench--payload item)
                 (list :source file-url :destination destination))
         (lambda (_result) (message "Feishu Project attachment downloaded")))
      (feishu-project-workbench--call
       'download-metadata (append (feishu-project-workbench--payload item) (list :file_url file-url))
       (lambda (result)
         (pcase-let ((`(:url ,url :sign ,sign :multipart ,multipart) (feishu-project-workbench--metadata (plist-get result :data))))
           (when multipart (user-error "Multipart attachment download is unsupported"))
           (unless (and (stringp url) (stringp sign)) (user-error "Download metadata is incomplete"))
           (feishu-project-workbench--transfer (feishu-project-workbench--part-url url) sign "GET" nil nil destination)
           (message "Feishu Project attachment downloaded")))))))

;;;###autoload
(defun feishu-project-advanced-node-subtask (node-id action &optional task-id)
  "Run explicit node subtask ACTION after confirmation."
  (interactive (list (read-string "Node ID: ") (read-string "Node action: ") (read-string "Task ID (optional): ")))
  (let ((item (feishu-project-workbench--item)))
    (unless (yes-or-no-p "Run node subtask action? ") (user-error "Cancelled"))
    (feishu-project-workbench--call 'advanced-node-subtask
     (append (feishu-project-workbench--payload item) (list :node_id node-id :action action)
             (unless (string-empty-p task-id) (list :task_id task-id)))
     (lambda (_result) (message "Node subtask action completed")))))

(defun feishu-project-workbench--batch-groups (items)
  "Group immutable ITEMS by project and type."
  (let ((groups (make-hash-table :test #'equal)))
    (dolist (item items)
      (push item (gethash (list (feishu-project--item-project item)
                                (feishu-project--item-type-key item)) groups))
      (puthash (list (feishu-project--item-project item)
                     (feishu-project--item-type-key item))
               (gethash (list (feishu-project--item-project item)
                              (feishu-project--item-type-key item)) groups) groups))
    groups))

;;;###autoload
(defun feishu-project-batch-edit-field (key value)
  "Schema-validate marked snapshot before one confirmation and all updates."
  (interactive "sField key: \nsValue: ")
  (feishu-project-workbench--require-action)
  (let* ((items (copy-tree (feishu-project-marked-items)))
         (groups (feishu-project-workbench--batch-groups items))
         (pending (hash-table-count groups)) invalid successes failures)
    (unless items (user-error "No work items to update"))
    (cl-labels
        ((report ()
           (message "Batch updated %d; failed %d: %s" (length successes) (length failures)
                    (string-join (nreverse failures) ", ")))
         (start-updates ()
           (unless (yes-or-no-p (format "Update %s on %d items? " key (length items)))
             (user-error "Batch update cancelled"))
           (dolist (item items)
             (let ((target item) (target-id (feishu-project--item-id item)))
               (feishu-project-workbench--call
                'update-field
                (append (feishu-project-workbench--payload target)
                        (list :fields (vector (list :field_key key :field_value value))))
                (lambda (_result)
                  (push target-id successes)
                  (setq pending (1- pending))
                  (when (zerop pending) (report)))
                (lambda (_error)
                  (push target-id failures)
                  (setq pending (1- pending))
                  (when (zerop pending) (report))))))))
      (maphash
       (lambda (group _group-items)
         (feishu-project-workbench--call
          'field-schema
          (list :project_key (nth 0 group) :work_item_type (nth 1 group)
                :field_keys (vector key))
          (lambda (schema)
            (unless (feishu-project-workbench--schema-has-field-p (plist-get schema :data) key)
              (push (format "%s/%s" (nth 0 group) (nth 1 group)) invalid))
            (setq pending (1- pending))
            (when (zerop pending)
              (if invalid
                  (user-error "Field %s is unavailable for: %s" key (string-join invalid ", "))
                (setq pending (length items))
                (start-updates))))
          (lambda (error)
            (push (format "%s/%s: %s" (nth 0 group) (nth 1 group) error) invalid)
            (setq pending (1- pending))
            (when (zerop pending)
              (user-error "Cannot validate batch field: %s" (string-join invalid "; "))))))
       groups))))

;;;###autoload
(defun feishu-project-export (format destination)
  "Export marked or visible items." (interactive (list (intern (completing-read "Format: " '("csv" "tsv" "org" "markdown" "json") nil t)) (read-file-name "Export to: ")))
  (feishu-project-export-write format (feishu-project-marked-items) destination (feishu-project--columns)))

;;;###autoload
(defun feishu-project-batch-copy-ids ()
  "Copy IDs from the immutable marked-or-visible snapshot." (interactive)
  (let ((ids (mapcar #'feishu-project--item-id (copy-tree (feishu-project-marked-items)))))
    (kill-new (string-join ids "\n")) (message "Copied %d work item IDs" (length ids))))

(eval-after-load 'transient
  '(eval
    '(progn
     (transient-define-prefix feishu-project--list-menu ()
       [["List" ("l" "list" feishu-project-list) ("q" "MQL" feishu-project-mql) ("f" "find" feishu-project-find)
         ("/" "filter" feishu-project-filter) ("c" "clear filters" feishu-project-clear-filters)]
        ["Views" ("s" "save query" feishu-project-save-query) ("L" "load query" feishu-project-load-query)
         ("p" "switch project" feishu-project-switch-project) ("v" "columns" feishu-project-columns)
         ("V" "reset columns" feishu-project-reset-columns)]
        ["Batch" ("i" "copy IDs" feishu-project-batch-copy-ids) ("e" "export" feishu-project-export)
         ("b" "edit field" feishu-project-batch-edit-field) ("u" "unmark" feishu-project-unmark-all)]])
     (transient-define-prefix feishu-project--detail-menu ()
       [["Inspect" ("g" "refresh" feishu-project-detail-refresh) ("c" "comments" feishu-project-show-comments)
         ("r" "related" feishu-project-show-related) ("h" "history" feishu-project-show-history)]
        ["Write" ("e" "edit field" feishu-project-edit-field) ("t" "transition" feishu-project-transition-state)
         ("a" "add comment" feishu-project-add-comment) ("U" "update comment" feishu-project-update-comment)
         ("n" "create" feishu-project-create-item)]
        ["Files" ("+" "upload" feishu-project-upload-attachment) ("d" "download" feishu-project-download-attachment)
         ("x" "node subtask" feishu-project-advanced-node-subtask)]]))))

;;;###autoload
(defun feishu-project-dispatch ()
  "Open the optional lazy-loaded list transient menu." (interactive)
  (if (require 'transient nil t) (funcall (intern "feishu-project--list-menu"))
    (user-error "Transient is optional; use M-x Feishu Project commands")))

;;;###autoload
(defun feishu-project-detail-dispatch ()
  "Open the optional lazy-loaded detail transient menu." (interactive)
  (if (require 'transient nil t) (funcall (intern "feishu-project--detail-menu"))
    (user-error "Transient is optional; use M-x Feishu Project commands")))

(provide 'feishu-project-workbench)
;;; feishu-project-workbench.el ends here
