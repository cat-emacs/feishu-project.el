;;; feishu-project-mcp.el --- Feishu Project MCP backend -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Misaka
;; Package-Requires: ((emacs "30.1") (mcp "0.2.0"))

;;; Commentary:

;; Optional MCP backend using mcp.el Streamable HTTP and native OAuth device
;; authorization.  OAuth credentials are owned and refreshed by mcp.el.

;;; Code:

(require 'json)
(require 'subr-x)
(require 'feishu-project)

(declare-function jsonrpc-running-p "jsonrpc")
(declare-function mcp-async-call-tool "mcp")
(declare-function mcp-connect-server "mcp")
(declare-function mcp--server-running-p "mcp")
(declare-function mcp--status "mcp")

(defcustom feishu-project-mcp-oauth
  '(:client-name "feishu-project.el" :open-browser t)
  "OAuth configuration passed to `mcp-connect-server'."
  :group 'feishu-project
  :type 'plist)

(defcustom feishu-project-mcp-url "https://project.feishu.cn/mcp_server/v1"
  "Official Feishu Project MCP URL."
  :group 'feishu-project
  :type 'string)

(defcustom feishu-project-mcp-server-name "feishu-project"
  "mcp.el connection name."
  :group 'feishu-project
  :type 'string)

(defcustom feishu-project-mcp-timeout 30
  "MCP request timeout in seconds."
  :group 'feishu-project
  :type 'integer)

(defvar feishu-project-mcp--connection nil
  "The initialized mcp.el connection, or nil.")

(defvar feishu-project-mcp--connecting nil
  "Non-nil while mcp.el initialization is in progress.")

(defvar feishu-project-mcp--pending nil
  "Queued (TOOL ARGUMENTS SUCCESS FAILURE) calls awaiting initialization.")

(defun feishu-project-mcp--json (text)
  "Decode MCP TEXT JSON into an alist."
  (condition-case err
      (json-parse-string text
                         :object-type 'alist
                         :array-type 'list
                         :null-object nil
                         :false-object :json-false)
    (json-parse-error
     (error "Feishu MCP returned malformed JSON: %s"
            (error-message-string err)))))

(defun feishu-project-mcp--content-json (result)
  "Extract and decode the first text content item in MCP RESULT."
  (let ((tool-error (or (plist-get result :isError)
                        (plist-get result :is-error))))
    (when (and tool-error (not (eq tool-error :json-false)))
      (error "Feishu MCP tool error: %s" result)))
  (let* ((content (or (plist-get result :content)
                      (alist-get 'content result)))
         (text (cl-loop for entry across (vconcat content)
                        when (equal (or (plist-get entry :type)
                                        (alist-get 'type entry))
                                    "text")
                        return (or (plist-get entry :text)
                                   (alist-get 'text entry)))))
    (unless (stringp text)
      (error "Feishu MCP returned no text JSON content"))
    (feishu-project-mcp--json text)))

(defun feishu-project-mcp--lookup (key alist)
  "Return dynamic string KEY from JSON ALIST regardless of key representation."
  (or (alist-get key alist nil nil #'equal)
      (alist-get (intern key) alist)))

(defun feishu-project-mcp--enabled-types (payload)
  "Normalize enabled work item types from list_workitem_types PAYLOAD."
  (cl-loop for type in (alist-get 'list payload)
           unless (equal (alist-get 'is_disable type) 1)
           collect (list :key (alist-get 'type_key type)
                         :name (alist-get 'name type))))

(defun feishu-project-mcp--field-value (field)
  "Unpack the typed MQL value in FIELD."
  (let* ((value (alist-get 'value field))
         (type (alist-get 'value_type field)))
    (cond
     ((not (listp value)) value)
     ((assoc (intern type) value) (alist-get (intern type) value))
     ((cdr value) (cdar value))
     (t nil))))

(defun feishu-project-mcp--mql-row (row project type)
  "Normalize MQL ROW, injecting PROJECT and selected TYPE."
  (let ((item `((simple_name . ,project)
                (work_item_type_key . ,(plist-get type :key)))))
    (dolist (field (alist-get 'moql_field_list row))
      (let ((key (alist-get 'key field))
            (value (feishu-project-mcp--field-value field)))
        (push (cons (intern key) value) item)))
    item))

(defun feishu-project-mcp--mql-result (payload project type replay-mql)
  "Normalize initial search PAYLOAD for PROJECT, TYPE, and REPLAY-MQL."
  (let* ((group (car (alist-get 'list payload)))
         (group-info (car (alist-get 'group_infos group)))
         (group-id (alist-get 'group_id group-info))
         (rows (feishu-project-mcp--lookup group-id (alist-get 'data payload)))
         (total (alist-get 'count group))
         (fetched (length rows))
         (session-id (alist-get 'session_id payload))
         (replay-token (and replay-mql
                            (list :kind 'replay
                                  :mql replay-mql
                                  :type type)))
         (continuation
          (and session-id group-id total (< fetched total)
               (list :kind 'session
                     :session-id session-id
                     :group-id group-id
                     :page-num 2
                     :fetched fetched
                     :total total
                     :type type))))
    (list :items (mapcar (lambda (row)
                           (feishu-project-mcp--mql-row row project type))
                         rows)
          :total total
          :current-page-token replay-token
          :continuation continuation)))

(defun feishu-project-mcp--mql-next-result (payload project continuation)
  "Normalize subsequent MQL PAYLOAD using CONTINUATION."
  (let* ((group-id (plist-get continuation :group-id))
         (rows (feishu-project-mcp--lookup group-id (alist-get 'data payload)))
         (total (plist-get continuation :total))
         (page-num (plist-get continuation :page-num))
         (fetched (+ (or (plist-get continuation :fetched) 0)
                     (length rows)))
         (next
          (and (< fetched total)
               (list :kind 'session
                     :session-id (plist-get continuation :session-id)
                     :group-id group-id
                     :page-num (1+ page-num)
                     :fetched fetched
                     :total total
                     :type (plist-get continuation :type)))))
    (list :items
          (mapcar (lambda (row)
                    (feishu-project-mcp--mql-row
                     row project (plist-get continuation :type)))
                  rows)
          :total total
          :continuation next)))

(defun feishu-project-mcp--detail-result (payload)
  "Normalize get_workitem_brief PAYLOAD to the common detail representation."
  (let* ((attribute (alist-get 'work_item_attribute payload))
         (owned-project (alist-get 'owned_project attribute))
         (type (alist-get 'work_item_type attribute))
         (pagination (alist-get 'pagination payload)))
    (list :item
          `((work_item_id . ,(alist-get 'work_item_id attribute))
            (name . ,(alist-get 'work_item_name attribute))
            (work_item_type_key . ,(alist-get 'key type))
            (work_item_type . ,(alist-get 'name type))
            (simple_name . ,(alist-get 'simple_name owned-project))
            (project_key . ,(alist-get 'key owned-project))
            (created_at . ,(alist-get 'create_time attribute))
            (updated_at . ,(alist-get 'update_time attribute))
            (work_item_status . ,(alist-get 'work_item_status attribute))
            (detail-truncated . ,(alist-get 'has_more pagination))
            (fields
             . ,(mapcar
                 (lambda (field)
                   `((field_key . ,(alist-get 'key field))
                     (field_alias . ,(alist-get 'name field))
                     (field_value . ,(alist-get 'value field))))
                 (alist-get 'work_item_fields payload)))))))

(defun feishu-project-mcp--require-runtime ()
  "Load and validate the optional MCP runtime on first actual operation."
  (unless (version<= "30.1" emacs-version)
    (user-error "MCP backend requires Emacs 30.1 or newer"))
  (require 'mcp)
  (require 'mcp-oauth)
  (unless (and (fboundp 'mcp-connect-server)
               (fboundp 'mcp-async-call-tool)
               (fboundp 'mcp-oauth-create)
               (boundp 'mcp-server-connections))
    (user-error "MCP backend requires mcp.el 0.2.0 or newer")))

(defun feishu-project-mcp--connection-live-p (connection)
  "Return non-nil when CONNECTION completed MCP initialization."
  (and connection
       (jsonrpc-running-p connection)
       (or (eq connection feishu-project-mcp--connection)
           (eq (mcp--status connection) 'connected))))

(defun feishu-project-mcp--clear-stale-connection ()
  "Reuse live mcp.el entries and discard stopped or errored entries.
`mcp-stop-server' retains stopped entries, so an explicit `remhash' is needed
before reconnecting."
  (when (boundp 'mcp-server-connections)
    (let ((connection
           (gethash feishu-project-mcp-server-name mcp-server-connections)))
      (cond
       ((feishu-project-mcp--connection-live-p connection)
        (setq feishu-project-mcp--connection connection))
       ((and feishu-project-mcp--connecting
             connection
             (jsonrpc-running-p connection))
        nil)
       (t
        (remhash feishu-project-mcp-server-name mcp-server-connections)
        (when (eq connection feishu-project-mcp--connection)
          (setq feishu-project-mcp--connection nil)))))))

(defun feishu-project-mcp--fail-pending (message)
  "Fail every queued call once with MESSAGE and restore retryable state."
  (let ((pending (nreverse feishu-project-mcp--pending)))
    (setq feishu-project-mcp--pending nil
          feishu-project-mcp--connecting nil
          feishu-project-mcp--connection nil)
    (dolist (call pending)
      (funcall (nth 3 call) message))))

(defun feishu-project-mcp--flush-pending (connection)
  "Flush queued calls exactly once after mcp.el initializes CONNECTION."
  (when feishu-project-mcp--connecting
    (let ((pending (nreverse feishu-project-mcp--pending)))
      (setq feishu-project-mcp--pending nil
            feishu-project-mcp--connecting nil
            feishu-project-mcp--connection connection)
      (dolist (call pending)
        (apply #'feishu-project-mcp--call-now connection call)))))

(defun feishu-project-mcp--start-connection ()
  "Connect to Feishu MCP and queue calls until initialization completes."
  (feishu-project-mcp--require-runtime)
  (feishu-project-mcp--clear-stale-connection)
  (unless feishu-project-mcp--connecting
    (setq feishu-project-mcp--connecting t)
    (condition-case err
        (mcp-connect-server
         feishu-project-mcp-server-name
         :url feishu-project-mcp-url
         :transport 'streamable
         :oauth feishu-project-mcp-oauth
         :timeout feishu-project-mcp-timeout
         :initial-callback #'feishu-project-mcp--flush-pending
         :error-callback
         (lambda (_code message)
           (feishu-project-mcp--fail-pending message)))
      (error
       (feishu-project-mcp--fail-pending (error-message-string err))))))

(defun feishu-project-mcp--call-now (connection tool arguments success failure)
  "Call TOOL on initialized CONNECTION with ARGUMENTS."
  (mcp-async-call-tool
   connection tool arguments
   (lambda (result)
     (condition-case err
         (funcall success (feishu-project-mcp--content-json result))
       (error (funcall failure (error-message-string err)))))
   (lambda (_code message)
     (funcall failure message))))

(defun feishu-project-mcp--call (tool arguments success failure)
  "Asynchronously call TOOL with ARGUMENTS after connection initialization."
  (condition-case err
      (progn
        (feishu-project-mcp--require-runtime)
        (feishu-project-mcp--clear-stale-connection)
        (if (feishu-project-mcp--connection-live-p
             feishu-project-mcp--connection)
            (feishu-project-mcp--call-now feishu-project-mcp--connection
                                           tool arguments success failure)
          (progn
            (push (list tool arguments success failure)
                  feishu-project-mcp--pending)
            (feishu-project-mcp--start-connection))))
    (error (funcall failure (error-message-string err)))))

(defun feishu-project-mcp--mql (project mql continuation _context success failure)
  "Execute MQL in PROJECT, replaying or continuing when requested."
  (pcase (plist-get continuation :kind)
    ('session
     (feishu-project-mcp--call
      "search_by_mql"
      (list :project_key project
            :session_id (plist-get continuation :session-id)
            :group_pagination_list
            (vector (list :group_id (plist-get continuation :group-id)
                          :page_num (plist-get continuation :page-num))))
      (lambda (payload)
        (funcall success
                 (feishu-project-mcp--mql-next-result
                  payload project continuation)))
      failure))
    (_
     (let ((query (or (plist-get continuation :mql) mql))
           (type (plist-get continuation :type)))
       (feishu-project-mcp--call
        "search_by_mql"
        (list :project_key project :mql query)
        (lambda (payload)
          (funcall success
                   (feishu-project-mcp--mql-result
                    payload project type query)))
        failure)))))

(defun feishu-project-mcp--context-active-p (context)
  "Return non-nil when list CONTEXT still belongs to the active request."
  (feishu-project--request-current-p context))

(defun feishu-project-mcp--select-type (types project name context success failure)
  "Select a type in CONTEXT's original list buffer, then run minimal MQL."
  (when (feishu-project-mcp--context-active-p context)
    (let ((buffer (plist-get context :buffer)))
      (with-current-buffer buffer
        (let* ((names (mapcar (lambda (type) (plist-get type :name)) types))
               (choice (completing-read "Work item type: " names nil t))
               (type (cl-find choice types
                              :key (lambda (value) (plist-get value :name))
                              :test #'equal))
               (mql (format
                     "SELECT `work_item_id`, `name`, `updated_at` FROM `%s`.`%s` LIMIT %d"
                     project (plist-get type :key) feishu-project-page-size)))
          (unless type
            (funcall failure "No Feishu Project work item type was selected"))
          (when name
            (message "MCP list ignores name filtering; use MQL for schema-specific filtering"))
          (when type
            (feishu-project-mcp--call
             "search_by_mql"
             (list :project_key project :mql mql)
             (lambda (payload)
               (funcall success
                        (feishu-project-mcp--mql-result
                         payload project type mql)))
             failure)))))))

(defun feishu-project-mcp--list (project _types name continuation context
                                         success failure)
  "List enabled types then query the selected type with minimal MQL."
  (if continuation
      (feishu-project-mcp--mql project nil continuation context success failure)
    (feishu-project-mcp--call
     "list_workitem_types"
     (list :project_key project)
     (lambda (payload)
       (feishu-project-mcp--select-type
        (feishu-project-mcp--enabled-types payload)
        project name context success failure))
     failure)))

(defun feishu-project-mcp--detail (item _context success failure)
  "Fetch ITEM details from get_workitem_brief."
  (feishu-project-mcp--call
   "get_workitem_brief"
   (list :project_key (feishu-project--item-project item)
         :work_item_id (feishu-project--item-id item)
         :fields (vector "_all")
         :page_size feishu-project-page-size)
   (lambda (payload)
     (funcall success (feishu-project-mcp--detail-result payload)))
   failure))

(feishu-project-register-backend
 'mcp
 (list :list #'feishu-project-mcp--list
       :mql #'feishu-project-mcp--mql
       :detail #'feishu-project-mcp--detail
       :types #'feishu-project-mcp--enabled-types))

(provide 'feishu-project-mcp)

;;; feishu-project-mcp.el ends here
