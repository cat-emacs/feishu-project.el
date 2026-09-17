;;; feishu-project-cli.el --- Meegle CLI v1.0.23 backend -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Commentary:
;; Optional Emacs 29.1 backend for the official Meegle CLI v1.0.23.
;;; Code:
(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'url-parse)
(require 'feishu-project)

(defcustom feishu-project-cli-executable "meegle" "Meegle CLI v1.0.23 executable." :type 'file :group 'feishu-project)
(defcustom feishu-project-cli-profile nil "Optional Meegle CLI profile." :type '(choice (const nil) string) :group 'feishu-project)
(defcustom feishu-project-cli-timeout 45 "CLI request timeout in seconds." :type 'integer :group 'feishu-project)
(defcustom feishu-project-cli-environment nil "Validated non-secret KEY=VALUE additions." :type '(repeat string) :group 'feishu-project)
(defconst feishu-project-cli--credential-key-regexp
  "\\(?:access[_-]?token\\|refresh[_-]?token\\|token\\|secret\\|password\\|credential\\|api[_-]?key\\|private[_-]?key\\|authorization\\|cookie\\)")
(defconst feishu-project-cli--environment-names
  '("PATH" "HOME" "USER" "USERNAME" "SHELL" "TMP" "TEMP" "TMPDIR" "TERM" "LANG"
    "XDG_CONFIG_HOME" "XDG_CACHE_HOME" "XDG_DATA_HOME" "XDG_RUNTIME_DIR"
    "SSL_CERT_FILE" "SSL_CERT_DIR" "HTTPS_PROXY" "HTTP_PROXY" "ALL_PROXY" "NO_PROXY"
    "SystemRoot" "WINDIR" "COMSPEC" "PATHEXT" "USERPROFILE" "APPDATA" "LOCALAPPDATA"
    "HOMEDRIVE" "HOMEPATH" "DBUS_SESSION_BUS_ADDRESS"))
(defconst feishu-project-cli--action-contracts
  '((find . (:project_key :work_item_id :name :fields :page_size :page_token))
    (field-schema . (:project_key :work_item_type :field_keys :field_query
                     :field_types :page_num))
    (field-meta . (:project_key :work_item_type))
    (update-field . (:project_key :work_item_id :fields :role_operate))
    (create . (:project_key :work_item_type :fields))
    (comments . (:project_key :work_item_id :start_time :end_time :page_num))
    (comment-save . (:project_key :work_item_id :action :comment_id :content :file_token))
    (related . (:project_key :work_item_id :node_id :page_num :page_size
                :relation_field_key :relation_id))
    (history . (:project_key :work_item_id :start :end :op_record_module
                :operation_type :operator :operator_type :source :source_type
                :start_from))
    (transition-states . (:project_key :work_item_id :user_key :work_item_type))
    (transition-required . (:state_key :mode :project_key :work_item_id))
    (transition . (:project_key :work_item_id :transition_id))
    (project . (:project_key :page_num))
    (advanced-node-subtask . (:action :node_id :assignee :deliverable :fields
                              :project_key :role_assignee :schedule :task_id
                              :work_item_id))))
(defvar feishu-project-cli--authenticated (make-hash-table :test #'equal))

(defun feishu-project-cli--credential-name-p (name) (string-match-p feishu-project-cli--credential-key-regexp (downcase name)))
(defun feishu-project-cli--unsafe-url-p (value)
  (and (string-match-p "\\`https?://" value)
       (string-match-p "\\`https?://[^/?#]*@" value)))
(defun feishu-project-cli--safe-environment ()
  "Return the minimal child environment plus validated custom entries."
  (let (environment)
    (dolist (entry (append feishu-project-cli--environment-names
                           (seq-filter (lambda (entry) (string-prefix-p "LC_" entry)) process-environment)))
      (let* ((name (if (string-match "\\`\\([^=]+\\)=" entry) (match-string 1 entry) entry))
             (value (or (and (string-match "\\`[^=]+=\\(.*\\)\\'" entry) (match-string 1 entry))
                        (getenv name))))
        (when (and value (or (not (string-match-p "_PROXY\\'" name)) (not (feishu-project-cli--unsafe-url-p value))))
          (push (concat name "=" value) environment))))
    (dolist (entry feishu-project-cli-environment)
      (unless (string-match "\\`\\([A-Za-z_][A-Za-z0-9_]*\\)=\\(.*\\)\\'" entry) (user-error "Invalid Meegle CLI environment entry"))
      (let ((name (match-string 1 entry)) (value (match-string 2 entry)))
        (when (or (feishu-project-cli--credential-name-p name) (feishu-project-cli--unsafe-url-p value))
          (user-error "Meegle CLI environment must not contain credentials or URL userinfo"))
        (push entry environment)))
    (cons "MEEGLE_NO_UPDATE_CHECK=1" (delete-dups environment))))
(defun feishu-project-cli--redact (text)
  "Redact credential assignment/header values and all HTTP(S) URLs in TEXT."
  (let ((case-fold-search t) (result (or text "")))
    (setq result (replace-regexp-in-string
                  "https?://[^[:space:]\"'<>]+" "[REDACTED-URL]" result t))
    (replace-regexp-in-string
     (concat "\\(" feishu-project-cli--credential-key-regexp
             "\\)[[:space:]]*[:=][[:space:]]*[^,}\\n\\r]+")
     "\\1=[REDACTED]" result t)))
(defun feishu-project-cli--json (text)
  "Decode JSON TEXT with false normalized to nil."
  (condition-case err (json-parse-string text :object-type 'alist :array-type 'list :null-object nil :false-object nil)
    (json-parse-error (error "Meegle CLI returned malformed JSON: %s" (error-message-string err)))))
(defun feishu-project-cli--envelope (text)
  "Return structurally valid CLI JSON envelope from TEXT; data:null is valid."
  (let ((object (feishu-project-cli--json text)))
    (unless (and (listp object) (consp object) (or (assq 'data object) (assq 'error object)))
      (error "Meegle CLI returned an invalid JSON envelope"))
    object))
(defun feishu-project-cli--envelope-result (text success failure)
  (condition-case err
      (let ((envelope (feishu-project-cli--envelope text)))
        (if (alist-get 'error envelope) (funcall failure (feishu-project-cli--redact (format "%s" (alist-get 'error envelope))) )
          (funcall success (alist-get 'data envelope))))
    (error (funcall failure (feishu-project-cli--redact (error-message-string err))))))
(defun feishu-project-cli--process (argv success failure)
  "Run ARGV asynchronously, calling exactly one callback."
  (let ((program (executable-find feishu-project-cli-executable)))
    (if (not program)
        (funcall failure (format "Meegle CLI executable not found: %s"
                                 feishu-project-cli-executable))
      (let* ((stdout (generate-new-buffer " *feishu-project-cli*"))
             (stderr (generate-new-buffer " *feishu-project-cli-stderr*"))
             (done nil) (timed-out nil) (timer nil) (process nil) finish)
        (ignore argv process finish)
        (setq finish
              (lambda (ok value)
                (unless done
                  (setq done t)
                  (when timer (cancel-timer timer))
                  (unwind-protect
                      (funcall (if ok success failure) value)
                    (when (buffer-live-p stdout) (kill-buffer stdout))
                    (when (buffer-live-p stderr) (kill-buffer stderr))))))
        (condition-case _
            (setq process
                  (make-process
                   :name "feishu-project-cli"
                   :buffer stdout
                   :stderr stderr
                   :command (cons program (cdr argv))
                   :connection-type 'pipe
                   :coding 'utf-8-unix
                   :noquery t
                   :sentinel
                   (lambda (proc _event)
                     (when (memq (process-status proc) '(exit signal))
                       (unless done
                         (if timed-out
                             (funcall finish nil "Meegle CLI request timed out")
                           (if (zerop (process-exit-status proc))
                               (funcall finish t
                                        (with-current-buffer stdout (buffer-string)))
                             (funcall finish nil
                                      (format "Meegle CLI failed: %s"
                                              (feishu-project-cli--redact
                                               (with-current-buffer stderr
                                                 (string-trim (buffer-string)))))))))))))
          (error (funcall finish nil "Meegle CLI process could not start")))
        (unless done
          (setq timer
                (run-at-time
                 feishu-project-cli-timeout nil
                 (lambda ()
                   (unless done
                     (setq timed-out t)
                     (when (process-live-p process) (kill-process process))
                     (funcall finish nil "Meegle CLI request timed out"))))))))))
(defun feishu-project-cli--auth-key () (cons (expand-file-name feishu-project-cli-executable) feishu-project-cli-profile))
(defun feishu-project-cli--ensure-auth (success failure)
  (if (gethash (feishu-project-cli--auth-key) feishu-project-cli--authenticated) (funcall success)
    (let ((process-environment (feishu-project-cli--safe-environment)))
      (feishu-project-cli--process (append (list feishu-project-cli-executable "auth" "status" "--format" "json") (and feishu-project-cli-profile (list "--profile" feishu-project-cli-profile)))
       (lambda (text) (condition-case err (if (alist-get 'authenticated (feishu-project-cli--json text)) (progn (puthash (feishu-project-cli--auth-key) t feishu-project-cli--authenticated) (funcall success)) (funcall failure "Meegle CLI is not authenticated; run `meegle auth login`")) (error (funcall failure (error-message-string err))))) failure))))
(defun feishu-project-cli--project-url (url find)
  "Adapt a configured Feishu project URL; FIND requires an exact detail URL."
  (let* ((parsed (url-generic-parse-url url)) (host (url-generic-parse-url (feishu-project--host)))
         (path (url-filename parsed))
         (same (and (equal (url-type parsed) "https")
                    (not (feishu-project-cli--unsafe-url-p url))
                    (not (url-user parsed))
                    (equal (downcase (url-host parsed)) (downcase (url-host host))) (equal (url-port parsed) (url-port host)))))
    (unless (and same (not (url-target parsed)) (not (string-match-p "[?#]" path))) (user-error "Unsupported Feishu Project URL"))
    (if find
        (if (string-match "\\`/\\([^/]+\\)/\\([^/]+\\)/detail/\\([0-9]+\\)\\'" path) (list :project_key (match-string 1 path) :work_item_id (match-string 3 path)) (user-error "Expected an exact Feishu work item detail URL"))
      (if (string-match "\\`/\\([^/]+\\)\\'" path) (list :project_key (match-string 1 path)) (user-error "Expected an exact Feishu project URL")))))

(defun feishu-project-cli--argv (domain method payload)
  (append (list feishu-project-cli-executable domain method "--params"
                (json-serialize payload :null-object nil :false-object nil)
                "--format" "json" "--envelope")
          (and feishu-project-cli-profile (list "--profile" feishu-project-cli-profile))))
(defun feishu-project-cli--call (domain method payload success failure)
  (feishu-project-cli--ensure-auth
   (lambda ()
     (let ((process-environment (feishu-project-cli--safe-environment)))
       (feishu-project-cli--process (feishu-project-cli--argv domain method payload)
                                    (lambda (text) (feishu-project-cli--envelope-result text success failure)) failure))) failure))
(defun feishu-project-cli--types (payload)
  (cl-loop for type in (alist-get 'list payload) unless (equal (alist-get 'is_disable type) 1)
           collect (list :key (alist-get 'type_key type) :name (alist-get 'name type))))
(defun feishu-project-cli--field-value (field)
  (let ((value (alist-get 'value field)) (kind (alist-get 'value_type field)))
    (if (not (listp value)) value (or (alist-get (intern kind) value) (cdar value)))))
(defun feishu-project-cli--lookup (key object) (or (alist-get key object nil nil #'equal) (alist-get (intern key) object)))
(defun feishu-project-cli--mql-row (row project type)
  (let ((item `((simple_name . ,project) (work_item_type_key . ,(plist-get type :key))
                (work_item_type . ,(plist-get type :name)))))
    (dolist (field (alist-get 'moql_field_list row))
      (push (cons (intern (alist-get 'key field)) (feishu-project-cli--field-value field)) item)) item))
(defun feishu-project-cli--mql-result (payload project type replay &optional fetched)
  (let* ((group (car (alist-get 'list payload))) (info (car (alist-get 'group_infos group)))
         (group-id (alist-get 'group_id info)) (rows (or (feishu-project-cli--lookup group-id (alist-get 'data payload)) '()))
         (total (alist-get 'count group)) (seen (+ (or fetched 0) (length rows))) (session (alist-get 'session_id payload)))
    (list :items (mapcar (lambda (row) (feishu-project-cli--mql-row row project type)) rows) :total total
          :current-page-token (and replay (list :kind 'replay :mql replay :type type))
          :continuation (and session group-id (< seen total)
                             (list :kind 'session :session-id session :group-id group-id
                                   :page-num 2 :fetched seen :total total :type type)))))
(defun feishu-project-cli--detail-result (payload)
  (let* ((attribute (alist-get 'work_item_attribute payload)) (project (alist-get 'owned_project attribute))
         (type (alist-get 'work_item_type attribute)) (pagination (alist-get 'pagination payload)))
    (list :item `((work_item_id . ,(alist-get 'work_item_id attribute)) (name . ,(alist-get 'work_item_name attribute))
                  (work_item_type_key . ,(alist-get 'key type)) (work_item_type . ,(alist-get 'name type))
                  (simple_name . ,(alist-get 'simple_name project)) (project_key . ,(alist-get 'key project))
                  (created_at . ,(alist-get 'create_time attribute))
                  (updated_at . ,(alist-get 'update_time attribute))
                  (work_item_status . ,(alist-get 'work_item_status attribute))
                  (detail-truncated . ,(alist-get 'has_more pagination))
                  (fields . ,(mapcar (lambda (field) `((field_key . ,(alist-get 'key field))
                                                       (field_alias . ,(alist-get 'name field)) (field_value . ,(alist-get 'value field))))
                                   (alist-get 'work_item_fields payload)))))))

(defun feishu-project-cli--action-command (action)
  (pcase action
    ('find '("workitem" "get")) ('field-schema '("workitem" "meta-fields"))
    ('field-meta '("workitem" "meta-create-fields")) ('update-field '("workitem" "update"))
    ('create '("workitem" "create")) ('comments '("comment" "list")) ('comment-save '("comment" "add"))
    ('related '("relation" "list")) ('history '("workitem" "list-op-records"))
    ('transition-states '("workflow" "list-state-transitions")) ('transition-required '("workflow" "list-state-required"))
    ('transition '("workflow" "transition-state")) ('project '("project" "search")) ('advanced-node-subtask '("subtask" "update"))))
(defun feishu-project-cli--action-payload (action payload)
  "Project PAYLOAD onto the accepted v1.0.23 ACTION contract."
  (let ((keys (alist-get action feishu-project-cli--action-contracts)))
    (unless keys (user-error "Unsupported Meegle CLI action: %s" action))
    (when (eq action 'transition-required)
      (unless (plist-get payload :state_key) (user-error "Transition requires a destination state_key"))
      (setq payload (plist-put (copy-tree payload) :transition_id nil)))
    (let (result)
      (dolist (key keys result)
        (when (plist-member payload key)
          (setq result (append result (list key (plist-get payload key)))))))))
(defun feishu-project-cli--action (action payload success failure)
  (condition-case err
      (let* ((command (feishu-project-cli--action-command action))
             (url-payload (cond
                           ((and (eq action 'find) (plist-get payload :url))
                            (feishu-project-cli--project-url (plist-get payload :url) t))
                           ((and (eq action 'project) (plist-get payload :url))
                            (feishu-project-cli--project-url (plist-get payload :url) nil))))
             (payload (if url-payload
                          (append url-payload
                                  (cl-loop for (key value) on payload by #'cddr
                                           unless (eq key :url) append (list key value)))
                        payload))
             (payload (feishu-project-cli--action-payload action payload)))
        (unless command (user-error "Unsupported Meegle CLI action: %s" action))
        (feishu-project-cli--call (car command) (cadr command) payload
                                  (lambda (data) (funcall success (if (eq action 'find) (feishu-project-cli--detail-result data) (list :action action :data data)))) failure))
    (error (funcall failure (error-message-string err)))))
(defun feishu-project-cli--mql (project mql continuation _context success failure)
  (if (eq (plist-get continuation :kind) 'session)
      (feishu-project-cli--call "workitem" "query"
       (list :project_key project :session_id (plist-get continuation :session-id)
             :group_pagination_list (vector (list :group_id (plist-get continuation :group-id)
                                                  :page_num (plist-get continuation :page-num)
                                                  :page_size feishu-project-page-size)))
       (lambda (data)
         (let* ((page-num (plist-get continuation :page-num))
                (result (feishu-project-cli--mql-result
                         data project (plist-get continuation :type) nil
                         (plist-get continuation :fetched)))
                (next (plist-get result :continuation)))
           (when next
             (setq next (plist-put next :page-num (1+ page-num)))
             (setq result (plist-put result :continuation next)))
           (funcall success result))) failure)
    (let ((query (or (plist-get continuation :mql) mql))
          (type (plist-get continuation :type)))
      (feishu-project-cli--call
       "workitem" "query" (list :project_key project :mql query)
       (lambda (data)
         (funcall success (feishu-project-cli--mql-result data project type query)))
       failure))))

(defun feishu-project-cli--mql-literal (value)
  "Return VALUE as an escaped MQL string literal."
  (concat "'" (replace-regexp-in-string
               "['\\\\]" (lambda (match) (concat "\\\\" match))
               (format "%s" value) t t) "'"))

(defun feishu-project-cli--filter-predicates (configs filters)
  "Validate FILTERS against CONFIGS and return MQL predicates."
  (let ((mapping '((:status . "work_item_status") (:assignee . "assignee")
                   (:creator . "creator"))) predicates)
    (dolist (entry mapping)
      (when-let* ((value (plist-get filters (car entry))))
        (unless (cl-some (lambda (config)
                           (equal (cdr entry) (alist-get 'field_key config)))
                         configs)
          (user-error "Filter field %s is unavailable for this work item type"
                      (cdr entry)))
        (push (format "`%s` = %s" (cdr entry)
                      (feishu-project-cli--mql-literal value))
              predicates)))
    (nreverse predicates)))

(defun feishu-project-cli--mql-for-type (project type predicates)
  "Build list MQL for PROJECT TYPE and schema-checked PREDICATES."
  (concat (format "SELECT `work_item_id`, `name`, `work_item_status`, `updated_at` FROM `%s`.`%s`"
                  project (plist-get type :key))
          (if predicates (concat " WHERE " (string-join predicates " AND ")) "")
          (format " LIMIT %d" feishu-project-page-size)))

(defun feishu-project-cli--run-type-query (project type predicates success failure)
  "Run normalized query for PROJECT TYPE and PREDICATES."
  (let ((mql (feishu-project-cli--mql-for-type project type predicates)))
    (feishu-project-cli--call
     "workitem" "query" (list :project_key project :mql mql)
     (lambda (data)
       (funcall success (feishu-project-cli--mql-result data project type mql)))
     failure)))

(defun feishu-project-cli--select-type (types project name context success failure)
  "Select a work item type in CONTEXT, then run the list query."
  (when (feishu-project--request-current-p context)
    (with-current-buffer (plist-get context :buffer)
      (let* ((choices (delete-dups
                       (append (mapcar (lambda (type) (plist-get type :name)) types)
                               (mapcar (lambda (type) (plist-get type :key)) types))))
             (filters (plist-get context :filters))
             (choice (or (plist-get filters :type)
                         (completing-read "Work item type: " choices nil t)))
             (type (cl-find-if
                    (lambda (entry)
                      (or (equal choice (plist-get entry :name))
                          (equal choice (plist-get entry :key))))
                    types)))
        (cond
         ((not type)
          (funcall failure "No Feishu Project work item type was selected"))
         (name
          (funcall failure "CLI list cannot safely apply name filtering; use MQL"))
         ((seq-some (lambda (key) (plist-get filters key))
                    '(:status :assignee :creator))
          (feishu-project-cli--call
           "workitem" "meta-fields"
           (list :project_key project :work_item_type (plist-get type :key))
           (lambda (data)
             (condition-case err
                 (feishu-project-cli--run-type-query
                  project type
                  (feishu-project-cli--filter-predicates
                   (alist-get 'list data) filters)
                  success failure)
               (error (funcall failure (error-message-string err)))))
           failure))
         (t (feishu-project-cli--run-type-query
             project type nil success failure)))))))

(defun feishu-project-cli--list (project _types name continuation context success failure)
  "List CLI work items with interactive type selection and filters."
  (if continuation
      (feishu-project-cli--mql project nil continuation context success failure)
    (feishu-project-cli--call
     "workitem" "meta-types" (list :project_key project)
     (lambda (data)
       (feishu-project-cli--select-type
        (feishu-project-cli--types data) project name context success failure))
     failure)))
(defun feishu-project-cli--detail (item _context success failure)
  (feishu-project-cli--call "workitem" "get" (list :project_key (feishu-project--item-project item) :work_item_id (feishu-project--item-id item) :fields (vector "_all") :page_size feishu-project-page-size)
                           (lambda (data) (funcall success (feishu-project-cli--detail-result data))) failure))
(defun feishu-project-cli--file-action (action source payload success failure)
  "Run official CLI attachment shortcut after authentication."
  (let* ((upload (eq action 'upload-file))
         (argv (append (list feishu-project-cli-executable "attachment" (if upload "+upload" "+download") source)
                       (if upload (list "--resource-type" "15" "--project-key" (plist-get payload :project_key) "--work-item-id" (plist-get payload :work_item_id) "--content-type" (plist-get payload :mime_type))
                         (list "--project-key" (plist-get payload :project_key) "--work-item-id" (plist-get payload :work_item_id) "--output" (plist-get payload :destination) "--overwrite"))
                       (and upload (not (string-empty-p (or (plist-get payload :field_key) ""))) (list "--field-key" (plist-get payload :field_key)))
                       (list "--format" "json" "--envelope") (and feishu-project-cli-profile (list "--profile" feishu-project-cli-profile)))))
    (feishu-project-cli--ensure-auth
     (lambda () (let ((process-environment (feishu-project-cli--safe-environment)))
                  (feishu-project-cli--process argv (lambda (text) (feishu-project-cli--envelope-result text (lambda (data) (funcall success (list :action action :data data))) failure)) failure))) failure)))
(defun feishu-project-cli--action-dispatch (action payload success failure)
  (if (memq action '(upload-file download-file)) (feishu-project-cli--file-action action (plist-get payload :source) payload success failure)
    (feishu-project-cli--action action payload success failure)))
(feishu-project-register-backend 'cli (list :list #'feishu-project-cli--list :mql #'feishu-project-cli--mql :detail #'feishu-project-cli--detail :action #'feishu-project-cli--action-dispatch :types #'feishu-project-cli--types :capabilities '(attachment-shortcuts)))
(provide 'feishu-project-cli)
;;; feishu-project-cli.el ends here
