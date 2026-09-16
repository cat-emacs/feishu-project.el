;;; feishu-project.el --- Browse Feishu Project work items -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Misaka

;; Author: Misaka <chuxubank@qq.com>
;; Maintainer: Misaka <chuxubank@qq.com>
;; Version: 0.1.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, hypermedia
;; URL: https://github.com/cat-emacs/feishu-project.el

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Browse Feishu Project work items with OpenAPI filters, or use a
;; user-supplied MQL backend.  Credentials are read from auth-source by
;; default; see `feishu-project-access-token'.

;;; Code:

(require 'auth-source)
(require 'browse-url)
(require 'cl-lib)
(require 'json)
(require 'pp)
(require 'subr-x)
(require 'tabulated-list)
(require 'url)
(require 'url-http)

(defvar url-http-end-of-headers)
(defvar url-http-response-status)

(defgroup feishu-project nil
  "Emacs interface to Feishu Project."
  :group 'tools
  :prefix "feishu-project-")

(defcustom feishu-project-host
  (or (getenv "FEISHU_PROJECT_HOST") "https://project.feishu.cn")
  "Feishu Project host, without a trailing slash.
The default comes from FEISHU_PROJECT_HOST when it is set."
  :type 'string)

(defcustom feishu-project-project-key
  (getenv "FEISHU_PROJECT_KEY")
  "Default project key or project simple name.
The default comes from FEISHU_PROJECT_KEY when it is set."
  :type '(choice (const :tag "Prompt" nil) string))

(defcustom feishu-project-work-item-type-keys nil
  "Default work item type keys used by OpenAPI filters."
  :type '(repeat string))

(defcustom feishu-project-user-key
  (getenv "FEISHU_PROJECT_USER_KEY")
  "User key sent with a plugin access token.
The default comes from FEISHU_PROJECT_USER_KEY when it is set.  Leave nil
when the token is a user access token."
  :type '(choice (const :tag "Not required" nil) string))

(defcustom feishu-project-access-token nil
  "OpenAPI token, or a function returning it.
When nil, use FEISHU_PROJECT_TOKEN or auth-source."
  :type '(choice (const :tag "Use environment or auth-source" nil)
                 string function))

(defcustom feishu-project-page-size 100
  "Number of work items fetched per OpenAPI page."
  :type 'integer)

(defcustom feishu-project-mql-function nil
  "Function used to execute MQL queries.
It receives PROJECT-KEY and MQL and returns a list of work item alists.
MQL is exposed by Feishu Project MCP rather than the documented OpenAPI;
configure this hook to bridge that service into Emacs."
  :type '(choice (const :tag "Not configured" nil) function))

(defcustom feishu-project-saved-mql nil
  "Named MQL queries offered by `feishu-project-mql'.
Each entry has the form (NAME . QUERY)."
  :type '(alist :key-type string :value-type string))

(defcustom feishu-project-list-columns
  '(("ID" 13 feishu-project--item-id)
    ("Type" 12 feishu-project--item-type)
    ("Status" 18 feishu-project--item-status)
    ("Updated" 17 feishu-project--item-updated)
    ("Name" 60 feishu-project--item-name))
  "Columns displayed by `feishu-project-list-mode'.
Each element is (TITLE WIDTH VALUE-FUNCTION)."
  :type '(repeat (list string integer function)))

(defvar-local feishu-project--items nil)
(defvar-local feishu-project--query nil)
(defvar-local feishu-project--query-kind 'filter)
(defvar-local feishu-project--project-key nil)
(defvar-local feishu-project--type-keys nil)
(defvar-local feishu-project--page 1)
(defvar-local feishu-project--total nil)

(defun feishu-project--host ()
  "Return the configured host without a trailing slash."
  (string-remove-suffix "/" feishu-project-host))

(defun feishu-project--auth-host ()
  "Return the host name used for auth-source lookup."
  (url-host (url-generic-parse-url (feishu-project--host))))

(defun feishu-project--secret-string (secret)
  "Resolve SECRET returned by auth-source to a string."
  (if (functionp secret) (funcall secret) secret))

(defun feishu-project--token ()
  "Return the configured OpenAPI token."
  (or (and (functionp feishu-project-access-token)
           (funcall feishu-project-access-token))
      (and (stringp feishu-project-access-token)
           (not (string-empty-p feishu-project-access-token))
           feishu-project-access-token)
      (getenv "FEISHU_PROJECT_TOKEN")
      (let ((entry (car (auth-source-search
                         :host (feishu-project--auth-host)
                         :require '(:secret)
                         :max 1))))
        (and entry (feishu-project--secret-string
                    (plist-get entry :secret))))
      (user-error "Set FEISHU_PROJECT_TOKEN or add %s to auth-source"
                  (feishu-project--auth-host))))

(defun feishu-project--headers ()
  "Return HTTP headers for an OpenAPI request."
  (append `(("Content-Type" . "application/json")
            ("Accept" . "application/json")
            ("X-Plugin-Token" . ,(feishu-project--token)))
          (when (and feishu-project-user-key
                     (not (string-empty-p feishu-project-user-key)))
            `(("X-User-Key" . ,feishu-project-user-key)))))

(defun feishu-project--json-body (body)
  "Encode BODY as UTF-8 JSON."
  (encode-coding-string (json-serialize body :null-object nil) 'utf-8))

(defun feishu-project--request (path body)
  "POST BODY to PATH and return the decoded response alist."
  (let* ((url-request-method "POST")
         (url-request-extra-headers (feishu-project--headers))
         (url-request-data (feishu-project--json-body body))
         (url (concat (feishu-project--host) path))
         (buffer (url-retrieve-synchronously url t t 30)))
    (unless buffer
      (error "Feishu Project request timed out: %s" path))
    (unwind-protect
        (with-current-buffer buffer
          (let ((status url-http-response-status)
                (header-end url-http-end-of-headers))
            (unless (and status (integer-or-marker-p header-end))
              (error "Invalid HTTP response from Feishu Project"))
            (goto-char header-end)
            (let ((response (json-parse-buffer
                             :object-type 'alist
                             :array-type 'list
                             :null-object nil
                             :false-object nil)))
              (unless (and (<= 200 status) (< status 300))
                (error "Feishu Project HTTP %d: %s" status response))
              (let ((code (alist-get 'err_code response)))
                (unless (or (null code) (equal code 0))
                  (error "Feishu Project error %s: %s"
                         code (or (alist-get 'err_msg response)
                                  (alist-get 'err response)))))
              response)))
      (kill-buffer buffer))))

(defun feishu-project--alist-get (key alist)
  "Return KEY from ALIST, accepting symbol and string keys."
  (or (alist-get key alist)
      (alist-get (symbol-name key) alist nil nil #'equal)))

(defun feishu-project--item-id (item)
  "Return ITEM's ID as a string."
  (format "%s" (or (feishu-project--alist-get 'id item)
                    (feishu-project--alist-get 'work_item_id item)
                    "")))

(defun feishu-project--item-name (item)
  "Return ITEM's display name."
  (format "%s" (or (feishu-project--alist-get 'name item)
                    (feishu-project--alist-get 'title item)
                    "")))

(defun feishu-project--item-type (item)
  "Return ITEM's work item type key."
  (format "%s" (or (feishu-project--alist-get 'work_item_type_key item)
                    (feishu-project--alist-get 'work_item_type item)
                    "")))

(defun feishu-project--item-status (item)
  "Return ITEM's current status or node names."
  (let* ((status (feishu-project--alist-get 'work_item_status item))
         (nodes (feishu-project--alist-get 'current_nodes item))
         (state (and (listp status)
                     (feishu-project--alist-get 'state_key status))))
    (or state
        (and nodes
             (mapconcat (lambda (node)
                          (format "%s" (or (feishu-project--alist-get 'name node)
                                            (feishu-project--alist-get 'id node))))
                        nodes ", "))
        (feishu-project--alist-get 'current_status_name item)
        (feishu-project--alist-get 'sub_stage item)
        "")))

(defun feishu-project--format-time (milliseconds)
  "Format MILLISECONDS since the epoch for display."
  (if (numberp milliseconds)
      (format-time-string "%Y-%m-%d %H:%M" (/ milliseconds 1000.0))
    ""))

(defun feishu-project--item-updated (item)
  "Return ITEM's update time for display."
  (feishu-project--format-time
   (feishu-project--alist-get 'updated_at item)))

(defun feishu-project--item-project (item)
  "Return ITEM's project key or simple name."
  (format "%s" (or (feishu-project--alist-get 'simple_name item)
                    (feishu-project--alist-get 'project_key item)
                    feishu-project--project-key
                    "")))

(defun feishu-project-item-url (item)
  "Return the browser URL for ITEM."
  (format "%s/%s/%s/detail/%s"
          (feishu-project--host)
          (url-hexify-string (feishu-project--item-project item))
          (url-hexify-string (feishu-project--item-type item))
          (url-hexify-string (feishu-project--item-id item))))

(defun feishu-project--read-project-key ()
  "Read a project key, using the configured default."
  (read-string "Project key or simple name: "
               feishu-project-project-key nil
               feishu-project-project-key))

(defun feishu-project--read-type-keys ()
  "Read comma-separated work item type keys."
  (let* ((default (string-join feishu-project-work-item-type-keys ","))
         (value (read-string "Work item type keys: " default)))
    (unless (string-empty-p value)
      (split-string value "[[:space:]]*,[[:space:]]*" t))))

(defun feishu-project--filter-page (project-key type-keys page &optional name)
  "Return one PAGE of work items from PROJECT-KEY and TYPE-KEYS.
Filter by NAME when non-nil."
  (let* ((path (format "/open_api/%s/work_item/filter"
                       (url-hexify-string project-key)))
         (body `((work_item_type_keys . ,(vconcat type-keys))
                 (page_num . ,page)
                 (page_size . ,feishu-project-page-size)))
         (body (if (and name (not (string-empty-p name)))
                   (cons `(work_item_name . ,name) body)
                 body)))
    (feishu-project--request path body)))

(defun feishu-project--detail (item)
  "Fetch and return complete details for ITEM."
  (let* ((project (feishu-project--item-project item))
         (type (feishu-project--item-type item))
         (path (format "/open_api/%s/work_item/%s/query"
                       (url-hexify-string project)
                       (url-hexify-string type)))
         (response (feishu-project--request
                    path `((work_item_ids . ,(vector
                                               (string-to-number
                                                (feishu-project--item-id item))))))))
    (car (feishu-project--alist-get 'data response))))

(defun feishu-project--mql-items (project-key mql)
  "Execute MQL for PROJECT-KEY and normalize the returned work items."
  (unless (functionp feishu-project-mql-function)
    (user-error "Configure `feishu-project-mql-function' to use Feishu Project MCP search_by_mql"))
  (let ((result (funcall feishu-project-mql-function project-key mql)))
    (cond
     ((null result) nil)
     ((and (listp result) (not (keywordp (car result)))) result)
     (t (error "MQL backend must return a list of work item alists")))))

(defun feishu-project--entry (item)
  "Create a tabulated list entry for ITEM."
  (list item
        (vconcat
         (mapcar (lambda (column)
                   (let ((value (funcall (nth 2 column) item)))
                     (if (stringp value) value (format "%s" value))))
                 feishu-project-list-columns))))

(defun feishu-project--refresh ()
  "Refresh the current Feishu Project work item list."
  (condition-case err
      (let ((inhibit-read-only t))
        (setq feishu-project--items
              (pcase feishu-project--query-kind
                ('mql (feishu-project--mql-items
                       feishu-project--project-key
                       feishu-project--query))
                (_ (let* ((response
                           (feishu-project--filter-page
                            feishu-project--project-key
                            feishu-project--type-keys
                            feishu-project--page
                            feishu-project--query))
                          (pagination
                           (feishu-project--alist-get 'pagination response)))
                     (setq feishu-project--total
                           (feishu-project--alist-get 'total pagination))
                     (feishu-project--alist-get 'data response)))))
        (setq tabulated-list-entries
              (mapcar #'feishu-project--entry feishu-project--items))
        (tabulated-list-print t)
        (message "Feishu Project: %d item%s%s"
                 (length feishu-project--items)
                 (if (= (length feishu-project--items) 1) "" "s")
                 (if feishu-project--total
                     (format " of %s" feishu-project--total)
                   "")))
    (error
     (setq tabulated-list-entries nil)
     (tabulated-list-print t)
     (signal (car err) (cdr err)))))

(defun feishu-project-item-at-point ()
  "Return the work item at point or signal a user error."
  (or (tabulated-list-get-id)
      (user-error "No Feishu Project work item at point")))

(defun feishu-project-open ()
  "Open the work item at point in a browser."
  (interactive)
  (browse-url (feishu-project-item-url
               (feishu-project-item-at-point))))

(defun feishu-project-copy-url ()
  "Copy the work item URL at point."
  (interactive)
  (let ((url (feishu-project-item-url
              (feishu-project-item-at-point))))
    (kill-new url)
    (message "Copied %s" url)))

(defun feishu-project-copy-id ()
  "Copy the work item ID at point."
  (interactive)
  (let ((id (feishu-project--item-id
             (feishu-project-item-at-point))))
    (kill-new id)
    (message "Copied %s" id)))

(defun feishu-project--insert-value (value)
  "Insert VALUE in a readable form at point."
  (cond
   ((null value) (insert "-"))
   ((stringp value) (insert value))
   ((numberp value) (insert (number-to-string value)))
   ((eq value t) (insert "true"))
   (t (pp value (current-buffer)))))

(defun feishu-project--field-name (field)
  "Return a display name for FIELD."
  (let ((alias (feishu-project--alist-get 'field_alias field)))
    (format "%s" (if (and (stringp alias) (not (string-empty-p alias)))
                       alias
                     (or (feishu-project--alist-get 'field_key field)
                         "field")))))

(defun feishu-project--render-detail (item)
  "Render ITEM into the current detail buffer."
  (let ((inhibit-read-only t)
        (fields (feishu-project--alist-get 'fields item)))
    (erase-buffer)
    (insert (propertize (feishu-project--item-name item)
                        'face '(:height 1.35 :weight bold))
            "\n\n")
    (dolist (row `(("ID" . ,(feishu-project--item-id item))
                   ("Type" . ,(feishu-project--item-type item))
                   ("Status" . ,(feishu-project--item-status item))
                   ("Project" . ,(feishu-project--item-project item))
                   ("Created" . ,(feishu-project--format-time
                                    (feishu-project--alist-get 'created_at item)))
                   ("Updated" . ,(feishu-project--item-updated item))
                   ("URL" . ,(feishu-project-item-url item))))
      (insert (propertize (format "%-12s" (car row)) 'face 'bold))
      (feishu-project--insert-value (cdr row))
      (insert "\n"))
    (when fields
      (insert "\n" (propertize "Fields" 'face '(:height 1.15 :weight bold)) "\n\n")
      (dolist (field fields)
        (insert (propertize (format "%s\n" (feishu-project--field-name field))
                            'face 'bold)
                "  ")
        (feishu-project--insert-value
         (feishu-project--alist-get 'field_value field))
        (unless (bolp) (insert "\n"))
        (insert "\n")))
    (goto-char (point-min))))

(defun feishu-project-show ()
  "Show details for the work item at point."
  (interactive)
  (let* ((summary (feishu-project-item-at-point))
         (item (feishu-project--detail summary))
         (buffer (get-buffer-create
                  (format "*Feishu Project %s*"
                          (feishu-project--item-id summary)))))
    (unless item
      (user-error "Feishu Project returned no details for %s"
                  (feishu-project--item-id summary)))
    (with-current-buffer buffer
      (feishu-project-detail-mode)
      (setq-local feishu-project--items (list item))
      (feishu-project--render-detail item))
    (pop-to-buffer buffer)))

(defun feishu-project-detail-open ()
  "Open the detail buffer's work item in a browser."
  (interactive)
  (browse-url (feishu-project-item-url (car feishu-project--items))))

(defun feishu-project-detail-copy-url ()
  "Copy the detail buffer's work item URL."
  (interactive)
  (let ((url (feishu-project-item-url (car feishu-project--items))))
    (kill-new url)
    (message "Copied %s" url)))

(defun feishu-project-next-page ()
  "Display the next OpenAPI result page."
  (interactive)
  (when (eq feishu-project--query-kind 'mql)
    (user-error "MQL pagination belongs to the configured backend"))
  (when (and feishu-project--total
             (>= (* feishu-project--page feishu-project-page-size)
                 feishu-project--total))
    (user-error "Already on the last page"))
  (cl-incf feishu-project--page)
  (feishu-project--refresh))

(defun feishu-project-previous-page ()
  "Display the previous OpenAPI result page."
  (interactive)
  (when (<= feishu-project--page 1)
    (user-error "Already on the first page"))
  (cl-decf feishu-project--page)
  (feishu-project--refresh))

(defun feishu-project--display (project-key type-keys kind query)
  "Display PROJECT-KEY results of KIND using TYPE-KEYS and QUERY."
  (let ((buffer (get-buffer-create "*Feishu Project*")))
    (with-current-buffer buffer
      (feishu-project-list-mode)
      (setq feishu-project--project-key project-key
            feishu-project--type-keys type-keys
            feishu-project--query-kind kind
            feishu-project--query query
            feishu-project--page 1
            feishu-project--total nil)
      (feishu-project--refresh))
    (pop-to-buffer buffer)))

;;;###autoload
(defun feishu-project-list (project-key type-keys &optional name)
  "List work items in PROJECT-KEY of TYPE-KEYS, optionally matching NAME."
  (interactive
   (list (feishu-project--read-project-key)
         (or (feishu-project--read-type-keys)
             (user-error "At least one work item type key is required"))
         (let ((value (read-string "Name contains (empty for all): ")))
           (unless (string-empty-p value) value))))
  (feishu-project--display project-key type-keys 'filter name))

;;;###autoload
(defun feishu-project-mql (project-key mql)
  "Run MQL in PROJECT-KEY using `feishu-project-mql-function'."
  (interactive
   (let* ((project (feishu-project--read-project-key))
          (saved (and feishu-project-saved-mql
                      (completing-read "Saved MQL (empty to enter): "
                                       (mapcar #'car feishu-project-saved-mql)
                                       nil t)))
          (query (if (and saved (not (string-empty-p saved)))
                     (alist-get saved feishu-project-saved-mql nil nil #'equal)
                   (read-string "MQL: "))))
     (list project query)))
  (when (string-empty-p mql)
    (user-error "MQL query must not be empty"))
  (feishu-project--display project-key nil 'mql mql))

(defvar-keymap feishu-project-list-mode-map
  :parent tabulated-list-mode-map
  "RET" #'feishu-project-show
  "o" #'feishu-project-open
  "w" #'feishu-project-copy-id
  "W" #'feishu-project-copy-url
  "g" #'revert-buffer
  "n" #'feishu-project-next-page
  "p" #'feishu-project-previous-page
  "q" #'quit-window)

(define-derived-mode feishu-project-list-mode tabulated-list-mode "Feishu Project"
  "Major mode for browsing Feishu Project work items."
  (setq tabulated-list-format
        (vconcat
         (mapcar (lambda (column)
                   (list (nth 0 column) (nth 1 column) t))
                 feishu-project-list-columns))
        tabulated-list-padding 2
        tabulated-list-sort-key '("Updated" . t))
  (add-hook 'tabulated-list-revert-hook #'feishu-project--refresh nil t)
  (tabulated-list-init-header))

(defvar-keymap feishu-project-detail-mode-map
  :parent special-mode-map
  "o" #'feishu-project-detail-open
  "W" #'feishu-project-detail-copy-url
  "q" #'quit-window)

(define-derived-mode feishu-project-detail-mode special-mode "Feishu Project Detail"
  "Major mode for displaying a Feishu Project work item.")

(provide 'feishu-project)

;;; feishu-project.el ends here

