;;; feishu-project.el --- Browse Feishu Project work items -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Misaka
;; Author: Misaka <chuxubank@qq.com>
;; Maintainer: Misaka <chuxubank@qq.com>
;; Version: 0.2.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, hypermedia
;; URL: https://github.com/cat-emacs/feishu-project.el

;; This file is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.

;;; Commentary:

;; Common UI and callback-first backend protocol.  The OpenAPI backend remains
;; the default; the MCP backend is optional and loaded on demand.

;;; Code:

(require 'browse-url)
(require 'cl-lib)
(require 'pp)
(require 'subr-x)
(require 'tabulated-list)
(require 'url)

(defgroup feishu-project nil
  "Emacs interface to Feishu Project."
  :group 'tools
  :prefix "feishu-project-")

(defcustom feishu-project-host
  (or (getenv "FEISHU_PROJECT_HOST") "https://project.feishu.cn")
  "Feishu Project host, without a trailing slash."
  :type 'string)

(defcustom feishu-project-project-key
  (getenv "FEISHU_PROJECT_KEY")
  "Default project key or project simple name."
  :type '(choice (const :tag "Prompt" nil) string))

(defcustom feishu-project-backend 'openapi
  "Backend used for Feishu Project requests."
  :type '(choice (const :tag "OpenAPI" openapi)
                 (const :tag "MCP" mcp)
                 (const :tag "Official Meegle CLI (v1.0.23)" cli)))

(defcustom feishu-project-page-size 100
  "Preferred number of rows requested by a backend."
  :type 'integer)

(defcustom feishu-project-query-history nil
  "Recently used Feishu Project MQL strings."
  :type '(repeat string)
  :group 'feishu-project)

(defcustom feishu-project-query-history-length 30
  "Maximum number of remembered MQL queries."
  :type 'integer
  :group 'feishu-project)

(defun feishu-project--remember-query (mql)
  "Remember MQL without Custom persistence."
  (setq feishu-project-query-history
        (seq-take (cons mql (delete mql feishu-project-query-history))
                  feishu-project-query-history-length)))

(defcustom feishu-project-project-aliases nil
  "Named Feishu Project spaces as (NAME . PROJECT-KEY) pairs."
  :type '(alist :key-type string :value-type string))

(defcustom feishu-project-detail-reuse-buffer nil
  "When non-nil, reuse one detail buffer for all work items."
  :type 'boolean)

(defcustom feishu-project-saved-mql nil
  "Named MQL queries offered by `feishu-project-mql'."
  :type '(alist :key-type string :value-type string))

(defcustom feishu-project-list-columns
  '(("ID" 13 feishu-project--item-id)
    ("Type" 12 feishu-project--item-type)
    ("Status" 18 feishu-project--item-status)
    ("Updated" 25 feishu-project--item-updated)
    ("Name" 60 feishu-project--item-name))
  "Columns displayed by `feishu-project-list-mode'."
  :type '(repeat (list string integer function)))

(defvar feishu-project--backends (make-hash-table :test #'eq)
  "Mapping of backend names to callback-first protocol plists.")

(defvar-local feishu-project--backend-name nil)

(defun feishu-project-register-backend (name protocol)
  "Register NAME with callback-first PROTOCOL.
A list function receives PROJECT, TYPE-SPEC, NAME, PAGE-TOKEN, CONTEXT,
SUCCESS, and FAILURE.  An MQL function omits TYPE-SPEC and NAME.  A detail
function receives ITEM, CONTEXT, SUCCESS, and FAILURE.  CONTEXT is a plist
with the originating list buffer and generation.  Results contain :items or
:item, :total, and an opaque :continuation where appropriate."
  (puthash name protocol feishu-project--backends))

(defun feishu-project--backend (&optional name)
  "Return backend NAME, loading it only when needed."
  (let ((name (or name feishu-project--backend-name feishu-project-backend)))
    (unless (gethash name feishu-project--backends)
      (pcase name
        ('openapi (require 'feishu-project-openapi))
        ('mcp (require 'feishu-project-mcp))
        ('cli (require 'feishu-project-cli))
        (_ (user-error "Unknown Feishu Project backend: %S" name))))
    (or (gethash name feishu-project--backends)
        (error "Feishu Project backend %S did not register" name))))

(defun feishu-project--backend-function (key &optional name)
  "Return backend NAME's function for KEY."
  (let ((name (or name feishu-project--backend-name feishu-project-backend)))
    (or (plist-get (feishu-project--backend name) key)
        (user-error "Backend %S does not support %s" name key))))

(defun feishu-project--action (action payload success failure &optional backend)
  "Run backend ACTION with callback SUCCESS or explicit capability FAILURE."
  (let* ((backend (or backend feishu-project--backend-name feishu-project-backend))
         (function (plist-get (feishu-project--backend backend) :action)))
    (if function
        (funcall function action payload success failure)
      (funcall failure (format "Feishu Project backend %s does not support %s; select an action-capable backend"
                               backend action)))))

(defun feishu-project--backend-capable-p (capability &optional backend)
  "Return non-nil when BACKEND declares CAPABILITY."
  (memq capability (plist-get (feishu-project--backend backend) :capabilities)))

(defun feishu-project--get (key object)
  "Return KEY from plist or alist OBJECT, accepting symbol or string keys."
  (or (and (listp object) (plist-get object key))
      (and (listp object) (alist-get key object))
      (and (symbolp key) (listp object)
           (alist-get (symbol-name key) object nil nil #'equal))))

(defvar-local feishu-project--items nil)
(defvar-local feishu-project--query nil)
(defvar-local feishu-project--query-kind nil)
(defvar-local feishu-project--project-key nil)
(defvar-local feishu-project--type-spec nil)
(defvar-local feishu-project--current-page-token nil)
(defvar-local feishu-project--next-page-token nil)
(defvar-local feishu-project--page-history nil)
(defvar-local feishu-project--total nil)
(defvar-local feishu-project--loading nil)
(defvar-local feishu-project--generation 0)
(defvar-local feishu-project--filters nil)
(defvar-local feishu-project--marked nil)
(defvar-local feishu-project--active-columns nil)
(defvar-local feishu-project--detail-identity nil)
(defvar-local feishu-project--detail-generation 0)
(defvar-local feishu-project--detail-sections nil)
(defvar-local feishu-project--detail-section-data nil)

(defun feishu-project--host ()
  "Return the configured host without a trailing slash."
  (string-remove-suffix "/" feishu-project-host))

(defun feishu-project--item-id (item)
  "Return ITEM's ID as a string."
  (format "%s" (or (feishu-project--get 'id item)
                    (feishu-project--get 'work_item_id item)
                    "")))

(defun feishu-project--item-name (item)
  "Return ITEM's display name."
  (format "%s" (or (feishu-project--get 'name item)
                    (feishu-project--get 'title item)
                    "")))

(defun feishu-project--item-type-key (item)
  "Return ITEM's stable work item type key."
  (format "%s" (or (feishu-project--get 'work_item_type_key item)
                    (feishu-project--get 'work_item_type item)
                    "")))

(defun feishu-project--item-type (item)
  "Return ITEM's work item type display name."
  (format "%s" (or (feishu-project--get 'work_item_type item)
                    (feishu-project--get 'work_item_type_key item)
                    "")))

(defun feishu-project--item-status (item)
  "Return ITEM's current status display name."
  (let* ((status (feishu-project--get 'work_item_status item))
         (option (and (listp status)
                      (listp (car status))
                      (car status))))
    (format "%s" (or (and (listp status)
                            (or (feishu-project--get 'name status)
                                (feishu-project--get 'label status)
                                (feishu-project--get 'state_key status)))
                     (and option
                          (or (feishu-project--get 'name option)
                              (feishu-project--get 'label option)
                              (feishu-project--get 'state_key option)))
                     (feishu-project--get 'current_status_name item)
                     ""))))

(defun feishu-project--item-updated (item)
  "Return ITEM's update time for display."
  (format "%s" (or (feishu-project--get 'updated_at item)
                    (feishu-project--get 'update_time item)
                    "")))

(defun feishu-project--item-project (item)
  "Return ITEM's project simple name or key."
  (format "%s" (or (feishu-project--get 'simple_name item)
                    (feishu-project--get 'project_key item)
                    feishu-project--project-key
                    "")))

(defun feishu-project-item-url (item)
  "Return the browser URL for ITEM.
Arbitrary MQL results without a type cannot form a Feishu detail URL."
  (when (string-empty-p (feishu-project--item-type-key item))
    (user-error "This result has no work item type; its detail URL is unavailable"))
  (format "%s/%s/%s/detail/%s"
          (feishu-project--host)
          (url-hexify-string (feishu-project--item-project item))
          (url-hexify-string (feishu-project--item-type-key item))
          (url-hexify-string (feishu-project--item-id item))))

(defun feishu-project--request-current-p (context)
  "Return non-nil when CONTEXT still identifies the active list request."
  (let ((buffer (plist-get context :buffer))
        (generation (plist-get context :generation)))
    (and (buffer-live-p buffer)
         (with-current-buffer buffer
           (= generation feishu-project--generation)))))

(defun feishu-project--row-identity (item &optional backend project)
  "Return stable identity for ITEM in BACKEND and PROJECT."
  (list (or backend feishu-project--backend-name feishu-project-backend)
        (or project (feishu-project--item-project item))
        (feishu-project--item-type-key item)
        (feishu-project--item-id item)))

(defun feishu-project--column-normalize (column)
  "Normalize legacy COLUMN tuple to a declarative plist."
  (if (keywordp (car column)) column
    (list :title (nth 0 column) :width (nth 1 column) :getter (nth 2 column))))

(defun feishu-project--columns ()
  "Return active normalized columns."
  (mapcar #'feishu-project--column-normalize
          (or feishu-project--active-columns feishu-project-list-columns)))

(defun feishu-project-toggle-mark (&optional item)
  "Toggle a mark on ITEM or the item at point."
  (interactive)
  (let ((item (or item (feishu-project-item-at-point))))
    (unless feishu-project--marked
      (setq feishu-project--marked (make-hash-table :test #'equal)))
    (let ((key (feishu-project--row-identity item)))
      (if (gethash key feishu-project--marked) (remhash key feishu-project--marked)
        (puthash key item feishu-project--marked)))
    (feishu-project--render-list)))

(defun feishu-project-unmark-all ()
  "Clear all marks." (interactive)
  (when feishu-project--marked (clrhash feishu-project--marked))
  (feishu-project--render-list))

(defun feishu-project-marked-items ()
  "Return marked items, or all visible items when none are marked."
  (if (and feishu-project--marked (> (hash-table-count feishu-project--marked) 0))
      (let (items) (maphash (lambda (_ item) (push item items)) feishu-project--marked) (nreverse items))
    feishu-project--items))

(defun feishu-project--entry (item)
  "Create a tabulated-list entry for ITEM."
  (let ((marked (and feishu-project--marked
                     (gethash (feishu-project--row-identity item) feishu-project--marked))))
    (list item (vconcat (mapcar (lambda (column)
                                  (let ((value (format "%s" (funcall (plist-get column :getter) item))))
                                    (if marked (propertize value 'face 'warning) value)))
                                (feishu-project--columns))))))

(defun feishu-project--render-list ()
  "Render the committed items in the current list buffer."
  (let ((inhibit-read-only t))
    (setq tabulated-list-entries
          (mapcar #'feishu-project--entry feishu-project--items))
    (tabulated-list-print t)))

(defun feishu-project--render-loading ()
  "Render the loading state in the current list buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert "Loading Feishu Project work items…\n")))

(defun feishu-project--finish-list (result context page-token history)
  "Render RESULT for CONTEXT and commit PAGE-TOKEN and HISTORY on success."
  (when (feishu-project--request-current-p context)
    (with-current-buffer (plist-get context :buffer)
      (setq feishu-project--loading nil
            feishu-project--items (plist-get result :items)
            feishu-project--current-page-token
            (if (plist-member result :current-page-token)
                (plist-get result :current-page-token)
              page-token)
            feishu-project--next-page-token (plist-get result :continuation)
            feishu-project--page-history history
            feishu-project--total (plist-get result :total))
      (feishu-project--render-list))))

(defun feishu-project--fail-list (error context)
  "Show ERROR for CONTEXT without changing committed page state."
  (when (feishu-project--request-current-p context)
    (with-current-buffer (plist-get context :buffer)
      (setq feishu-project--loading nil)
      (feishu-project--render-list)
      (message "Feishu Project: %s" error))))

(defun feishu-project--invoke-list-backend (context page-token history)
  "Invoke the active backend for CONTEXT, PAGE-TOKEN and proposed HISTORY."
  (let ((success (lambda (result)
                   (feishu-project--finish-list result context page-token history)))
        (failure (lambda (error)
                   (feishu-project--fail-list error context))))
    (condition-case err
        (pcase feishu-project--query-kind
          ('list
           (funcall (feishu-project--backend-function :list)
                    feishu-project--project-key
                    feishu-project--type-spec
                    feishu-project--query
                    page-token
                    context
                    success
                    failure))
          ('mql
           (funcall (feishu-project--backend-function :mql)
                    feishu-project--project-key
                    feishu-project--query
                    page-token
                    context
                    success
                    failure)))
      (error (funcall failure (error-message-string err))))))

(defun feishu-project--request-page (page-token history)
  "Request PAGE-TOKEN with proposed HISTORY from the current list buffer.
Page state is committed only by a successful callback."
  (when feishu-project--loading
    (user-error "Feishu Project request is already in progress"))
  (let* ((buffer (current-buffer))
         (generation (cl-incf feishu-project--generation))
         (context (list :buffer buffer :generation generation
                        :filters (copy-tree feishu-project--filters))))
    (setq feishu-project--loading t)
    (feishu-project--render-loading)
    (feishu-project--invoke-list-backend context page-token history)))

(defun feishu-project--display (project-key type-spec kind query &optional filters)
  "Display KIND QUERY in PROJECT-KEY using TYPE-SPEC and FILTERS."
  (when (eq kind 'mql) (feishu-project--remember-query query))
  (let ((buffer (get-buffer-create "*Feishu Project*")))
    (with-current-buffer buffer
      (feishu-project-list-mode)
      (setq feishu-project--backend-name feishu-project-backend
            feishu-project--project-key project-key
            feishu-project--type-spec type-spec
            feishu-project--query-kind kind
            feishu-project--query query
            feishu-project--current-page-token nil
            feishu-project--next-page-token nil
            feishu-project--page-history nil
            feishu-project--total nil
            feishu-project--filters filters)
      (feishu-project--request-page nil nil))
    (pop-to-buffer buffer)))

(defun feishu-project-item-at-point ()
  "Return the work item at point or signal a user error."
  (or (tabulated-list-get-id)
      (user-error "No Feishu Project work item at point")))

(defun feishu-project-open ()
  "Open the work item at point in a browser."
  (interactive)
  (browse-url (feishu-project-item-url (feishu-project-item-at-point))))

(defun feishu-project-copy-url ()
  "Copy the work item URL at point."
  (interactive)
  (let ((url (feishu-project-item-url (feishu-project-item-at-point))))
    (kill-new url)
    (message "Copied %s" url)))

(defun feishu-project-copy-id ()
  "Copy the work item ID at point."
  (interactive)
  (let ((id (feishu-project--item-id (feishu-project-item-at-point))))
    (kill-new id)
    (message "Copied %s" id)))

(defun feishu-project--insert-value (value)
  "Insert VALUE in readable form without raw Lisp dumps."
  (cond ((null value) (insert "-")) ((stringp value) (insert value))
        ((numberp value) (insert (number-to-string value)))
        ((listp value) (insert (string-join (delq nil (mapcar (lambda (x) (and (consp x) (format "%s" (cdr x)))) value)) ", ")))
        (t (insert (format "%s" value)))))

(defun feishu-project--render-detail (item)
  "Render ITEM and structured sections in the current detail buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (when (feishu-project--get 'detail-truncated item)
      (insert "Warning: detail fields are truncated; refresh cannot provide omitted fields.\n\n"))
    (insert (propertize (feishu-project--item-name item) 'face '(:height 1.35 :weight bold)) "\n\n")
    (dolist (row `(("ID" . ,(feishu-project--item-id item)) ("Type" . ,(feishu-project--item-type item))
                   ("Status" . ,(feishu-project--item-status item)) ("Project" . ,(feishu-project--item-project item))
                   ("Updated" . ,(feishu-project--item-updated item))))
      (insert (propertize (format "%-12s" (car row)) 'face 'bold)) (feishu-project--insert-value (cdr row)) (insert "\n"))
    (insert "\nFields\n------\n")
    (dolist (field (feishu-project--get 'fields item))
      (insert (propertize (format "%s: " (or (feishu-project--get 'field_alias field) (feishu-project--get 'field_key field))) 'face 'bold))
      (feishu-project--insert-value (feishu-project--get 'field_value field)) (insert "\n"))
    (dolist (section feishu-project--detail-sections)
      (insert "\n" (propertize (format "%s\n" (car section)) 'face 'bold) (make-string (length (car section)) ?-) "\n")
      (dolist (line (cdr section)) (insert (format "%s\n" line))))
    (goto-char (point-min))))

(defun feishu-project--detail-buffer-name (item backend)
  "Return collision-resistant detail buffer name for ITEM and BACKEND."
  (if feishu-project-detail-reuse-buffer "*Feishu Project Detail*"
    (format "*Feishu Project %s:%s:%s:%s*" backend (feishu-project--item-project item)
            (feishu-project--item-type-key item) (feishu-project--item-id item))))

(defun feishu-project--detail-current-p (buffer generation identity)
  "Return non-nil when BUFFER still expects GENERATION and IDENTITY."
  (and (buffer-live-p buffer) (with-current-buffer buffer
                                (and (= generation feishu-project--detail-generation)
                                     (equal identity feishu-project--detail-identity)))))

(defun feishu-project--finish-detail (result _context buffer generation identity)
  "Render RESULT only when BUFFER still expects this detail request."
  (when (feishu-project--detail-current-p buffer generation identity)
    (let ((item (plist-get result :item)))
      (with-current-buffer buffer
        (when (equal identity (feishu-project--row-identity item feishu-project--backend-name))
          (setq-local feishu-project--items (list item)) (feishu-project--render-detail item))))))

(defun feishu-project--fail-detail (error _context buffer generation identity)
  "Render ERROR only when BUFFER still expects this detail request."
  (when (feishu-project--detail-current-p buffer generation identity)
    (with-current-buffer buffer (let ((inhibit-read-only t)) (erase-buffer) (insert (format "Feishu Project: %s\n" error))))))

(defun feishu-project--request-detail (item backend buffer &optional context)
  "Fetch ITEM through pinned BACKEND into BUFFER with stale protection."
  (let* ((identity (feishu-project--row-identity item backend))
         (context (or context (list :buffer buffer :generation 0))) generation)
    (with-current-buffer buffer
      (unless (derived-mode-p 'feishu-project-detail-mode) (feishu-project-detail-mode))
      (setq-local feishu-project--backend-name backend feishu-project--detail-identity identity)
      (setq generation (cl-incf feishu-project--detail-generation))
      (let ((inhibit-read-only t)) (erase-buffer) (insert "Loading Feishu Project details…\n")))
    (funcall (feishu-project--backend-function :detail backend) item context
             (lambda (result) (feishu-project--finish-detail result context buffer generation identity))
             (lambda (error) (feishu-project--fail-detail error context buffer generation identity)))))

(defun feishu-project-show ()
  "Show details for the work item at point asynchronously."
  (interactive)
  (let* ((item (feishu-project-item-at-point)) (backend feishu-project--backend-name)
         (buffer (get-buffer-create (feishu-project--detail-buffer-name item backend))))
    (pop-to-buffer buffer)
    (feishu-project--request-detail item backend buffer
                                    (list :buffer (current-buffer) :generation feishu-project--generation))))

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

(defun feishu-project--read-project-key ()
  "Read a project key using the configured default."
  (read-string "Project key or simple name: "
               feishu-project-project-key nil
               feishu-project-project-key))

(defun feishu-project--read-type-keys ()
  "Read comma-separated OpenAPI work item type keys."
  (let* ((default (and (boundp 'feishu-project-work-item-type-keys)
                       (string-join feishu-project-work-item-type-keys ",")))
         (value (read-string "Work item type keys: " default)))
    (unless (string-empty-p value)
      (split-string value "[[:space:]]*,[[:space:]]*" t))))

;;;###autoload
(defun feishu-project-list (project-key type-keys &optional name)
  "List work items in PROJECT-KEY of TYPE-KEYS, optionally matching NAME."
  (interactive
   (progn
     (when (eq feishu-project-backend 'openapi)
       (require 'feishu-project-openapi))
     (list (feishu-project--read-project-key)
           (unless (memq feishu-project-backend '(mcp cli))
             (or (feishu-project--read-type-keys)
                 (user-error "At least one work item type key is required")))
           (let ((value (read-string "Name contains (empty for all): ")))
             (unless (string-empty-p value)
               value)))))
  (feishu-project--display project-key type-keys 'list name))

;;;###autoload
(defun feishu-project-mql (project-key mql)
  "Run MQL in PROJECT-KEY through the selected backend."
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

(defun feishu-project-next-page ()
  "Display the next backend page."
  (interactive)
  (when feishu-project--loading
    (user-error "Feishu Project request is already in progress"))
  (unless feishu-project--next-page-token
    (user-error "Already on the last page"))
  (feishu-project--request-page
   feishu-project--next-page-token
   (cons feishu-project--current-page-token feishu-project--page-history)))

(defun feishu-project-previous-page ()
  "Display the preceding backend page."
  (interactive)
  (when feishu-project--loading
    (user-error "Feishu Project request is already in progress"))
  (unless feishu-project--page-history
    (user-error "Already on the first page"))
  (feishu-project--request-page
   (car feishu-project--page-history)
   (cdr feishu-project--page-history)))

(autoload 'feishu-project-dispatch "feishu-project-workbench")
(autoload 'feishu-project-detail-dispatch "feishu-project-workbench")
(autoload 'feishu-project-detail-refresh "feishu-project-workbench")
(autoload 'feishu-project-toggle-mark "feishu-project-workbench")
(autoload 'feishu-project-unmark-all "feishu-project-workbench")

(defvar-keymap feishu-project-list-mode-map
  :parent tabulated-list-mode-map
  "RET" #'feishu-project-show
  "o" #'feishu-project-open
  "w" #'feishu-project-copy-id
  "W" #'feishu-project-copy-url
  "m" #'feishu-project-toggle-mark
  "u" #'feishu-project-unmark-all
  "?" #'feishu-project-dispatch
  "g" #'revert-buffer
  "n" #'feishu-project-next-page
  "p" #'feishu-project-previous-page
  "q" #'quit-window)

(define-derived-mode feishu-project-list-mode tabulated-list-mode "Feishu Project"
  "Major mode for browsing Feishu Project work items."
  (setq tabulated-list-format
        (vconcat (mapcar (lambda (column)
                           (list (nth 0 column) (nth 1 column) t))
                         feishu-project-list-columns))
        tabulated-list-padding 2
        tabulated-list-sort-key '("Updated" . t))
  (add-hook 'tabulated-list-revert-hook
            (lambda () (feishu-project--request-page nil nil))
            nil t)
  (tabulated-list-init-header))

(defvar-keymap feishu-project-detail-mode-map
  :parent special-mode-map
  "g" #'feishu-project-detail-refresh
  "?" #'feishu-project-detail-dispatch
  "o" #'feishu-project-detail-open
  "W" #'feishu-project-detail-copy-url
  "q" #'quit-window)

(define-derived-mode feishu-project-detail-mode special-mode "Feishu Project Detail"
  "Major mode for displaying a Feishu Project work item.")

(provide 'feishu-project)

;;; feishu-project.el ends here
