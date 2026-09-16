;;; feishu-project-openapi.el --- Feishu Project OpenAPI backend -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Misaka
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; Plugin-token OpenAPI backend.  It is loaded by the common frontend only
;; when the OpenAPI backend is selected.

;;; Code:

(require 'auth-source)
(require 'json)
(require 'url-http)
(require 'feishu-project)

(defvar url-http-end-of-headers)
(defvar url-http-response-status)

(defcustom feishu-project-work-item-type-keys nil
  "Default work item type keys used by OpenAPI filters."
  :group 'feishu-project
  :type '(repeat string))

(defcustom feishu-project-user-key
  (getenv "FEISHU_PROJECT_USER_KEY")
  "User key sent with a plugin or virtual-plugin token."
  :group 'feishu-project
  :type '(choice (const :tag "Not required" nil) string))

(defcustom feishu-project-access-token nil
  "OpenAPI token or function returning it.
When nil, use FEISHU_PROJECT_TOKEN or auth-source."
  :group 'feishu-project
  :type '(choice (const :tag "Use environment or auth-source" nil)
                 string function))

(defcustom feishu-project-mql-function nil
  "Function that executes MQL as (PROJECT-KEY MQL) for OpenAPI.
MQL is not an OpenAPI endpoint; this preserves the former user bridge."
  :group 'feishu-project
  :type '(choice (const :tag "Not configured" nil) function))

(defun feishu-project-openapi--auth-host ()
  "Return the host name used for auth-source lookup."
  (url-host (url-generic-parse-url (feishu-project--host))))

(defun feishu-project-openapi--token ()
  "Return the configured OpenAPI token."
  (or (and (functionp feishu-project-access-token)
           (funcall feishu-project-access-token))
      (and (stringp feishu-project-access-token)
           (not (string-empty-p feishu-project-access-token))
           feishu-project-access-token)
      (getenv "FEISHU_PROJECT_TOKEN")
      (let ((entry (car (auth-source-search
                         :host (feishu-project-openapi--auth-host)
                         :require '(:secret)
                         :max 1))))
        (when entry
          (let ((secret (plist-get entry :secret)))
            (if (functionp secret) (funcall secret) secret))))
      (user-error "Set FEISHU_PROJECT_TOKEN or add %s to auth-source"
                  (feishu-project-openapi--auth-host))))

(defun feishu-project-openapi--headers ()
  "Return HTTP headers for an OpenAPI request."
  (append `(("Content-Type" . "application/json")
            ("Accept" . "application/json")
            ("X-Plugin-Token" . ,(feishu-project-openapi--token)))
          (when (and feishu-project-user-key
                     (not (string-empty-p feishu-project-user-key)))
            `(("X-User-Key" . ,feishu-project-user-key)))))

(defun feishu-project-openapi--request (path body)
  "POST BODY to PATH and return a decoded OpenAPI response."
  (let* ((url-request-method "POST")
         (url-request-extra-headers (feishu-project-openapi--headers))
         (url-request-data
          (encode-coding-string (json-serialize body :null-object nil) 'utf-8))
         (buffer (url-retrieve-synchronously
                  (concat (feishu-project--host) path) t t 30)))
    (unless buffer
      (error "Feishu Project request timed out: %s" path))
    (unwind-protect
        (with-current-buffer buffer
          (unless (and url-http-response-status
                       (integer-or-marker-p url-http-end-of-headers))
            (error "Invalid HTTP response from Feishu Project"))
          (goto-char url-http-end-of-headers)
          (let ((response (json-parse-buffer
                           :object-type 'alist
                           :array-type 'list
                           :null-object nil
                           :false-object nil)))
            (unless (<= 200 url-http-response-status 299)
              (error "Feishu Project HTTP %d: %s"
                     url-http-response-status response))
            (let ((code (alist-get 'err_code response)))
              (unless (or (null code) (equal code 0))
                (error "Feishu Project error %s: %s"
                       code
                       (or (alist-get 'err_msg response)
                           (alist-get 'err response)))))
            response))
      (kill-buffer buffer))))

(defun feishu-project-openapi--list (project types name continuation _context
                                              success failure)
  "Fetch an OpenAPI filter page and call SUCCESS or FAILURE."
  (condition-case err
      (let* ((page (or continuation 1))
             (body `((work_item_type_keys . ,(vconcat types))
                     (page_num . ,page)
                     (page_size . ,feishu-project-page-size)))
             (body (if name (cons `(work_item_name . ,name) body) body))
             (response
              (feishu-project-openapi--request
               (format "/open_api/%s/work_item/filter"
                       (url-hexify-string project))
               body))
             (pagination (alist-get 'pagination response))
             (total (alist-get 'total pagination)))
        (funcall success
                 (list :items (alist-get 'data response)
                       :total total
                       :continuation
                       (and total
                            (< (* page feishu-project-page-size) total)
                            (1+ page)))))
    (error (funcall failure (error-message-string err)))))

(defun feishu-project-openapi--detail (item _context success failure)
  "Fetch ITEM details and call SUCCESS or FAILURE."
  (condition-case err
      (let* ((project (feishu-project--item-project item))
             (type (feishu-project--item-type-key item))
             (response
              (feishu-project-openapi--request
               (format "/open_api/%s/work_item/%s/query"
                       (url-hexify-string project)
                       (url-hexify-string type))
               `((work_item_ids
                  . ,(vector
                      (string-to-number (feishu-project--item-id item))))))))
        (funcall success (list :item (car (alist-get 'data response)))))
    (error (funcall failure (error-message-string err)))))

(defun feishu-project-openapi--mql (project mql continuation _context
                                             success failure)
  "Execute the legacy MQL bridge and call SUCCESS or FAILURE."
  (cond
   (continuation
    (funcall failure "This OpenAPI MQL bridge does not support pagination"))
   ((functionp feishu-project-mql-function)
    (condition-case err
        (funcall success
                 (list :items (funcall feishu-project-mql-function project mql)))
      (error (funcall failure (error-message-string err)))))
   (t
    (funcall failure
             "Configure feishu-project-mql-function or select the MCP backend"))))

(feishu-project-register-backend
 'openapi
 (list :list #'feishu-project-openapi--list
       :mql #'feishu-project-openapi--mql
       :detail #'feishu-project-openapi--detail))

(provide 'feishu-project-openapi)

;;; feishu-project-openapi.el ends here
