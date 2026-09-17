;;; feishu-project-test.el --- Tests for feishu-project -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'feishu-project)
(require 'feishu-project-openapi)
(require 'feishu-project-workbench)
(require 'feishu-project-export)
(require 'feishu-project-cli)

;; Load pure MCP parsers without starting or requiring mcp.el.
(load (expand-file-name "../feishu-project-mcp.el"
                        (file-name-directory load-file-name))
      nil nil t)

(defconst feishu-project-test--item
  '((id . 7107052352)
    (name . "Fix login")
    (work_item_type_key . "bug")
    (simple_name . "example")
    (updated_at . "2026-01-01")
    (work_item_status . ((state_key . "doing")))))

(ert-deftest feishu-project-test-environment-defaults ()
  (let ((process-environment (copy-sequence process-environment)))
    (setenv "FEISHU_PROJECT_HOST" "https://project.example.com")
    (setenv "FEISHU_PROJECT_KEY" "example-project")
    (should (equal (eval (car (get 'feishu-project-host 'standard-value)))
                   "https://project.example.com"))
    (should (equal (eval (car (get 'feishu-project-project-key
                                    'standard-value)))
                   "example-project"))))

(ert-deftest feishu-project-test-item-accessors-and-url ()
  (should (equal (feishu-project--item-id feishu-project-test--item)
                 "7107052352"))
  (should (equal (feishu-project--item-status feishu-project-test--item)
                 "doing"))
  (should (equal (feishu-project--item-type
                  '((work_item_type_key . "bug")
                    (work_item_type . "缺陷")))
                 "缺陷"))
  (should (equal (feishu-project--item-status
                  '((work_item_status
                     . (((key . "doing") (label . "处理中"))))))
                 "处理中"))
  (let ((feishu-project-host "https://project.feishu.cn/"))
    (should (equal (feishu-project-item-url feishu-project-test--item)
                   "https://project.feishu.cn/example/bug/detail/7107052352")))
  (should-error (feishu-project-item-url '((id . 1))) :type 'user-error))

(ert-deftest feishu-project-test-openapi-request-and-headers ()
  (let ((feishu-project-access-token "token")
        (feishu-project-user-key "user"))
    (should (equal (cdr (assoc "X-Plugin-Token"
                               (feishu-project-openapi--headers)))
                   "token"))
    (should (equal (cdr (assoc "X-User-Key"
                               (feishu-project-openapi--headers)))
                   "user"))
    (cl-letf (((symbol-function 'url-retrieve-synchronously)
               (lambda (&rest _)
                 (let ((buffer (generate-new-buffer " *feishu-test*")))
                   (with-current-buffer buffer
                     (insert "HTTP/1.1 200 OK\n\n"
                             "{\"err_code\":0,\"data\":[{\"id\":7}]}")
                     (setq-local url-http-response-status 200)
                     (setq-local url-http-end-of-headers
                                 (copy-marker
                                  (progn
                                    (goto-char (point-min))
                                    (search-forward "\n\n")
                                    (point)))))
                   buffer))))
      (let* ((response (feishu-project-openapi--request "/test" '((ok . t))))
             (item (car (alist-get 'data response))))
        (should (= 7 (alist-get 'id item)))))))

(ert-deftest feishu-project-test-openapi-detail-uses-type-key ()
  (let (requested-path)
    (cl-letf (((symbol-function 'feishu-project-openapi--request)
               (lambda (path _body)
                 (setq requested-path path)
                 '((data . (((id . 7))))))))
      (feishu-project-openapi--detail
       '((id . 7)
         (simple_name . "example")
         (work_item_type_key . "bug")
         (work_item_type . "缺陷"))
       nil #'ignore #'ert-fail)
      (should (equal requested-path
                     "/open_api/example/work_item/bug/query")))))

(ert-deftest feishu-project-test-backend-dispatch-default-openapi ()
  (let ((feishu-project-backend 'openapi))
    (should (eq (plist-get (feishu-project--backend) :list)
                #'feishu-project-openapi--list))))

(ert-deftest feishu-project-test-mcp-content-and-types ()
  (let ((payload
         (feishu-project-mcp--content-json
          '(:content
            ((:type "text"
              :text
              "{\"list\":[{\"type_key\":\"bug\",\"name\":\"Bug\",\"is_disable\":2},{\"type_key\":\"old\",\"name\":\"Old\",\"is_disable\":1}]}"))))))
    (should (equal (feishu-project-mcp--enabled-types payload)
                   '((:key "bug" :name "Bug")))))
  (let ((payload
         (feishu-project-mcp--content-json
          '(:content
            [(:type "text"
              :text
              "{\"list\":[{\"type_key\":\"bug\",\"name\":\"Bug\",\"is_disable\":2}]}")]))))
    (should (equal (feishu-project-mcp--enabled-types payload)
                   '((:key "bug" :name "Bug")))))
  (should-error (feishu-project-mcp--content-json '(:isError t :content nil)))
  (should-error
   (feishu-project-mcp--content-json
    '(:content ((:type "text" :text "not json"))))))

(ert-deftest feishu-project-test-mcp-mql-unpack-and-continuation ()
  (let* ((payload
          (feishu-project-mcp--json
           "{\"list\":[{\"group_infos\":[{\"group_id\":\"1\"}],\"count\":51}],\"session_id\":\"s\",\"data\":{\"1\":[{\"moql_field_list\":[{\"key\":\"work_item_id\",\"value_type\":\"long_value\",\"value\":{\"long_value\":7}},{\"key\":\"name\",\"value_type\":\"string_value\",\"value\":{\"string_value\":\"A\"}},{\"key\":\"work_item_status\",\"value_type\":\"key_label_value_list\",\"value\":{\"key_label_value_list\":[{\"key\":\"doing\",\"label\":\"处理中\"}]}}]}]}}"))
         (result (feishu-project-mcp--mql-result
                  payload "example" '(:key "bug" :name "Bug") "SELECT ..."))
         (item (car (plist-get result :items))))
    (should (= 7 (alist-get 'work_item_id item)))
    (should (equal "bug" (alist-get 'work_item_type_key item)))
    (should (equal "Bug" (alist-get 'work_item_type item)))
    (should (equal "处理中" (feishu-project--item-status item)))
    (should (equal "s"
                   (plist-get (plist-get result :continuation) :session-id)))
    (should (= 1 (plist-get (plist-get result :continuation) :fetched)))
    (should (equal "SELECT ..."
                   (plist-get (plist-get result :current-page-token) :mql)))
    (should (= 2 (plist-get (plist-get result :continuation) :page-num)))))

(ert-deftest feishu-project-test-mcp-detail-normalization ()
  (let* ((payload
          (feishu-project-mcp--json
           "{\"work_item_attribute\":{\"owned_project\":{\"key\":\"p\",\"simple_name\":\"simple\"},\"work_item_id\":\"7\",\"work_item_name\":\"Name\",\"work_item_type\":{\"key\":\"bug\",\"name\":\"Bug\"},\"work_item_status\":{\"name\":\"OPEN\"}},\"work_item_fields\":[{\"key\":\"description\",\"name\":\"Description\",\"value\":\"text\"}],\"pagination\":{\"has_more\":true}}"))
         (item (plist-get (feishu-project-mcp--detail-result payload) :item)))
    (should (equal "Name" (alist-get 'name item)))
    (should (equal "simple" (alist-get 'simple_name item)))
    (should (alist-get 'detail-truncated item))
    (should (equal "text"
                   (alist-get 'field_value (car (alist-get 'fields item)))))))

(ert-deftest feishu-project-test-stale-list-callback-is-ignored ()
  (with-temp-buffer
    (feishu-project-list-mode)
    (setq feishu-project--generation 2
          feishu-project--loading t
          feishu-project--items nil)
    (feishu-project--finish-list
     (list :items (list feishu-project-test--item))
     (list :buffer (current-buffer) :generation 1)
     nil nil)
    (should feishu-project--loading)
    (should-not feishu-project--items)))

(ert-deftest feishu-project-test-pagination-commits-only-on-success ()
  (let ((feishu-project-backend 'test-pagination)
        (calls nil))
    (feishu-project-register-backend
     'test-pagination
     (list :list
           (lambda (_project _types _name token _context success _failure)
             (push token calls)
             (funcall success
                      (list :items (list `((id . ,(or token 1))))
                            :continuation (pcase token
                                            ('nil 'page-2)
                                            ('page-2 'page-3)))))))
    (unwind-protect
        (with-temp-buffer
          (feishu-project-list-mode)
          (setq feishu-project--project-key "example"
                feishu-project--type-spec '("bug")
                feishu-project--query-kind 'list
                feishu-project--query nil)
          (feishu-project--request-page nil nil)
          (should-not feishu-project--current-page-token)
          (should (eq feishu-project--next-page-token 'page-2))
          (feishu-project-next-page)
          (should (eq feishu-project--current-page-token 'page-2))
          (should (equal feishu-project--page-history '(nil)))
          (feishu-project-previous-page)
          (should-not feishu-project--current-page-token)
          (should-not feishu-project--page-history)
          (should (equal (nreverse calls) '(nil page-2 nil))))
      (remhash 'test-pagination feishu-project--backends))))

(ert-deftest feishu-project-test-pagination-failure-keeps-committed-page ()
  (let ((feishu-project-backend 'test-pagination-failure))
    (feishu-project-register-backend
     'test-pagination-failure
     (list :list
           (lambda (_project _types _name token _context success failure)
             (if token
                 (funcall failure "network failure")
               (funcall success
                        (list :items (list feishu-project-test--item)
                              :continuation 'page-2))))))
    (unwind-protect
        (with-temp-buffer
          (feishu-project-list-mode)
          (setq feishu-project--project-key "example"
                feishu-project--query-kind 'list
                feishu-project--query nil)
          (feishu-project--request-page nil nil)
          (feishu-project-next-page)
          (should-not feishu-project--loading)
          (should-not feishu-project--current-page-token)
          (should (eq feishu-project--next-page-token 'page-2))
          (should-not feishu-project--page-history)
          (should (string-search "Fix login" (buffer-string))))
      (remhash 'test-pagination-failure feishu-project--backends))))

(ert-deftest feishu-project-test-detail-callback-updates-source-buffer ()
  (let ((feishu-project-backend 'test-detail)
        callback
        source-buffer
        detail-buffer)
    (feishu-project-register-backend
     'test-detail
     (list :detail
           (lambda (_item _context success _failure)
             (setq callback success))))
    (unwind-protect
        (with-temp-buffer
          (feishu-project-list-mode)
          (setq source-buffer (current-buffer)
                feishu-project--items (list feishu-project-test--item)
                feishu-project--generation 0
                feishu-project--loading nil
                tabulated-list-entries
                (mapcar #'feishu-project--entry feishu-project--items))
          (tabulated-list-print t)
          (cl-letf (((symbol-function 'tabulated-list-get-id)
                     (lambda () feishu-project-test--item))
                    ((symbol-function 'pop-to-buffer)
                     (lambda (buffer &rest _)
                       (setq detail-buffer buffer)
                       buffer)))
            (feishu-project-show))
          (should-not feishu-project--loading)
          (with-temp-buffer
            (funcall callback (list :item feishu-project-test--item)))
          (should-not feishu-project--loading)
          (should (buffer-live-p detail-buffer))
          (with-current-buffer detail-buffer
            (should (string-search "Fix login" (buffer-string))))))
      (when (buffer-live-p detail-buffer)
        (kill-buffer detail-buffer))
      (remhash 'test-detail feishu-project--backends)))

(ert-deftest feishu-project-test-mcp-init-queues-until-connected ()
  (let ((feishu-project-mcp--connection nil)
        (feishu-project-mcp--connecting nil)
        (feishu-project-mcp--pending nil)
        (mcp-server-connections (make-hash-table :test #'equal))
        initial-callback
        connection-arguments
        sent)
    (cl-letf (((symbol-function 'feishu-project-mcp--require-runtime)
               #'ignore)
              ((symbol-function 'jsonrpc-running-p)
               (lambda (_connection) t))
              ((symbol-function 'mcp--status)
               (lambda (_connection) 'init))
              ((symbol-function 'mcp-connect-server)
               (lambda (name &rest arguments)
                 (let ((connection 'initializing))
                   (puthash name connection mcp-server-connections)
                   (setq connection-arguments arguments
                         initial-callback
                         (plist-get arguments :initial-callback)))))
              ((symbol-function 'mcp-async-call-tool)
               (lambda (_connection tool _arguments _success _failure)
                 (push tool sent))))
      (feishu-project-mcp--call "first" nil #'ignore #'ert-fail)
      (feishu-project-mcp--call "second" nil #'ignore #'ert-fail)
      (should feishu-project-mcp--connecting)
      (should (= 2 (length feishu-project-mcp--pending)))
      (should (equal (plist-get connection-arguments :url)
                     feishu-project-mcp-url))
      (should (eq (plist-get connection-arguments :transport) 'streamable))
      (should (equal (plist-get connection-arguments :oauth)
                     feishu-project-mcp-oauth))
      (should-not (plist-member connection-arguments :command))
      (should-not sent)
      (funcall initial-callback 'connected)
      (should-not feishu-project-mcp--connecting)
      (should-not feishu-project-mcp--pending)
      (should (equal sent '("second" "first"))))))

(ert-deftest feishu-project-test-killed-detail-does-not-affect-list-loading ()
  (with-temp-buffer
    (feishu-project-list-mode)
    (setq feishu-project--generation 1 feishu-project--loading t)
    (let ((context (list :buffer (current-buffer) :generation 1))
          (detail-buffer (generate-new-buffer " *feishu-killed-detail*")))
      (kill-buffer detail-buffer)
      (feishu-project--finish-detail
       (list :item feishu-project-test--item) context detail-buffer 1 nil)
      (should feishu-project--loading)
      (feishu-project--fail-detail "failed" context detail-buffer 1 nil)
      (should feishu-project--loading))))

(ert-deftest feishu-project-test-list-buffer-pins-backend ()
  (let ((feishu-project-backend 'openapi))
    (with-temp-buffer
      (feishu-project-list-mode)
      (setq feishu-project--backend-name 'openapi
            feishu-project-backend 'mcp)
      (should (eq (plist-get (feishu-project--backend) :list)
                  #'feishu-project-openapi--list)))))

(ert-deftest feishu-project-test-template-options-and-required-meta ()
  (let* ((option '((option_id . "tpl") (option_name . "Default")))
         (record `((field_key . "template") (option . ,option)))
         (schema `((list . (,record))))
         (meta '((FieldConfList . (((field_key . "template"))
                                   ((field_key . "name"))
                                   ((field_key . "priority") (is_required . t)))))))
    (should (feishu-project-workbench--template-valid-p schema "tpl"))
    (setcdr (assq 'option record) '(((option_id . "tpl") (disabled . t))))
    (should-not (feishu-project-workbench--template-valid-p schema "tpl"))
    (should (equal (mapcar (lambda (x) (alist-get 'field_key x))
                           (feishu-project-workbench--meta-records meta))
                   '("template" "name" "priority")))))

(ert-deftest feishu-project-test-create-field-meta-contract-and-missing-required ()
  (let* ((feishu-project-backend 'test-create)
         (calls nil)
         (template `((field_key . "template")
                     (option . ((option_id . "tpl") (option_name . "Default")))) )
         (name '((field_key . "name")))
         (schema `((list . (,template ,name))))
         (meta '((FieldConfList . (((field_key . "priority") (is_required . t)))))))
    (feishu-project-register-backend
     'test-create
     (list :action (lambda (action payload success _failure)
                     (push (list action payload) calls)
                     (pcase action
                       ('field-schema (funcall success (list :data schema)))
                       ('field-meta (funcall success (list :data meta)))
                       (_ (funcall success (list :data nil)))))))
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
      (should-error (feishu-project-create-item "p" "bug" "tpl" "New" "[]") :type 'user-error))
    (let ((ordered (nreverse calls)))
      (should (equal (mapcar #'car ordered) '(field-schema field-meta)))
      (should (equal (cadar (cdr ordered)) (list :project_key "p" :work_item_type "bug")))
      (should-not (plist-member (cadar (cdr ordered)) :field_key)))
    (remhash 'test-create feishu-project--backends)))

(ert-deftest feishu-project-test-attachment-contract-and-transfer-headers ()
  (let ((feishu-project-backend 'test-attachment) action payload transfer)
    (feishu-project-register-backend
     'test-attachment
     (list :action (lambda (requested-action value success _failure)
                     (setq action requested-action payload value)
                     (funcall success (list :data '((url . "https://x/:part_number")
                                                    (sign . "secret") (is_multipart . nil)))))))
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
              ((symbol-function 'file-attributes) (lambda (_) '(nil nil nil nil nil nil nil 3)))
              ((symbol-function 'insert-file-contents-literally) (lambda (&rest _) (insert "abc")))
              ((symbol-function 'feishu-project-workbench--transfer)
               (lambda (url sign method mime &optional body _destination)
                 (setq transfer (list url sign method mime body)))))
      (with-temp-buffer
        (feishu-project-detail-mode)
        (setq feishu-project--backend-name 'test-attachment
              feishu-project--items (list feishu-project-test--item))
        (feishu-project-upload-attachment "/tmp/a" "text/plain" "attach")))
    (should (eq action 'upload-metadata))
    (should (equal (plist-get payload :project_key) "example"))
    (should (equal (plist-get payload :work_item_id) "7107052352"))
    (should (equal (plist-get payload :resource_type) 15))
    (should (equal (plist-get payload :field_key) "attach"))
    (should (equal (plist-get payload :file_name) "a"))
    (should (equal (plist-get payload :mime_type) "text/plain"))
    (should (= (plist-get payload :size) 3))
    (should (equal (nth 0 transfer) "https://x/0"))
    (should (equal (nth 2 transfer) "POST"))
    (should (equal (nth 3 transfer) "text/plain"))
    (remhash 'test-attachment feishu-project--backends)))

(ert-deftest feishu-project-test-upload-omits-empty-optional-field-key ()
  (let ((feishu-project-backend 'test-empty-attachment) payload)
    (feishu-project-register-backend
     'test-empty-attachment
     (list :action (lambda (_action value success _failure)
                     (setq payload value)
                     (funcall success (list :data '((url . "https://x/:part_number")
                                                    (sign . "secret") (is_multipart . nil)))))))
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
              ((symbol-function 'file-attributes) (lambda (_) '(nil nil nil nil nil nil nil 0)))
              ((symbol-function 'insert-file-contents-literally) (lambda (&rest _)))
              ((symbol-function 'feishu-project-workbench--transfer) (lambda (&rest _))))
      (with-temp-buffer
        (feishu-project-detail-mode)
        (setq feishu-project--backend-name 'test-empty-attachment
              feishu-project--items (list feishu-project-test--item))
        (feishu-project-upload-attachment "/tmp/a" "text/plain" "")))
    (should-not (plist-member payload :field_key))
    (remhash 'test-empty-attachment feishu-project--backends)))

(ert-deftest feishu-project-test-download-payload-uses-file-url ()
  (let ((feishu-project-backend 'test-download) action payload transfer)
    (feishu-project-register-backend
     'test-download (list :action (lambda (requested-action value success _failure)
                                    (setq action requested-action payload value)
                                    (funcall success (list :data '((url . "https://x/:part_number") (sign . "s") (is_multipart . nil)))))))
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
              ((symbol-function 'feishu-project-workbench--transfer)
               (lambda (&rest args) (setq transfer args))))
      (with-temp-buffer
        (feishu-project-detail-mode)
        (setq feishu-project--backend-name 'test-download feishu-project--items (list feishu-project-test--item))
        (feishu-project-download-attachment "file://token" "/tmp/out")))
    (should (eq action 'download-metadata))
    (should (equal (plist-get payload :project_key) "example"))
    (should (equal (plist-get payload :work_item_id) "7107052352"))
    (should (equal (plist-get payload :file_url) "file://token"))
    (should-not (plist-member payload :file_id))
    (should (equal (car transfer) "https://x/0"))
    (should (equal (nth 2 transfer) "GET"))
    (should-not (nth 3 transfer))
    (remhash 'test-download feishu-project--backends)))
(ert-deftest feishu-project-test-transfer-sends-sign-only-as-header ()
  (let (headers)
    (cl-letf (((symbol-function 'url-retrieve-synchronously)
               (lambda (&rest _)
                 (setq headers url-request-extra-headers)
                 (let ((buffer (generate-new-buffer " *feishu-transfer*")))
                   (with-current-buffer buffer (setq-local url-http-response-status 200))
                   buffer))))
      (feishu-project-workbench--transfer "https://example.invalid/0" "secret" "POST" "text/plain" "body"))
    (should (equal (cdr (assoc "X-Meego-File-Sign" headers)) "secret"))
    (should (equal (cdr (assoc "Content-Type" headers)) "text/plain"))))

(ert-deftest feishu-project-test-multipart-metadata-is-rejected ()
  (let ((feishu-project-backend 'test-multipart) transferred)
    (feishu-project-register-backend
     'test-multipart
     (list :action (lambda (_action _value success _failure)
                     (funcall success (list :data '((url . "https://x/:part_number")
                                                    (sign . "secret") (is_multipart . t)))))))
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
              ((symbol-function 'feishu-project-workbench--transfer) (lambda (&rest _) (setq transferred t))))
      (with-temp-buffer
        (feishu-project-detail-mode)
        (setq feishu-project--backend-name 'test-multipart feishu-project--items (list feishu-project-test--item))
        (should-error (feishu-project-download-attachment "file://token" "/tmp/out") :type 'user-error)))
    (should-not transferred)
    (remhash 'test-multipart feishu-project--backends)))

(ert-deftest feishu-project-test-p1-stale-callbacks-and-autoloads ()
  (let ((feishu-project-backend 'test-stale) detail-callbacks section-callback find-callbacks)
    (feishu-project-register-backend
     'test-stale
     (list :detail (lambda (_item _context success _failure) (push success detail-callbacks))
           :action (lambda (action _payload success _failure)
                     (pcase action ('comments (setq section-callback success)) ('find (push success find-callbacks))))))
    (with-temp-buffer
      (feishu-project-detail-mode)
      (setq feishu-project--backend-name 'test-stale feishu-project--items (list feishu-project-test--item)
            feishu-project--detail-identity (feishu-project--row-identity feishu-project-test--item 'test-stale))
      (feishu-project-detail-refresh) (feishu-project-detail-refresh)
      (funcall (car detail-callbacks) (list :item (append (assq-delete-all 'name (copy-tree feishu-project-test--item)) '((name . "New")))))
      (funcall (cadr detail-callbacks) (list :item (append (assq-delete-all 'name (copy-tree feishu-project-test--item)) '((name . "Old")))))
      (should (string-search "New" (buffer-string)))
      (feishu-project-show-comments)
      (setq feishu-project--detail-generation (1+ feishu-project--detail-generation))
      (funcall section-callback (list :data '((list . ((content . "stale"))))))
      (should-not (alist-get "Comments" feishu-project--detail-sections nil nil #'equal)))
    (cl-letf (((symbol-function 'feishu-project--read-project-key) (lambda () "p"))
              ((symbol-function 'pop-to-buffer) (lambda (&rest _) nil)))
      (let ((feishu-project-backend 'test-stale)) (feishu-project-find "1") (feishu-project-find "2")))
    (let ((buffer (get-buffer "*Feishu Project Find*")))
      (funcall (car find-callbacks) (list :item (append (assq-delete-all 'name (copy-tree feishu-project-test--item)) '((name . "Newest")))))
      (funcall (cadr find-callbacks) (list :item (append (assq-delete-all 'name (copy-tree feishu-project-test--item)) '((name . "Oldest")))))
      (with-current-buffer buffer (should (string-search "Newest" (buffer-string))) (should-not (string-search "Oldest" (buffer-string))))
      (kill-buffer buffer))
    (remhash 'test-stale feishu-project--backends))
  (with-temp-buffer
    (insert-file-contents "feishu-project-workbench.el")
    (dolist (name '(feishu-project-find feishu-project-detail-refresh feishu-project-upload-attachment feishu-project-batch-edit-field feishu-project-dispatch))
      (should (string-match-p (format ";;;###autoload[[:space:][:nonascii:]]+(defun %s" (symbol-name name)) (buffer-string))))))

(ert-deftest feishu-project-test-filter-builds-validated-escaped-mql ()
  (let (calls result)
    (with-temp-buffer
      (feishu-project-list-mode)
      (setq feishu-project--generation 1)
      (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "bug"))
                ((symbol-function 'feishu-project-mcp--call)
                 (lambda (tool payload success _failure)
                   (push (list tool payload) calls)
                   (pcase tool
                     ("list_workitem_field_config"
                      (funcall success '((list . (((field_key . "work_item_status"))
                                                   ((field_key . "assignee")))))))
                     ("search_by_mql" (setq result payload))))))
        (feishu-project-mcp--select-type
         (list (list :key "bug" :name "Bug")) "p" nil
         (list :buffer (current-buffer) :generation 1 :filters (list :type "bug" :status "O'Reilly" :assignee "u"))
         #'ignore #'ert-fail)))
    (should (equal (mapcar #'car (nreverse calls)) '("list_workitem_field_config" "search_by_mql")))
    (should (string-search "`work_item_status` = 'O\\\\'Reilly'" (plist-get result :mql)))
    (should (string-match-p "`assignee` = 'u'" (plist-get result :mql)))))

(ert-deftest feishu-project-test-filter-rejects-unconfigured-field ()
  (with-temp-buffer
    (feishu-project-list-mode)
    (setq feishu-project--generation 1)
    (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "bug"))
              ((symbol-function 'feishu-project-mcp--call)
               (lambda (tool _payload success failure)
                 (if (equal tool "list_workitem_field_config")
                     (funcall success '((list . (((field_key . "work_item_status"))))) )
                   (funcall failure "MQL must not run")))))
      (let (failure)
        (feishu-project-mcp--select-type (list (list :key "bug" :name "Bug")) "p" nil
                                         (list :buffer (current-buffer) :generation 1 :filters (list :creator "u"))
                                         #'ignore (lambda (error) (setq failure error)))
        (should (string-match-p "creator" failure))))))

(ert-deftest feishu-project-test-openapi-filter-is-capability-error ()
  (let ((feishu-project-backend 'openapi))
    (with-temp-buffer
      (feishu-project-list-mode)
      (setq feishu-project--backend-name 'openapi feishu-project--query-kind 'list)
      (should-error (feishu-project-filter "bug" "" "" "") :type 'user-error))))
(ert-deftest feishu-project-test-transition-sequence-and-required-rejection ()
  (let ((feishu-project-backend 'test-transition) calls)
    (feishu-project-register-backend
     'test-transition
     (list :action (lambda (action payload success _failure)
                     (push (list action payload) calls)
                     (pcase action
                       ('transition-states (funcall success (list :data '((list . (((transition_id . "go"))))))))
                       ('transition-required (funcall success (list :data '((list . nil)))) )
                       ('transition (funcall success nil))))))
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
      (with-temp-buffer
        (feishu-project-detail-mode)
        (setq feishu-project--backend-name 'test-transition feishu-project--items (list feishu-project-test--item))
        (feishu-project-transition-state "go")))
    (let ((ordered (nreverse calls)))
      (should (equal (mapcar #'car ordered) '(transition-states transition-required transition)))
      (should (equal (plist-get (cadar (cdr ordered)) :transition_id) "go")))
    (remhash 'test-transition feishu-project--backends))
  (let ((feishu-project-backend 'test-required) calls)
    (feishu-project-register-backend
     'test-required (list :action (lambda (action _p success _f)
                                    (push action calls)
                                    (pcase action
                                      ('transition-states (funcall success (list :data '((list . (((transition_id . "go"))))))))
                                      ('transition-required (funcall success (list :data '((list . (((field_key . "reason"))))))))
                                      (_ (ert-fail "transition must not run"))))))
    (with-temp-buffer
      (feishu-project-detail-mode)
      (setq feishu-project--backend-name 'test-required feishu-project--items (list feishu-project-test--item))
      (should-error (feishu-project-transition-state "go") :type 'user-error))
    (should (equal (nreverse calls) '(transition-states transition-required)))
    (remhash 'test-required feishu-project--backends)))

(ert-deftest feishu-project-test-comment-create-update-payloads ()
  (let ((feishu-project-backend 'test-comment) payloads)
    (feishu-project-register-backend 'test-comment
      (list :action (lambda (_action payload success _failure) (push payload payloads) (funcall success nil))))
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
      (with-temp-buffer
        (feishu-project-detail-mode)
        (setq feishu-project--backend-name 'test-comment feishu-project--items (list feishu-project-test--item))
        (feishu-project-add-comment "new") (feishu-project-update-comment "c1" "changed")))
    (setq payloads (nreverse payloads))
    (should (equal (mapcar (lambda (p) (plist-get p :action)) payloads) '("create" "update")))
    (should (equal (plist-get (cadr payloads) :comment_id) "c1"))
    (remhash 'test-comment feishu-project--backends)))

(ert-deftest feishu-project-test-batch-schema-gate-and-partial-results ()
  (let ((feishu-project-backend 'test-batch) calls messages)
    (feishu-project-register-backend
     'test-batch (list :action (lambda (action payload success failure)
                                 (push action calls)
                                 (pcase action
                                   ('field-schema (funcall success (list :data '((list . (((field_key . "priority"))))))))
                                   ('update-field (if (equal (plist-get payload :work_item_id) "2")
                                                      (funcall failure "no") (funcall success nil)))))))
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
              ((symbol-function 'message) (lambda (f &rest a) (push (apply #'format f a) messages))))
      (with-temp-buffer
        (feishu-project-list-mode)
        (setq feishu-project--backend-name 'test-batch
              feishu-project--items (list feishu-project-test--item '((id . 2) (name . "Two") (work_item_type_key . "bug") (simple_name . "example"))))
        (feishu-project-batch-edit-field "priority" "high")))
    (should (equal (nreverse calls) '(field-schema update-field update-field)))
    (should (string-match-p "Batch updated 1; failed 1: 2" (car messages)))
    (remhash 'test-batch feishu-project--backends)))

(ert-deftest feishu-project-test-batch-unavailable-schema-does-not-update ()
  (let ((feishu-project-backend 'test-batch-invalid) calls)
    (feishu-project-register-backend 'test-batch-invalid
      (list :action (lambda (action _payload success _failure)
                      (push action calls)
                      (when (eq action 'field-schema) (funcall success (list :data '((list . nil))))))))
    (with-temp-buffer
      (feishu-project-list-mode)
      (setq feishu-project--backend-name 'test-batch-invalid feishu-project--items (list feishu-project-test--item))
      (should-error (feishu-project-batch-edit-field "priority" "high") :type 'user-error))
    (should (equal calls '(field-schema)))
    (remhash 'test-batch-invalid feishu-project--backends)))




(ert-deftest feishu-project-test-columns-marks-export-and-detail-truncation ()
  (with-temp-buffer
    (feishu-project-list-mode)
    (setq feishu-project--items (list feishu-project-test--item))
    (feishu-project-toggle-mark feishu-project-test--item)
    (should (equal (feishu-project-marked-items) (list feishu-project-test--item)))
    (feishu-project-columns '("Name" "ID"))
    (should (= (length (feishu-project--columns)) 2))
    (should (string-match-p "Fix login"
                            (feishu-project-export-content 'csv (feishu-project-marked-items))))
    (feishu-project-reset-columns)
    (should (= (length (feishu-project--columns))
               (length feishu-project-list-columns))))
  (with-temp-buffer
    (feishu-project-detail-mode)
    (feishu-project--render-detail
     (append feishu-project-test--item '((detail-truncated . t))))
    (should (string-match-p "detail fields are truncated" (buffer-string)))))

(ert-deftest feishu-project-test-project-switch-and-lazy-transient-fallback ()
  (let ((feishu-project-backend 'test-project) displayed payloads)
    (feishu-project-register-backend
     'test-project
     (list :action (lambda (_action payload success _failure)
                     (push payload payloads)
                     (funcall success (list :data '((project_key . "resolved")))))))
    (cl-letf (((symbol-function 'feishu-project--display)
               (lambda (&rest args) (setq displayed args))))
      (with-temp-buffer
        (feishu-project-list-mode)
        (setq feishu-project--backend-name 'test-project
              feishu-project--query-kind 'list
              feishu-project--project-key "old")
        (let ((feishu-project-project-aliases '(("alias" . "raw"))))
          (feishu-project-switch-project "alias")
          (feishu-project-validate-project "https://project.feishu.cn/space"))))
    (setq payloads (nreverse payloads))
    (should (equal (plist-get (car payloads) :project_key) "raw"))
    (should (equal (plist-get (cadr payloads) :url) "https://project.feishu.cn/space"))
    (remhash 'test-project feishu-project--backends))
  (cl-letf (((symbol-function 'require) (lambda (&rest _) nil)))
    (should-error (feishu-project-dispatch) :type 'user-error)
    (should-error (feishu-project-detail-dispatch) :type 'user-error))
  (require 'transient)
  (should (fboundp 'feishu-project--list-menu))
  (should (fboundp 'feishu-project--detail-menu)))

(ert-deftest feishu-project-test-transition-and-comment-cancel-without-mutation ()
  (let ((feishu-project-backend 'test-cancel) calls)
    (feishu-project-register-backend
     'test-cancel
     (list :action (lambda (action _payload success _failure)
                     (push action calls)
                     (pcase action
                       ('transition-states (funcall success (list :data '((list . (((transition_id . "go"))))))))
                       ('transition-required (funcall success (list :data '((list . nil)))) )
                       (_ (funcall success nil))))))
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) nil)))
      (with-temp-buffer
        (feishu-project-detail-mode)
        (setq feishu-project--backend-name 'test-cancel feishu-project--items (list feishu-project-test--item))
        (should-error (feishu-project-transition-state "go") :type 'user-error)
        (should-error (feishu-project-add-comment "cancel") :type 'user-error)))
    (should (equal (nreverse calls) '(transition-states transition-required)))
    (remhash 'test-cancel feishu-project--backends)))

(ert-deftest feishu-project-test-mcp-json-false-is-nil-across-workbench-guards ()
  (let* ((payload
          (feishu-project-mcp--json
           "{\"option\":[{\"option_id\":\"tpl\",\"disabled\":false}],\"FieldConfList\":[{\"field_key\":\"optional\",\"is_required\":false}],\"required\":[{\"field_key\":\"gate\",\"optional\":false}],\"is_multipart\":false,\"pagination\":{\"has_more\":false}}"))
         (schema `((list . (((field_key . "template")
                             (option . ,(alist-get 'option payload)))))))
         (detail-payload
          `((work_item_attribute
             . ((owned_project . ((key . "p") (simple_name . "p")))
                (work_item_id . "1") (work_item_name . "One")
                (work_item_type . ((key . "story") (name . "Story")))))
            (pagination . ,(alist-get 'pagination payload)))))
    (should (feishu-project-workbench--template-valid-p schema "tpl"))
    (should-not (plist-get (feishu-project-workbench--metadata payload) :multipart))
    (should-not (alist-get 'is_required
                           (car (feishu-project-workbench--meta-records payload))))
    (should-not (alist-get 'optional (car (alist-get 'required payload))))
    (should-not (alist-get 'detail-truncated
                           (plist-get (feishu-project-mcp--detail-result detail-payload) :item)))))

(ert-deftest feishu-project-test-cli-security-contracts-and-url-adaptation ()
  (let ((process-environment '("PATH=/bin" "COOKIE=bad" "PRIVATE_KEY=bad"
                               "DROP=bad" "LC_ALL=C"
                               "HTTPS_PROXY=https://u:p@bad"
                               "HTTP_PROXY=https://:secret@bad"))
        (feishu-project-cli-environment '("LANG=C")))
    (let ((environment (feishu-project-cli--safe-environment)))
      (should (member "PATH=/bin" environment))
      (should (member "LC_ALL=C" environment))
      (should-not (seq-some (lambda (entry) (string-prefix-p "COOKIE=" entry)) environment))
      (should-not (seq-some (lambda (entry) (string-prefix-p "DROP=" entry)) environment))
      (should-not (seq-some (lambda (entry) (string-suffix-p "_PROXY=" entry))
                            environment))
      (should-not (seq-some (lambda (entry)
                              (string-match-p "\\`HTTPS?_PROXY=" entry))
                            environment))))
  (dolist (name '("TOKEN=x" "PRIVATE_KEY=x" "COOKIE=x" "API_KEY=x"
                  "PROXY=https://:secret@bad"))
    (let ((feishu-project-cli-environment (list name)))
      (should-error (feishu-project-cli--safe-environment) :type 'user-error)))
  (dolist (text '("{}" "[]" "null")) (should-error (feishu-project-cli--envelope text)))
  (should (equal (alist-get 'data (feishu-project-cli--envelope "{\"data\":null}")) nil))
  (should (string-match-p "REDACTED" (feishu-project-cli--redact "token=abc https://x/y?sig=bad")))
  (let ((feishu-project-host "https://project.feishu.cn"))
    (should (equal (feishu-project-cli--project-url "https://project.feishu.cn/p/story/detail/7117416064" t)
                   '(:project_key "p" :work_item_id "7117416064")))
    (dolist (url '("http://project.feishu.cn/p" "https://evil.example/p"
                   "https://user:password@project.feishu.cn/p"
                   "https://:password@project.feishu.cn/p"
                   "https://@project.feishu.cn/p"
                   "https://project.feishu.cn/p/extra" "https://project.feishu.cn/p/story/detail/7/x"))
      (should-error (feishu-project-cli--project-url url (string-match-p "detail" url)) :type 'user-error))))

(ert-deftest feishu-project-test-cli-contract-filter-pagination-and-attachments ()
  (let ((payload (feishu-project-cli--action-payload 'transition-required
                                                      '(:project_key "p" :work_item_id "7" :state_key "done" :transition_id "bad" :drop "x"))))
    (should (equal "p" (plist-get payload :project_key)))
    (should (equal "7" (plist-get payload :work_item_id)))
    (should (equal "done" (plist-get payload :state_key)))
    (should-not (plist-member payload :transition_id))
    (should-not (plist-member payload :drop)))
  (should-error (feishu-project-cli--action-payload 'transition-required '(:transition_id "bad")) :type 'user-error)
  (let* ((payload (feishu-project-cli--json "{\"list\":[{\"group_infos\":[{\"group_id\":\"g\"}],\"count\":100}],\"session_id\":\"s\",\"data\":{\"g\":[{\"moql_field_list\":[{\"key\":\"work_item_id\",\"value_type\":\"long_value\",\"value\":{\"long_value\":7117416064}}]}]}}"))
         (result (feishu-project-cli--mql-result payload "p" '(:key "story" :name "Story") "SELECT" 4)))
    (should (= 7117416064 (alist-get 'work_item_id (car (plist-get result :items)))))
    (should (= 5 (plist-get (plist-get result :continuation) :fetched))))
  (let (argv)
    (cl-letf (((symbol-function 'feishu-project-cli--ensure-auth) (lambda (success _failure) (funcall success)))
              ((symbol-function 'feishu-project-cli--process) (lambda (command success _failure) (setq argv command) (funcall success "{\"data\":null}"))))
      (feishu-project-cli--file-action 'upload-file "/tmp/source" '(:project_key "p" :work_item_id "7" :mime_type "text/plain" :field_key "f") #'ignore #'ert-fail)
      (should (equal (seq-take argv 4) '("meegle" "attachment" "+upload" "/tmp/source")))
      (should-not (member "--params" argv)))))

(ert-deftest feishu-project-test-cli-process-races-and-start-cleanup ()
  (let (sentinel timeout status stdout stderr failures)
    (cl-letf (((symbol-function 'executable-find) (lambda (_) "/bin/meegle"))
              ((symbol-function 'make-process)
               (lambda (&rest args)
                 (setq sentinel (plist-get args :sentinel)
                       stdout (plist-get args :buffer)
                       stderr (plist-get args :stderr))
                 'fake))
              ((symbol-function 'run-at-time)
               (lambda (_seconds _repeat callback) (setq timeout callback) 'timer))
              ((symbol-function 'cancel-timer) #'ignore)
              ((symbol-function 'process-live-p) (lambda (_) (eq status 'run)))
              ((symbol-function 'process-status) (lambda (_) status))
              ((symbol-function 'process-exit-status) (lambda (_) 1))
              ((symbol-function 'kill-process)
               (lambda (proc) (setq status 'signal) (funcall sentinel proc "killed"))))
      (setq status 'run)
      (feishu-project-cli--process '("meegle" "auth" "status") #'ert-fail
                                   (lambda (error) (push error failures)))
      (funcall timeout)
      (funcall sentinel 'fake "again")
      (should (equal failures '("Meegle CLI request timed out")))
      (should-not (buffer-live-p stdout))
      (should-not (buffer-live-p stderr))))
  (let (stdout stderr failure)
    (cl-letf (((symbol-function 'executable-find) (lambda (_) "/bin/meegle"))
              ((symbol-function 'generate-new-buffer)
               (lambda (name)
                 (let ((buffer (get-buffer-create (generate-new-buffer-name name))))
                   (if stdout (setq stderr buffer) (setq stdout buffer)) buffer)))
              ((symbol-function 'make-process) (lambda (&rest _) (error "boom"))))
      (feishu-project-cli--process '("meegle") #'ert-fail
                                   (lambda (error) (setq failure error)))
      (should failure)
      (should-not (buffer-live-p stdout))
      (should-not (buffer-live-p stderr)))))

(ert-deftest feishu-project-test-cli-auth-cache-and-deferred-file-environment ()
  (let ((feishu-project-cli--authenticated (make-hash-table :test #'equal)) calls)
    (cl-letf (((symbol-function 'feishu-project-cli--process)
               (lambda (argv success _failure)
                 (push argv calls) (funcall success "{\"authenticated\":true}"))))
      (let ((feishu-project-cli-profile nil))
        (feishu-project-cli--ensure-auth #'ignore #'ert-fail)
        (feishu-project-cli--ensure-auth #'ignore #'ert-fail))
      (let ((feishu-project-cli-profile "other"))
        (feishu-project-cli--ensure-auth #'ignore #'ert-fail))
      (should (= 2 (length calls)))))
  (let ((process-environment '("PATH=/bin" "COOKIE=drop"))
        (feishu-project-cli-environment '("LANG=C"))
        deferred seen)
    (cl-letf (((symbol-function 'feishu-project-cli--ensure-auth)
               (lambda (success _failure) (setq deferred success)))
              ((symbol-function 'feishu-project-cli--process)
               (lambda (_argv success _failure)
                 (setq seen process-environment)
                 (funcall success "{\"data\":null}"))))
      (feishu-project-cli--file-action
       'upload-file "/tmp/source"
       '(:project_key "p" :work_item_id "7" :mime_type "text/plain")
       #'ignore #'ert-fail)
      (should-not seen)
      (funcall deferred)
      (should (member "MEEGLE_NO_UPDATE_CHECK=1" seen))
      (should (member "PATH=/bin" seen))
      (should-not (seq-some (lambda (entry) (string-prefix-p "COOKIE=" entry)) seen)))))

(ert-deftest feishu-project-test-cli-list-selects-type-and-validates-filters ()
  (let ((types (feishu-project-cli--json
                "{\"list\":[{\"type_key\":\"story\",\"name\":\"Story\"}]}"))
        (fields (feishu-project-cli--json
                 "{\"list\":[{\"field_key\":\"work_item_status\"}]}"))
        (page (feishu-project-cli--json
               "{\"list\":[{\"group_infos\":[{\"group_id\":\"g\"}],\"count\":0}],\"data\":{\"g\":[]}}"))
        calls result)
    (with-temp-buffer
      (feishu-project-list-mode)
      (setq feishu-project--generation 1
            feishu-project--filters '(:type "story" :status "doing"))
      (let ((context (list :buffer (current-buffer) :generation 1
                           :filters feishu-project--filters)))
        (cl-letf (((symbol-function 'feishu-project-cli--call)
                   (lambda (domain method payload success _failure)
                     (push (list domain method payload) calls)
                     (funcall success
                              (pcase method
                                ("meta-types" types)
                                ("meta-fields" fields)
                                ("query" page))))))
          (feishu-project-cli--list
           "p" nil nil nil context
           (lambda (value) (setq result value)) #'ert-fail))))
    (should result)
    (setq calls (nreverse calls))
    (should (equal (mapcar #'cadr calls)
                   '("meta-types" "meta-fields" "query")))
    (should (string-match-p "`work_item_status` = 'doing'"
                            (plist-get (nth 2 (nth 2 calls)) :mql)))))

(ert-deftest feishu-project-test-cli-mql-replay-and-pagination-state ()
  (let ((response (feishu-project-cli--json
                   "{\"list\":[{\"group_infos\":[{\"group_id\":\"g\"}],\"count\":5}],\"session_id\":\"s\",\"data\":{\"g\":[]}}"))
        payload result)
    (cl-letf (((symbol-function 'feishu-project-cli--call)
               (lambda (_domain _method params success _failure)
                 (setq payload params)
                 (funcall success response))))
      (feishu-project-cli--mql
       "p" nil '(:kind replay :mql "SELECT" :type (:key "story" :name "Story"))
       nil (lambda (value) (setq result value)) #'ert-fail))
    (should (equal "SELECT" (plist-get payload :mql)))
    (should (equal "SELECT"
                   (plist-get (plist-get result :current-page-token) :mql))))
  (let ((response (feishu-project-cli--json
                   "{\"list\":[{\"group_infos\":[{\"group_id\":\"g\"}],\"count\":5}],\"session_id\":\"s\",\"data\":{\"g\":[{\"moql_field_list\":[]}]}}"))
        result)
    (cl-letf (((symbol-function 'feishu-project-cli--call)
               (lambda (_domain _method _params success _failure)
                 (funcall success response))))
      (feishu-project-cli--mql
       "p" nil '(:kind session :session-id "s" :group-id "g"
                       :page-num 2 :fetched 2 :type (:key "story" :name "Story"))
       nil (lambda (value) (setq result value)) #'ert-fail))
    (let ((next (plist-get result :continuation)))
      (should (= 3 (plist-get next :fetched)))
      (should (= 3 (plist-get next :page-num))))))

(ert-deftest feishu-project-test-cli-url-find-preserves-allowed-options ()
  (let (payload)
    (cl-letf (((symbol-function 'feishu-project-cli--call)
               (lambda (_domain _method params _success _failure)
                 (setq payload params))))
      (let ((feishu-project-host "https://project.feishu.cn"))
        (feishu-project-cli--action
         'find '(:url "https://project.feishu.cn/p/story/detail/7"
                      :fields ["_all"] :page_size 100 :drop "x")
         #'ignore #'ert-fail)))
    (should (equal "p" (plist-get payload :project_key)))
    (should (equal "7" (plist-get payload :work_item_id)))
    (should (equal ["_all"] (plist-get payload :fields)))
    (should (= 100 (plist-get payload :page_size)))
    (should-not (plist-member payload :drop))))

(ert-deftest feishu-project-test-cli-detail-normalizes-common-fields ()
  (let* ((payload
          (feishu-project-cli--json
           "{\"work_item_attribute\":{\"owned_project\":{\"key\":\"p\",\"simple_name\":\"simple\"},\"work_item_id\":\"7\",\"work_item_name\":\"Name\",\"work_item_type\":{\"key\":\"story\",\"name\":\"Story\"},\"work_item_status\":{\"name\":\"OPEN\"},\"create_time\":\"created\",\"update_time\":\"updated\"},\"work_item_fields\":[],\"pagination\":{\"has_more\":false}}"))
         (item (plist-get (feishu-project-cli--detail-result payload) :item)))
    (should (equal "OPEN" (alist-get 'name (alist-get 'work_item_status item))))
    (should (equal "created" (alist-get 'created_at item)))
    (should (equal "updated" (alist-get 'updated_at item)))))

(provide 'feishu-project-test)
;;; feishu-project-test.el ends here
