;;; feishu-project-test.el --- Tests for feishu-project -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'feishu-project)
(require 'feishu-project-openapi)

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
          (should feishu-project--loading)
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

(ert-deftest feishu-project-test-killed-detail-clears-source-loading ()
  (with-temp-buffer
    (feishu-project-list-mode)
    (setq feishu-project--generation 1
          feishu-project--loading t)
    (let ((context (list :buffer (current-buffer) :generation 1))
          (detail-buffer (generate-new-buffer " *feishu-killed-detail*")))
      (kill-buffer detail-buffer)
      (feishu-project--finish-detail
       (list :item feishu-project-test--item) context detail-buffer)
      (should-not feishu-project--loading)
      (setq feishu-project--loading t)
      (feishu-project--fail-detail "failed" context detail-buffer)
      (should-not feishu-project--loading))))

(ert-deftest feishu-project-test-list-buffer-pins-backend ()
  (let ((feishu-project-backend 'openapi))
    (with-temp-buffer
      (feishu-project-list-mode)
      (setq feishu-project--backend-name 'openapi
            feishu-project-backend 'mcp)
      (should (eq (plist-get (feishu-project--backend) :list)
                  #'feishu-project-openapi--list)))))

(provide 'feishu-project-test)

;;; feishu-project-test.el ends here
