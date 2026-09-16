;;; feishu-project-test.el --- Tests for feishu-project -*- lexical-binding: t; -*-

(require 'ert)
(require 'feishu-project)

(defconst feishu-project-test--item
  '((id . 7107052352)
    (name . "Fix login")
    (work_item_type_key . "bug")
    (simple_name . "example")
    (updated_at . 1761475143215)
    (work_item_status . ((state_key . "doing")))))

(ert-deftest feishu-project-test-item-accessors ()
  (should (equal (feishu-project--item-id feishu-project-test--item)
                 "7107052352"))
  (should (equal (feishu-project--item-name feishu-project-test--item)
                 "Fix login"))
  (should (equal (feishu-project--item-status feishu-project-test--item)
                 "doing")))

(ert-deftest feishu-project-test-item-url ()
  (let ((feishu-project-host "https://project.feishu.cn/"))
    (should
     (equal (feishu-project-item-url feishu-project-test--item)
            "https://project.feishu.cn/example/bug/detail/7107052352"))))

(ert-deftest feishu-project-test-request ()
  (let ((feishu-project-access-token "token"))
    (cl-letf (((symbol-function 'url-retrieve-synchronously)
               (lambda (&rest _)
                 (let ((buffer (generate-new-buffer " *feishu-http-test*")))
                   (with-current-buffer buffer
                     (insert "HTTP/1.1 200 OK\nContent-Type: application/json\n\n"
                             "{\"err_code\":0,\"data\":[{\"id\":7}]}")
                     (setq-local url-http-response-status 200)
                     (setq-local url-http-end-of-headers
                                 (copy-marker
                                  (progn
                                    (goto-char (point-min))
                                    (search-forward "\n\n")
                                    (point)))))
                   buffer))))
      (let ((response (feishu-project--request "/test" '((ok . t)))))
        (should (= (alist-get 'id (car (alist-get 'data response))) 7))))))

(ert-deftest feishu-project-test-render-list-entry ()
  (let ((entry (feishu-project--entry feishu-project-test--item)))
    (should (eq (car entry) feishu-project-test--item))
    (should (member "Fix login" (append (cadr entry) nil)))))

(ert-deftest feishu-project-test-render-detail ()
  (with-temp-buffer
    (feishu-project-detail-mode)
    (feishu-project--render-detail feishu-project-test--item)
    (should (string-search "Fix login" (buffer-string)))
    (should (string-search "doing" (buffer-string)))))

(provide 'feishu-project-test)

;;; feishu-project-test.el ends here
