;;; feishu-project-export.el --- Export Feishu Project items -*- lexical-binding: t; -*-

(require 'json)
(require 'feishu-project)
(declare-function feishu-project--columns "feishu-project")

(defun feishu-project-export--columns (&optional columns)
  "Return normalized COLUMNS."
  (or columns (feishu-project--columns)))

(defun feishu-project-export--title (column)
  "Return COLUMN title."
  (plist-get column :title))

(defun feishu-project-export--value (item column)
  "Return ITEM display value for COLUMN."
  (format "%s" (funcall (plist-get column :getter) item)))

(defun feishu-project-export--csv-field (value separator)
  "Escape VALUE for SEPARATOR-delimited output."
  (if (string-match-p (format "[%s\"\n\r]" (regexp-quote separator)) value)
      (concat "\"" (replace-regexp-in-string "\"" "\"\"" value t t) "\"") value))

(defun feishu-project-export-csv (items &optional columns separator)
  "Return ITEMS as CSV using COLUMNS and optional SEPARATOR."
  (let* ((separator (or separator ",")) (columns (feishu-project-export--columns columns))
         (line (lambda (values) (mapconcat (lambda (v) (feishu-project-export--csv-field v separator)) values separator))))
    (concat (funcall line (mapcar #'feishu-project-export--title columns)) "\n"
            (mapconcat (lambda (item) (funcall line (mapcar (lambda (c) (feishu-project-export--value item c)) columns))) items "\n")
            (if items "\n" ""))))

(defun feishu-project-export-org (items &optional columns)
  "Return ITEMS as an Org table."
  (let ((columns (feishu-project-export--columns columns)))
    (concat "| " (mapconcat #'feishu-project-export--title columns " | ") " |\n"
            "|-" (mapconcat (lambda (_c) "-") columns "-+-") "-|\n"
            (mapconcat (lambda (item) (concat "| " (mapconcat (lambda (c) (feishu-project-export--value item c)) columns " | ") " |")) items "\n")
            (if items "\n" ""))))

(defun feishu-project-export-markdown (items &optional columns)
  "Return ITEMS as a Markdown table."
  (let ((columns (feishu-project-export--columns columns)))
    (concat "| " (mapconcat #'feishu-project-export--title columns " | ") " |\n"
            "|" (mapconcat (lambda (_c) " --- ") columns "|") "|\n"
            (mapconcat (lambda (item) (concat "| " (mapconcat (lambda (c) (feishu-project-export--value item c)) columns " | ") " |")) items "\n")
            (if items "\n" ""))))

(defun feishu-project-export-content (format items &optional columns)
  "Return ITEMS encoded in FORMAT."
  (pcase format
    ('csv (feishu-project-export-csv items columns))
    ('tsv (feishu-project-export-csv items columns "\t"))
    ('org (feishu-project-export-org items columns))
    ('markdown (feishu-project-export-markdown items columns))
    ('json (json-serialize (vconcat items)))
    (_ (user-error "Unsupported export format: %S" format))))

(defun feishu-project-export-write (format items destination &optional columns)
  "Write ITEMS in FORMAT to DESTINATION."
  (when (and (file-exists-p destination) (not (yes-or-no-p (format "Overwrite %s? " destination))))
    (user-error "Export cancelled"))
  (with-temp-file destination (insert (feishu-project-export-content format items columns)))
  (message "Exported %d work items" (length items)))

(provide 'feishu-project-export)
;;; feishu-project-export.el ends here
