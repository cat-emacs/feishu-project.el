# feishu-project.el

Browse [Feishu Project](https://project.feishu.cn/) work items without leaving
Emacs. The package provides tabulated work-item results, detail views, OpenAPI
filtering and an extension point for MQL backends.

## Installation

```elisp
(use-package feishu-project
  :vc (:url "https://github.com/cat-emacs/feishu-project.el")
  :commands (feishu-project-list feishu-project-mql)
  :custom
  (feishu-project-project-key "my-project")
  (feishu-project-work-item-type-keys '("story" "bug")))
```

## Authentication

Use a user access token by itself, or a plugin/virtual-plugin access token with
a user key:

```sh
export FEISHU_PROJECT_TOKEN='u-...'
# Required only for p-... or v-... tokens:
export FEISHU_PROJECT_USER_KEY='...'
export FEISHU_PROJECT_KEY='my-project'
```

Instead of `FEISHU_PROJECT_TOKEN`, put the token in `auth-source`:

```text
machine project.feishu.cn password u-...
```

## Commands

- `M-x feishu-project-list` filters work items by project, type and name.
- `M-x feishu-project-mql` runs a complete MQL query through a configured
  `feishu-project-mql-function`.

List keys: `RET` details, `o` browser, `w` copy ID, `W` copy URL, `g` refresh,
`n`/`p` next/previous page, and `q` quit. Detail keys: `o`, `W`, and `q`.

## MQL backend

Feishu exposes MQL execution through its MCP `search_by_mql` tool rather than
the documented OpenAPI. The backend function receives `(PROJECT-KEY MQL)` and
returns a list of work-item alists. Rows should include `work_item_id` (or
`id`), `name`, and `work_item_type_key`; `simple_name`,
`current_status_name`, and `updated_at` improve the display and browser links.

```elisp
(defun my-feishu-project-mql (project-key mql)
  ;; Call Feishu Project MCP search_by_mql and normalize its rows.
  ;; Return: '(((work_item_id . 123) (name . "Example")
  ;;            (work_item_type_key . "story") ...))
  )

(setq feishu-project-mql-function #'my-feishu-project-mql
      feishu-project-saved-mql
      '(("My open stories" .
         "SELECT `work_item_id`, `name`, `current_status_name`
          FROM `MySpace`.`需求`
          WHERE `创建人` = current_login_user()")))
```

MQL identifiers in `SELECT` and `WHERE` must use backticks. MQL pagination
belongs to the backend because MCP uses a `session_id` and per-group pagination
rather than OpenAPI page numbers.

## Development

```sh
make
```

Licensed under GPL-3.0-or-later.
