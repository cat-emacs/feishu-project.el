# feishu-project.el

Browse [Feishu Project](https://project.feishu.cn/) work items from Emacs.
`feishu-project.el` has two interchangeable read backends: the original
plugin-token OpenAPI backend and an optional user-OAuth MCP backend.  MCP also
provides the explicitly confirmed workbench write actions documented below. The
default is `openapi`, so upgrading does not start Node, open a browser, or
change existing credentials.

## Installation

```elisp
(use-package feishu-project
  :vc (:url "https://github.com/cat-emacs/feishu-project.el")
  :commands (feishu-project-list feishu-project-mql)
  :custom
  (feishu-project-project-key "my-project"))
```

The common UI requires Emacs 29.1.  `feishu-project.el` is the UI and backend
protocol; `feishu-project-openapi.el` and `feishu-project-mcp.el` are loaded
only when their backend is selected.

## OpenAPI backend (default)

OpenAPI requires a Feishu Project plugin or user token.  A plugin/virtual-plugin
token also requires the user key:

```sh
export FEISHU_PROJECT_TOKEN='u-...'
export FEISHU_PROJECT_USER_KEY='...'
export FEISHU_PROJECT_KEY='my-project'
```

Instead of `FEISHU_PROJECT_TOKEN`, `auth-source` can supply it:

```text
machine project.feishu.cn password u-...
```

`feishu-project-list` prompts for OpenAPI type keys (with
`feishu-project-work-item-type-keys` as its default).  Existing custom MQL
bridges remain supported through `feishu-project-mql-function`; they return a
list of normalized item alists and own their own pagination.

## MCP backend

The MCP backend uses the official `https://project.feishu.cn/mcp_server/v1`
server. It requires Emacs 30.1 and the temporary
[`cat-emacs/mcp.el`](https://github.com/cat-emacs/mcp.el) OAuth branch
`feat/oauth-client`:

```elisp
(use-package mcp
  :if EMACS30+
  :vc (:url "https://github.com/cat-emacs/mcp.el"
       :branch "feat/oauth-client"))

(setq feishu-project-backend 'mcp)
```

`mcp.el` performs protected-resource discovery, dynamic public-client
registration, device authorization, token storage, and refresh. The first
request opens the verification URL in a browser and asks for the displayed
user code. OAuth state is stored under `~/.emacs.d/mcp-oauth/` by default;
do not add tokens to Emacs Custom values or shell arguments.

MCP list queries first retrieve enabled work-item types, then build a minimal
MQL query for the selected type.  Name filtering is deliberately omitted from
that generated MQL because field schemas differ; use `feishu-project-mql` for
schema-specific conditions.  MQL pagination keeps the server-issued
`session_id` and group continuation privately.  Detail requests request `_all`
fields; if the server says more field pages exist, the detail buffer explicitly
marks the result truncated rather than claiming completeness.  Arbitrary MQL
may omit a type key, in which case browser/detail URLs are unavailable.

## Workbench commands

With the MCP backend, the common callback-first UI also provides project aliases,
query history and saved descriptors, schema-aware filters, selectable columns,
marks, CSV/TSV/Org/Markdown/JSON export, and stale-safe detail sections.
`M-x feishu-project-find`, `feishu-project-detail-refresh`,
`feishu-project-show-comments`, `feishu-project-show-related`, and
`feishu-project-show-history` work from their respective list/detail contexts.

List filters are MCP-only. After selecting a type by key or display name, the
backend validates the exact status (`work_item_status`), assignee, and creator
field keys through that type's field configuration before it emits escaped MQL
predicates. OpenAPI rejects filters explicitly rather than silently ignoring
them. `?` opens optional lazy-loaded transient list/detail/batch menus when
`transient` is installed; every action remains available through `M-x` without
it.

Writes are MCP-only and always require explicit confirmation. Field edits,
state transitions, comments, batch edits, and creates validate the current
server schema first. A state transition queries allowed transitions and required
fields before confirmation; required fields stop the transition instead of being
silently guessed. Batch edits snapshot marks, validate each project/type once,
confirm only after all schemas pass, and report final success/failure IDs.
Create validates enabled template `option_id` values and uses aggregate
`FieldConfList` metadata to reject missing required fields. Comment deletion is
not exposed because the MCP contract does not verify it.

Attachments support only direct single-part transfers. Upload metadata uses
attachment resource type `15`; download takes the MCP-provided `file_url`.
Multipart metadata is rejected. The temporary signed URL, file token, and
`X-Meego-File-Sign` header are used only for the transfer and are never shown,
logged, or persisted.

## Commands and keys

- `M-x feishu-project-list` lists work items.
- `M-x feishu-project-mql` runs a complete MQL query.
- List: `RET` details, `o` browser, `w` copy ID, `W` copy URL, `g` refresh,
  `n`/`p` next/previous page, `q` quit.

The UI is callback-first: it renders a loading state immediately, prevents
duplicate requests, and ignores stale asynchronous results.

## Development

```sh
make
```

The test suite never contacts Feishu, starts OAuth, transfers files, or requires
`mcp.el`. It tests backend dispatch, OpenAPI request headers, MCP payload
normalization, MQL continuation and schema-aware filters, schema-first create
and transition validation, comment and batch payloads, exact single-part
attachment transfer contracts, column/mark/export behavior, lazy transient
menus, and stale callbacks.

Licensed under GPL-3.0-or-later.
