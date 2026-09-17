EMACS ?= emacs
BATCH = $(EMACS) -Q --batch
SOURCES = feishu-project.el feishu-project-openapi.el feishu-project-mcp.el \
	feishu-project-cli.el feishu-project-workbench.el feishu-project-export.el

.PHONY: all compile test clean

all: clean compile test

compile:
	$(BATCH) -L . -L test \
		--eval "(setq byte-compile-error-on-warn t)" \
		-f batch-byte-compile $(SOURCES)

test:
	$(BATCH) -L . -L test \
		-l feishu-project-test \
		-f ert-run-tests-batch-and-exit

clean:
	rm -f *.elc test/*.elc
