EMACS ?= emacs
BATCH = $(EMACS) -Q --batch

.PHONY: all compile test clean

all: clean compile test

compile:
	$(BATCH) -L . -L test \
		--eval "(setq byte-compile-error-on-warn t)" \
		-f batch-byte-compile feishu-project.el

test:
	$(BATCH) -L . -L test -l feishu-project-test \
		-f ert-run-tests-batch-and-exit

clean:
	rm -f *.elc test/*.elc
