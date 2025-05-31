# Makefile for claude-code-emacs

EMACS ?= emacs
BATCH = $(EMACS) -batch -Q -L .

.PHONY: test clean compile

test:
	@echo "Running tests..."
	@$(BATCH) -l run-tests.el

compile:
	@echo "Compiling..."
	@$(BATCH) -f batch-byte-compile claude-code-emacs.el

clean:
	@echo "Cleaning..."
	@rm -f *.elc

all: clean compile test
