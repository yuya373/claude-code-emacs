# Makefile for claude-code-emacs

EMACS ?= emacs
BATCH = $(EMACS) -batch -Q -L .

# Files
EL_FILES = claude-code-emacs.el
TEST_FILES = test-claude-code-emacs.el

.PHONY: test clean compile install-deps all

# Install dependencies for development
install-deps:
	@echo "Installing dependencies..."
	@$(BATCH) -l install-deps.el

# Compile with actual dependencies
compile: install-deps
	@echo "Compiling with actual dependencies..."
	@$(BATCH) -l package \
		--eval "(package-initialize)" \
		-f batch-byte-compile $(EL_FILES)

test: install-deps
	@echo "Running tests..."
	@$(BATCH) -l package \
		--eval "(package-initialize)" \
		-l run-tests.el

clean:
	@echo "Cleaning..."
	@rm -f *.elc

all: clean compile test
