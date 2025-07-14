# Makefile for claude-code

EMACS ?= emacs
BATCH = $(EMACS) -batch -Q -L .

# Files
CORE_FILES = claude-code-core.el \
	     claude-code-commands.el \
	     claude-code-ui.el \
	     claude-code-prompt.el

MCP_MODULES = claude-code-mcp-connection.el \
	      claude-code-mcp-protocol.el \
	      claude-code-mcp-tools.el \
	      claude-code-mcp-events.el

TEST_CORE_FILES = claude-code-core-test.el \
		  claude-code-buffer-test.el \
		  claude-code-commands-test.el \
		  claude-code-ui-test.el \
		  claude-code-prompt-test.el

TEST_MCP_FILES = claude-code-mcp-connection-test.el \
		 claude-code-mcp-protocol-test.el \
		 claude-code-mcp-tools-test.el \
		 claude-code-mcp-events-test.el

EL_FILES = $(CORE_FILES) claude-code.el
MCP_EL_FILES = $(MCP_MODULES) claude-code-mcp.el
ALL_EL_FILES = $(EL_FILES) $(MCP_EL_FILES)
TEST_FILES = $(TEST_CORE_FILES) $(TEST_MCP_FILES)

.PHONY: test clean compile install-deps all mcp-build mcp-clean mcp-install mcp-dev mcp-start mcp-test lint package-lint

# Install dependencies for development
install-deps:
	@echo "Installing dependencies..."
	@$(BATCH) -l install-deps.el

# Compile with actual dependencies
compile: install-deps
	@echo "Compiling with actual dependencies..."
	@$(BATCH) -l package \
		--eval "(package-initialize)" \
		-f batch-byte-compile $(ALL_EL_FILES)

# Compile without optional dependencies (for MELPA testing)
compile-minimal:
	@echo "Compiling without optional dependencies (websocket, lsp-mode)..."
	@echo "Installing minimal dependencies..."
	@$(BATCH) --eval "(progn (package-initialize) \
			      (add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\") t) \
			      (package-refresh-contents) \
			      (dolist (pkg '(projectile vterm transient markdown-mode)) \
				(unless (package-installed-p pkg) \
				  (package-install pkg))))"
	@echo "Compiling core files..."
	@$(BATCH) -l package \
		--eval "(package-initialize)" \
		-f batch-byte-compile $(EL_FILES)
	@echo "Compiling MCP files (optional)..."
	@$(BATCH) -l package \
		--eval "(package-initialize)" \
		-f batch-byte-compile $(MCP_EL_FILES) || true

# Run package-lint on all elisp files
package-lint:
	@echo "Running package-lint..."
	@$(BATCH) --eval "(progn (package-initialize) \
			      (add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\") t) \
			      (package-refresh-contents) \
			      (unless (package-installed-p 'package-lint) \
				(package-install 'package-lint)))" \
		--eval "(require 'package-lint)" \
		--eval "(setq package-lint-main-file \"claude-code.el\")" \
		--eval "(setq package-lint-batch-fail-on-warnings t)" \
		-f package-lint-batch-and-exit $(ALL_EL_FILES)

lint: package-lint

test: compile
	@echo "Running tests..."
	@$(BATCH) -l package \
		--eval "(package-initialize)" \
		-l run-tests.el

clean:
	@echo "Cleaning..."
	@rm -f *.elc

# MCP Server targets
mcp-install:
	@if [ -d mcp-server ]; then \
		echo "Installing MCP server dependencies..."; \
		cd mcp-server && npm install; \
	fi

mcp-build: mcp-install
	@if [ -d mcp-server ]; then \
		echo "Building MCP server..."; \
		cd mcp-server && npm run build; \
	fi

mcp-clean:
	@if [ -d mcp-server ]; then \
		echo "Cleaning MCP server..."; \
		cd mcp-server && npm run clean && rm -rf node_modules; \
	fi

mcp-dev: mcp-install
	@if [ -d mcp-server ]; then \
		echo "Starting MCP server in development mode..."; \
		cd mcp-server && npm run dev; \
	fi

mcp-start: mcp-build
	@if [ -d mcp-server ]; then \
		echo "Starting MCP server..."; \
		cd mcp-server && npm start; \
	fi

mcp-test: mcp-install
	@if [ -d mcp-server ]; then \
		echo "Running MCP server tests..."; \
		cd mcp-server && npm test; \
	fi

all: clean test mcp-clean mcp-build mcp-test
