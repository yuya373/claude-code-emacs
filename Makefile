# Makefile for claude-code-emacs

EMACS ?= emacs
BATCH = $(EMACS) -batch -Q -L .

# Files
EL_FILES = claude-code-emacs.el
MCP_EL_FILES = claude-code-emacs-mcp.el
TEST_FILES = test-claude-code-emacs.el test-claude-code-emacs-mcp.el

.PHONY: test clean compile install-deps all mcp-build mcp-clean mcp-install mcp-dev mcp-start mcp-test

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
	@if [ -f $(MCP_EL_FILES) ]; then \
		echo "Compiling MCP integration..."; \
		$(BATCH) -l package \
			--eval "(package-initialize)" \
			-f batch-byte-compile $(MCP_EL_FILES); \
	fi

test: install-deps
	@echo "Running tests..."
	@$(BATCH) -l package \
		--eval "(package-initialize)" \
		-l run-tests.el

clean: mcp-clean
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

all: clean compile mcp-build test
