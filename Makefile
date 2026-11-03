.DEFAULT_GOAL := build

CARGO ?= cargo
ARGS ?= README.md

.PHONY: build debug run install deps test fmt lint check clean help

deps: ## Install missing build tools and Rust (may invoke sudo)
	bash scripts/install-deps.sh

build: ## Build the optimized binary in target/release/mdvi
	$(CARGO) build --release --locked

debug: ## Build a faster-to-compile development binary
	$(CARGO) build --locked

run: ## Run the viewer (override with ARGS='--line 10 file.md')
	$(CARGO) run --locked -- $(ARGS)

install: ## Install mdvi into Cargo's bin directory
	$(CARGO) install --locked --path .

test: ## Run all tests
	$(CARGO) test --locked --all-targets

fmt: ## Format Rust source
	$(CARGO) fmt --all

lint: ## Check Rust code for warnings
	$(CARGO) clippy --locked --all-targets -- -D warnings

check: ## Check formatting, run tests, and lint
	$(CARGO) fmt --all -- --check
	$(MAKE) test
	$(MAKE) lint

clean: ## Remove Cargo build artifacts
	$(CARGO) clean

help: ## Show available commands
	@awk 'BEGIN {FS = ":.*## "} /^[a-z-]+:.*## / {printf "  make %-10s %s\n", $$1, $$2}' $(MAKEFILE_LIST)
