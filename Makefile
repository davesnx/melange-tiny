project_name = melange-tiny

DUNE = opam exec -- dune
opam_file = $(project_name).opam

.PHONY: help
help: ## Print this help message
	@echo "";
	@echo "List of available make commands";
	@echo "";
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "  \033[36m%-15s\033[0m %s\n", $$1, $$2}';
	@echo "";

.PHONY: build
build: ## Build the project, including non installable libraries and executables
	$(DUNE) build --root . @all

.PHONY: dev
dev: ## Build in watch mode
	$(DUNE) build --root . -w @all

.PHONY: clean
clean: ## Clean artifacts
	$(DUNE) clean --root .

.PHONY: test
test: ## Run the tests, needs node
	$(DUNE) build --root . @runtest

.PHONY: test-watch
test-watch: ## Run the tests in watch mode
	$(DUNE) build --root . @runtest -w

.PHONY: size
size: build ## Print the size table, needs esbuild
	node size/size.mts

.PHONY: create-switch
create-switch: ## Create opam switch
	opam switch create . 5.4.0 --deps-only --with-test --no-install -y

.PHONY: install
install: ## Install dependencies
	opam update
	opam install . --deps-only --with-test --with-doc --with-dev-setup -y
	npm install

.PHONY: init
init: create-switch install ## Create a local dev enviroment
