.PHONY: docs

ROOT_DIR = $(shell pwd)
DOCS_DIR = $(ROOT_DIR)/docs
REPO = $(shell git config --get remote.origin.url)
DOCS_BUILD_DIR = $(DOCS_DIR)/build
DOCS_PROD_DIR = $(DOCS_DIR)/master
DOCS_GIT_HACK = $(DOCS_DIR)/.git
LOCAL_DOCS_HOST = localhost
LOCAL_DOCS_PORT = 5099

$(DOCS_GIT_HACK):
	-@ln -s $(ROOT_DIR)/.git $(DOCS_DIR)

devdocs: local-docs
	@echo "\nRunning docs server on http://$(LOCAL_DOCS_HOST):$(LOCAL_DOCS_PORT)..."
	@lein simpleton $(LOCAL_DOCS_PORT) file :from $(DOCS_PROD_DIR)/current

docs: local-docs

local-docs: clean-docs
	@echo "\nBuilding docs ...\n"
	@lein codox

prod-docs: clean-docs $(DOCS_GIT_HACK) local-docs

clean-docs:
	@rm -rf $(DOCS_PROD_DIR)/current

setup-temp-repo: $(DOCS_GIT_HACK)
	@echo "\nSetting up temporary git repos for gh-pages ...\n"
	@rm -rf $(DOCS_PROD_DIR)/.git $(DOCS_PROD_DIR)/*/.git
	@cd $(DOCS_PROD_DIR) && git init
	@cd $(DOCS_PROD_DIR) && git add * > /dev/null
	@cd $(DOCS_PROD_DIR) && git commit -a -m "Generated content." > /dev/null

teardown-temp-repo:
	@echo "\nTearing down temporary gh-pages repos ..."
	@rm $(DOCS_DIR)/.git
	@rm -rf $(DOCS_PROD_DIR)/.git $(DOCS_PROD_DIR)/*/.git

publish: prod-docs setup-temp-repo
	@echo "\nPublishing docs ...\n"
	@cd $(DOCS_PROD_DIR) && git push -f $(REPO) master:gh-pages
	@make teardown-temp-repo
