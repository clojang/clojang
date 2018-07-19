.PHONY: docs

ROOT_DIR = $(shell pwd)
DOCS_DIR = $(ROOT_DIR)/docs
CURRENT = $(DOCS_DIR)/current
LOCAL_DOCS_HOST = localhost
LOCAL_DOCS_PORT = 5099

devdocs:
	@echo "\nRunning docs server on http://$(LOCAL_DOCS_HOST):$(LOCAL_DOCS_PORT) ..."
	@lein with-profile docs simpleton $(LOCAL_DOCS_PORT) file :from $(CURRENT)

docs: clean-docs pre-docs clojang-docs

pre-docs:
	@echo "\nBuilding docs ...\n"
	@lein with-profile docs clean

clojang-docs:
	@lein with-profile docs codox

clean-docs:
	@rm -rf $(CURRENT)
