.PHONY: docs

ROOT_DIR = $(shell pwd)
DOCS_DIR = $(ROOT_DIR)/docs
REPO = $(shell git config --get remote.origin.url)
DOCS_BUILD_DIR = $(DOCS_DIR)/build
DOCS_PROD_DIR = $(DOCS_DIR)/master
CURRENT = $(DOCS_PROD_DIR)/current
ERL_DOCS_SRC = ~/Dropbox/Docs/Erlang
ERL_DOCS_DIR = $(CURRENT)/erlang
JAVA_DOCS_DIR = $(ERL_DOCS_DIR)/java
DOCS_GIT_HACK = $(DOCS_DIR)/.git
REMOTE_DOCS_HOST = http://oubiwann.github.io/clojang/current/java
LOCAL_DOCS_HOST = localhost
LOCAL_DOCS_PORT = 5099

$(DOCS_GIT_HACK):
	-@ln -s $(ROOT_DIR)/.git $(DOCS_DIR)

devdocs: local-docs
	@echo "\nRunning docs server on http://$(LOCAL_DOCS_HOST):$(LOCAL_DOCS_PORT)..."
	@lein simpleton $(LOCAL_DOCS_PORT) file :from $(CURRENT)

docs: local-docs

local-docs: clean-docs pre-docs clojang-docs java-docs erl-docs

prod-docs: clean-docs $(DOCS_GIT_HACK) local-docs

pre-docs:
	@echo "\nBuilding docs ...\n"

java-docs:
	@javadoc -public \
	         -use \
	         -version \
	         -author \
	         -nodeprecated \
	         -keywords \
	         -quiet \
	         -d $(JAVA_DOCS_DIR) $(JINTERFACE_FILES)

erl-docs:
	@mkdir -p $(ERL_DOCS_DIR) $(DOCS_PROD_DIR)/doc
	@cp \
	$(ERL_DOCS_SRC)/lib/jinterface-$(JINTERFACE_VER)/doc/html/users_guide.html \
	$(ERL_DOCS_SRC)/lib/jinterface-$(JINTERFACE_VER)/doc/html/jinterface_users_guide.html \
	$(ERL_DOCS_DIR)
	@cp -r \
	$(ERL_DOCS_SRC)/doc/js \
	$(ERL_DOCS_SRC)/doc/otp_doc.css \
	$(ERL_DOCS_SRC)/doc/erlang-logo.png \
	$(CURRENT)
	@patch \
	$(CURRENT)/erlang/users_guide.html \
	resources/erlang_users_guide.patch
	@patch \
	$(CURRENT)/erlang/jinterface_users_guide.html \
	resources/jinterface_users_guide.patch

clojang-docs:
	@lein codox

clean-docs:
	@rm -rf $(CURRENT)

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

publish-docs: prod-docs setup-temp-repo
	@echo "\nPublishing docs ...\n"
	@cd $(DOCS_PROD_DIR) && git push -f $(REPO) master:gh-pages
	@make teardown-temp-repo
