show-versions:
	@echo Project: $(PROJ), $(PROJ_VERSION)
	@echo Erlang: $(ERL_VERSION)
	@echo JInterface: $(JINTERFACE_VER)
	@echo Clojure: $(CLOJURE_VER)
	@echo lein/JVM: $(shell lein version)

local:
	lein jar && lein install

local-standalone:
	lein uberjar && install

debug:
	@echo $(PROJ)
	@echo $(ERL_LIBS)
	@echo $(JINTERFACE)
	@echo $(JINTERFACE_VER)
	@echo $(VERSION)
	@echo $(JAR)

clean-jinterface-build:
ifeq ($(strip $(JINTERFACE_BUILD)),)
	echo
else
	rm -rf $(JINTERFACE_BUILD)
endif

build-jinterface: clean-jinterface-build
	mkdir -p $(JINTERFACE_BUILD)/src
	cp -r $(ERL_LIBS)/lib/jinterface-$(JINTERFACE_VER)/java_src \
	 $(JINTERFACE_BUILD)/src/java
	cat ./resources/project.clj.tmpl | \
	sed 's/{{VERSION}}/$(JINTERFACE_VER)/g' > \
	$(JINTERFACE_BUILD)/project.clj
	cd $(JINTERFACE_BUILD) && lein jar

jinterface: build-jinterface
	cd $(JINTERFACE_BUILD) && lein deploy clojars

jinterface-local: build-jinterface
	cd $(JINTERFACE_BUILD) && lein install
