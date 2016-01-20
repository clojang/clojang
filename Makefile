PROJ := $(strip $(word 2, $(shell grep defproject project.clj )))
PROJ_VERSION := $(strip $(subst \", , $(word 3, $(shell grep defproject project.clj ))))
ERL_VERSION := $(shell erl -eval "io:format(erlang:system_info(system_version)),halt()" -noshell)
ERL_LIBS := $(shell erl -eval "io:format(code:root_dir()),halt()" -noshell)
JINTERFACE := $(shell ls -1 $(ERL_LIBS)/lib/|grep jinterface)
JINTERFACE_VER := $(strip $(subst \", , $(word 2, $(subst -, , $(JINTERFACE)))))
JINTERFACE_JAR := jinterface-$(JINTERFACE_VER).jar
JINTERFACE_BUILD := /tmp/jinterface/$(JINTERFACE_VER)
CLOJURE_DEP := $(strip $(shell grep "org.clojure/clojure" project.clj))
CLOJURE_VER := $(subst ], , $(word 3, $(CLOJURE_DEP)))
JAR := $(PROJ)-$(VERSION).jar
UBERJAR := $(PROJ)-$(VERSION)-standalone.jar
LOCAL_MAVEN := ~/.m2/repository

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

docs:
	@lein codox

.PHONY: docs
