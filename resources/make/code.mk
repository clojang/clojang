show-versions:
	@echo Project: $(PROJ), $(PROJ_VERSION)
	@echo Erlang: $(ERL_VERSION)
	@echo JInterface: $(JINTERFACE_VER)
	@echo Clojure: $(CLOJURE_VER)
	@echo lein/JVM: $(shell lein version)

debug:
	@echo $(PROJ)
	@echo $(ERL_LIBS)
	@echo $(JINTERFACE)
	@echo $(JINTERFACE_VER)
	@echo $(VERSION)
	@echo $(JAR)

