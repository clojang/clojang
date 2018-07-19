check:
	@lein ltest :unit

check-system:
	@lein ltest :system

check-integration:
	@lein ltest :integration

check-all: check-deps
	@lein ltest :all

kibit:
	@lein with-profile +lint kibit

bikeshed:
	@lein with-profile +lint bikeshed

base-eastwood:
	@lein with-profile +lint eastwood "$(EW_OPTS)"

yagni:
	@lein with-profile +lint yagni

eastwood:
	@EW_OPTS="{:namespaces [:source-paths]}" make base-eastwood

lint: kibit eastwood bikeshed yagni

lint-unused:
	@EW_OPTS="{:linters [:unused-fn-args :unused-locals :unused-namespaces :unused-private-vars :wrong-ns-form] :namespaces [:source-paths]}" make base-eastwood

lint-ns:
	@EW_OPTS="{:linters [:unused-namespaces :wrong-ns-form] :namespaces [:source-paths]}" make base-eastwood

check-deps:
	@lein check-vers
