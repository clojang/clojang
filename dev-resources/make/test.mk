check:
	@lein with-profile +test test :unit

check-system:
	@lein with-profile +test test :system

check-integration:
	@lein with-profile +test test :integration

check-all: check-deps
	@lein with-profile +test test :all

kibit:
	@lein with-profile +test kibit

bikeshed:
	@lein with-profile +test bikeshed

base-eastwood:
	@lein with-profile +test eastwood "$(EW_OPTS)"

yagni:
	@lein with-profile +test yagni

eastwood:
	@EW_OPTS="{:namespaces [:source-paths]}" make base-eastwood

lint: kibit eastwood bikeshed yagni

lint-unused:
	@EW_OPTS="{:linters [:unused-fn-args :unused-locals :unused-namespaces :unused-private-vars :wrong-ns-form] :namespaces [:source-paths]}" make base-eastwood

lint-ns:
	@EW_OPTS="{:linters [:unused-namespaces :wrong-ns-form] :namespaces [:source-paths]}" make base-eastwood

check-deps:
	@lein with-profile +test do ancient check :all
