check:
	@lein +testing :unit

check-system:
	@lein +testing :system

check-integration:
	@lein +testing :integration

check-all:
	@lein +testing :all
