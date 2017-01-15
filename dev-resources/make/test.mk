check:
	@lein with-profile +testing test :unit

check-system:
	@lein with-profile +testing test :system

check-integration:
	@lein with-profile +testing test :integration

check-all:
	@lein with-profile +testing test :all
