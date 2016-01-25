check:
	@lein test :unit

check-system:
	@lein test :system

check-integration:
	@lein test :integration

check-all:
	@lein test :all

check-compiled:
	@lein with-profile +testing test :unit

check-compiled-system:
	@lein with-profile +testing test :system

check-compiled-integration:
	@lein with-profile +testing test :integration

check-compiled-all:
	@lein with-profile +testing test :all
