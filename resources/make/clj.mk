local:
	lein jar && lein install

local-standalone:
	lein uberjar && install

clj-repl:
	@lein with-profile +dev repl

clojars:
	@lein deploy clojars
