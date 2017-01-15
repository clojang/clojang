LFE = ./_build/dev/lib/lfe/bin/lfe
LFE_NODE_NAME = clojang-lfe

lfe-repl:
	@rebar3 as dev compile
	@$(LFE) -pa `rebar3 as dev path -s " -pa "` -sname $(LFE_NODE_NAME)
