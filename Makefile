PROJECT = termbawx2
DIALYZER = dialyzer

ERL       ?= erl
REBAR3 := $(shell which rebar3 2>/dev/null || echo ./rebar3)
REBAR3_VERSION := 3.17.0
REBAR3_URL := https://github.com/erlang/rebar3/releases/download/$(REBAR3_VERSION)/rebar3

all: compile

$(REBAR3):
	$(ERL) -noshell -s inets -s ssl \
	 -eval '{ok, saved_to_file} = httpc:request(get, {"$(REBAR3_URL)", []}, [], [{stream, "./rebar3"}])' \
	 -s init stop
	chmod +x ./rebar3

compile: $(REBAR3)
	$(REBAR3) compile

test: compile
	$(REBAR3) eunit

clean: $(REBAR3)
	$(REBAR3) clean
	rm -rf _build

distclean:
	rm $(REBAR3)

# dializer

build-plt:
	@$(DIALYZER) --build_plt --output_plt .$(PROJECT).plt \
		--apps kernel stdlib

dialyze:
	@$(DIALYZER) --src src --plt .$(PROJECT).plt  \
		-Werror_handling  -Wunmatched_returns -Wunderspecs

build-all:
	rebar3 compile && rebar3 eunit
	cd wrappers/elixir && mix deps.get && mix test
	cd wrappers/gleam && gleam deps download && gleam test

	cp priv/termbox2_nif.so wrappers/elixir/_build/dev/lib/termbox2_nif/priv/ 2>/dev/null || true
	ln -sf ../../priv/termbox2_nif.so wrappers/gleam/termbox2_nif.so

setup-all:
	rebar3 compile && rebar3 eunit
	cd wrappers/elixir && mix deps.get && mix test
	cd wrappers/gleam && gleam deps download && gleam test

.PHONY: gleam-copy-nif-beam
# Copy the compiled BEAM file into the Gleam wrapper's build output
gleam-copy-nif-beam:
	mkdir -p wrappers/gleam/_build/dev/lib/termbox2_nif/ebin
	cp _build/default/lib/termbox2_nif/ebin/termbox2_nif.beam wrappers/gleam/_build/dev/lib/termbox2_nif/ebin/

.PHONY: gleam-test
# Run Gleam tests with the correct code path
gleam-test: gleam-copy-nif-beam
	cd wrappers/gleam && ERL_LIBS=../../_build/default/lib gleam test

