PROJECT = termbawx2
DIALYZER = dialyzer

ERL       ?= erl
REBAR3 := $(shell which rebar3 2>/dev/null || echo ./rebar3)
REBAR3_VERSION := 3.17.0
REBAR3_URL := https://github.com/erlang/rebar3/releases/download/$(REBAR3_VERSION)/rebar3

# Default target
.DEFAULT_GOAL := all

# Check for required tools
check-tools:
	@which $(ERL) >/dev/null 2>&1 || (echo "Error: Erlang not found. Please install Erlang." && exit 1)
	@which $(DIALYZER) >/dev/null 2>&1 || (echo "Warning: Dialyzer not found. Static analysis will be skipped." && exit 0)

all: check-tools compile

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

distclean: clean
	rm -f $(REBAR3)
	rm -rf c_src/termbox2

# Development targets
dev: compile
	$(REBAR3) shell

dialyze: check-tools
	@$(DIALYZER) --src src --plt .$(PROJECT).plt \
		-Werror_handling -Wunmatched_returns -Wunderspecs

build-plt: check-tools
	@$(DIALYZER) --build_plt --output_plt .$(PROJECT).plt \
		--apps kernel stdlib

# Wrapper-specific targets
build-nif:
	$(MAKE) -C c_src

.PHONY: gleam-copy-nif-beam
gleam-copy-nif-beam:
	mkdir -p wrappers/gleam/_build/dev/lib/termbox2_nif/ebin
	cp _build/default/lib/termbox2_nif/ebin/termbox2_nif.beam wrappers/gleam/_build/dev/lib/termbox2_nif/ebin/
	mkdir -p wrappers/gleam/_build/dev/lib/termbox2_nif/priv
	cp priv/termbox2_nif.so wrappers/gleam/_build/dev/lib/termbox2_nif/priv/

.PHONY: gleam-test
gleam-test: gleam-copy-nif-beam
	cd wrappers/gleam && ERL_LIBS=../../_build/default/lib gleam test

# Unified build targets
.PHONY: build test-all clean-all

build: build-nif
	$(REBAR3) compile
	cd wrappers/elixir && mix deps.get && mix compile
	cd wrappers/gleam && gleam deps download && gleam build
	$(MAKE) gleam-copy-nif-beam

test-all: build
	$(REBAR3) eunit
	cd wrappers/elixir && mix test
	$(MAKE) gleam-test

clean-all: clean
	cd c_src/termbox2 && $(MAKE) clean || true
	cd c_src && $(MAKE) clean || true
	cd wrappers/elixir && mix clean || true
	rm -rf wrappers/elixir/_build
	rm -rf wrappers/gleam/_build
	rm -f wrappers/gleam/_build/dev/lib/termbox2_nif/ebin/termbox2_nif.beam
	rm -f wrappers/gleam/_build/dev/lib/termbox2_nif/priv/termbox2_nif.so
	rm -f wrappers/elixir/_build/dev/lib/termbox2_nif/priv/termbox2_nif.so
	rm -f priv/termbox2_nif.so

# Help target
help:
	@echo "Available targets:"
	@echo "  all        - Build the project (default)"
	@echo "  compile    - Compile the project"
	@echo "  test       - Run Erlang tests"
	@echo "  test-all   - Run all tests (Erlang, Elixir, Gleam)"
	@echo "  clean      - Clean build artifacts"
	@echo "  distclean  - Clean everything including downloaded dependencies"
	@echo "  dev        - Start development shell"
	@echo "  dialyze    - Run dialyzer static analysis"
	@echo "  build-plt  - Build dialyzer PLT"
	@echo "  help       - Show this help message"

