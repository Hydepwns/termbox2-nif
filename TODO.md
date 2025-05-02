# TODO List

This file tracks suggested improvements and future work for the `termbox2-nif` project.

## High Priority

- **Enhance Testing:**
  - [x] Ensure there's a reliable way to run the test suite (e.g., `make test` or `rebar3 ct`).
  - [x] Add the test command to the `README.md`.
  - [x] **Crucially, add tests specifically for macOS ARM** that verify the NIF loads correctly (`:on_load` success) and basic functions (`tb_init`, `tb_shutdown`, etc.) are callable without `:undef` errors. This will help prevent regressions of the NIF loading issue.
  - [ ] Consider setting up Continuous Integration (CI) via GitHub Actions to automate builds and tests on Linux, macOS x86_64, and macOS ARM.

## Documentation

- [x] Add a concise Erlang usage example to the `README.md`.
- [x] Ensure API documentation (e.g., `edoc`) is generated correctly and add instructions for accessing it to the `README.md`.
- [ ] Update the logo URL in the `README.md` to point to the `Hydepwns/termbox2-nif` repository if the image file exists there.

## Future Work / Roadmap (from README)

- [ ] Create a Gleam wrapper.
- [ ] Create an Elixir wrapper.

## Lower Priority / Considerations

- [ ] Clarify and document the best way to use this library from Elixir/Mix projects (e.g., using `mix_rebar3_plugin` or potentially adding `mix.exs` support).
- [ ] Configure C linter include paths for local development environments to resolve spurious errors related to finding `erl_nif.h`.
- [ ] Review and potentially update the underlying `termbox2` C library submodule if needed.
