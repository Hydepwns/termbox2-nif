<a name="readme-top"></a>

[![Contributors](https://img.shields.io/github/contributors/Hydepwns/termbox2-nif.svg?style=for-the-badge)](https://github.com/Hydepwns/termbox2-nif/graphs/contributors)
[![Forks](https://img.shields.io/github/forks/Hydepwns/termbox2-nif.svg?style=for-the-badge)](https://github.com/Hydepwns/termbox2-nif/network/members)
[![Issues](https://img.shields.io/github/issues/Hydepwns/termbox2-nif.svg?style=for-the-badge)](https://github.com/Hydepwns/termbox2-nif/issues)
[![Hex.pm Version](https://img.shields.io/hexpm/v/termbox2_nif.svg?style=for-the-badge)](https://hex.pm/packages/termbox2_nif)
[![License](https://img.shields.io/hexpm/l/termbox2_nif.svg?style=for-the-badge)](https://github.com/Hydepwns/termbox2-nif/blob/master/LICENSE)

<!-- PROJECT LOGO -->
[<img src="https://github.com/wmealing/termbox2-nif/raw/master/images/logo.svg" alt="Logo" width="80" height="80">](https://github.com/Hydepwns/termbox2-nif)
<!-- TODO: Update logo source if moved -->

### TERMBOX2-NIF

An erlang wrapper for the termbox2 library.

[**Explore the docs »**](https://github.com/Hydepwns/termbox2-nif) ·
[View Demo](https://github.com/Hydepwns/termbox2-nif) ·
[Report Bug](https://github.com/Hydepwns/termbox2-nif/issues/new?labels=bug) ·
[Request Feature](https://github.com/Hydepwns/termbox2-nif/issues/new?labels=enhancement&template=feature-request---.md)

---

<!-- TABLE OF CONTENTS -->
## Table of Contents

* [About The Project](#about-the-project)
  * [Built With](#built-with)
* [Getting Started](#getting-started)
  * [Prerequisites](#prerequisites)
  * [Installation](#installation)
* [Testing](#testing)
* [Usage](#usage)
* [Roadmap](#roadmap)
* [Contributing](#contributing)
* [License](#license)
* [Contact](#contact)
* [Acknowledgments](#acknowledgments)

---

<!-- ABOUT THE PROJECT -->
## About The Project

[![Product Name Screen Shot][product-screenshot]](https://example.com)

Ever wanted to write a TUI library based on termbox2 in beam based languages?
Tired of writing web apps that may make your app popular and usable by many while likely wasting near gigabytes of memory?

You could write a TUI, using this library to avoid the pitfalls.

<p align="right">(<a href="#readme-top">back to top</a>)</p>

### Built With

* [![Erlang][Erlang]][Erlang-url]
* [![Termbox][Termbox2]][Termbox2-url]

<p align="right">(<a href="#readme-top">back to top</a>)</p>

<!-- GETTING STARTED -->
## Getting Started

### Prerequisites

You will need to have a:

* modern erlang (Version 22+)
* make
* c compiler.

### Installation

1. Add `termbox2_nif` to your list of dependencies in `rebar.config`:

    ```erlang
    {deps, [
      {termbox2_nif, "~> 0.1.5"}
    ]}.
    ```

2. Ensure `rebar3_hex` is added to your plugins in `rebar.config` (if not already):

    ```erlang
    {plugins, [
      rebar3_hex
    ]}.
    ```

3. Fetch and compile the dependencies:

    ```sh
    rebar3 get-deps
    rebar3 compile
    ```

<p align="right">(<a href="#readme-top">back to top</a>)</p>

<!-- TESTING -->
## Testing

To run the automated tests, use the following command:

```sh
rebar3 ct
```

<p align="right">(<a href="#readme-top">back to top</a>)</p>

<!-- USAGE EXAMPLES -->
## Usage

Here's a basic example of how to initialize termbox, print a message, wait for a key press, and shut down. Run this in an Erlang shell (`erl`):

```erlang
%% Define default attributes (foreground/background)
-define(TB_DEFAULT, 0).

%% Initialize termbox
ok = termbox2_nif:tb_init(),

%% Clear the screen with default attributes
%% Note: tb_set_clear_attrs/2 could be used to change defaults
ok = termbox2_nif:tb_clear(),

%% Print "Hello, termbox!" at position (1, 1)
Str = "Hello, termbox!",
X = 1, Y = 1,
Fg = ?TB_DEFAULT, Bg = ?TB_DEFAULT,
ok = termbox2_nif:tb_print(X, Y, Fg, Bg, Str),

%% Present the buffer to the terminal
ok = termbox2_nif:tb_present(),

%% Wait for any event (like a key press)
{ok, _Type, _Mod, _KeyOrChar} = termbox2_nif:tb_poll_event(),

%% Shut down termbox
ok = termbox2_nif:tb_shutdown().
```

Constants for colors (like `TB_RED`, `TB_BOLD`) and event types/keys are typically defined in the `termbox2.h` header. You would usually define corresponding Erlang constants or functions to map these for easier use.

_For more detailed examples and API usage, please refer to the underlying [termbox2 documentation](https://github.com/termbox/termbox2) and the NIF source code (`c_src/termbox2_nif.c`)._

### API Documentation

To generate local API documentation using `ex_doc`:

```sh
rebar3 ex_doc
```

Then open the generated `doc/index.html` file in your web browser.

<p align="right">(<a href="#readme-top">back to top</a>)</p>

<!-- ROADMAP -->
## Roadmap

* [x] Erlang NIF created.
* [ ] Gleam wrapper created.
* [ ] Elixir wrapper

See the [open issues](https://github.com/Hydepwns/termbox2-nif/issues) for a full list of proposed features (and known issues).

<p align="right">(<a href="#readme-top">back to top</a>)</p>

<!-- CONTRIBUTING -->
## Contributing

Contributions are what make the open source community such an amazing place to learn, inspire, and create. Any contributions you make are **greatly appreciated**.

If you have a suggestion that would make this better, please fork the repo and create a pull request. You can also simply open an issue with the tag "enhancement".
Don't forget to give the project a star! Thanks again!

1. Fork the Project
2. Create your Feature Branch (`git checkout -b feature/AmazingFeature`)
3. Commit your Changes (`git commit -m 'Add some AmazingFeature'`)
4. Push to the Branch (`git push origin feature/AmazingFeature`)
5. Open a Pull Request

<p align="right">(<a href="#readme-top">back to top</a>)</p>

<!-- LICENSE -->
## License

Distributed under the MIT License. See `LICENSE` for more information.

<p align="right">(<a href="#readme-top">back to top</a>)</p>

<!-- CONTACT -->
## Contact

* DROO (Hydepwns) - <https://github.com/hydepwns>
* Wade Mealing - <wmealing@gmail.com>
* Garlic0x1 - <https://github.com/garlic0x1>

Project Link: [https://github.com/Hydepwns/termbox2-nif](https://github.com/Hydepwns/termbox2-nif)

<p align="right">(<a href="#readme-top">back to top</a>)</p>

<!-- ACKNOWLEDGMENTS -->
## Acknowledgments

Used with permission from the garlic0x1 original author. Published to hex.pm with permission.

<p align="right">(<a href="#readme-top">back to top</a>)</p>

<!-- MARKDOWN LINKS & IMAGES -->
<!-- https://www.markdownguide.org/basic-syntax/#reference-style-links -->
[product-screenshot]: images/screenshot.png
[Erlang]: https://img.shields.io/badge/erlang-000000?style=for-the-badge&logo=erlang&logoColor=white
[Erlang-url]: https://www.erlang.org/
[Termbox2]: https://img.shields.io/badge/termbox2-000000?style=for-the-badge&logo=codewars&logoColor=61DAFB
[Termbox2-url]: https://github.com/termbox/termbox2
