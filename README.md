# mdvi
<p align="center"><a href="./README.md">English</a> · <a href="./README.zh-CN.md">简体中文</a> · <a href="./README.ja.md">日本語</a></p>

**A fast, full-screen markdown viewer for the terminal, with Vim-style navigation.**

`mdvi` renders markdown with clear typography, syntax-highlighted code, aligned
tables, and inline images, and stays responsive on large files.

<img width="1595" height="582" alt="mdvi rendering a markdown document in the terminal" src="https://github.com/user-attachments/assets/1b2da7a1-b5fb-4169-9d10-bcd97a07ae45" />

## Quick start

```bash
brew tap taf2/tap
brew install mdvi

mdvi README.md
```

Press `?` for help and `q` to quit. Other install options are in [Install](#install).

## Features

**Rendering**
- Headings, lists, task lists, blockquotes, footnotes, links, and emphasis/strong/strikethrough
- Fenced code blocks with syntax highlighting. Authored line breaks are preserved, tabs expand to
  four-column stops, and wide blocks scroll horizontally instead of wrapping
- Tables with distinct headers and aligned columns. Wide tables scroll horizontally
- Word wrapping that keeps scrolling, search, selection, and images aligned
- Unicode-aware layout, including wide characters and emoji
- Optional maximum content width (`--width 80`)
- Lightweight rendering of embedded HTML: headings, paragraphs, lists, tables, links, `<pre>`,
  `<details>`, inline styles (`<b>`, `<kbd>`, `<code>`, …), entities, and `align`/`<center>`

**Images**
- Markdown images (`![alt](...)`) and HTML `<img>` tags
- Local, `file://`, and remote `http(s)://` sources, loaded lazily with space reserved up front
- SVG support, rasterized with `resvg`
- Native graphics protocols (Kitty, iTerm2, Sixel) when available
- High-detail text-mode fallback for other terminals (see [Images](#images))

**Navigation**
- Vim-style motions, plus arrows, Page Up/Down, and Home/End
- Visible cursor with `line:column` in the status bar
- Case-insensitive search with match highlighting
- Rectangular selection with copy to the clipboard
- Reload from disk without restarting (`r`)

## Install

### Homebrew

```bash
brew tap taf2/tap
brew install mdvi
```

### From source

Requires a current stable Rust toolchain (with Cargo), a C linker, and Make.

```bash
git clone https://github.com/taf2/mdvi.git
cd mdvi
./scripts/install-deps.sh  # installs missing build tools and Rust; may use sudo
export PATH="${CARGO_HOME:-$HOME/.cargo}/bin:$PATH"
make            # builds target/release/mdvi
make install    # installs into Cargo's bin directory (usually ~/.cargo/bin)
```

The dependency installer supports Fedora, Debian/Ubuntu, Arch Linux, and macOS
(via Apple's Command Line Tools). If Make is already available, `make deps`
runs the same installer. Existing working Rust installations are kept; use
`rustup update stable` if an older toolchain cannot compile the locked dependencies.

Make sure Cargo's bin directory is on your `PATH`. Without Make, use
`cargo build --release --locked` to build or `cargo install --locked --path .` to install.

To try it without installing:

```bash
make run                                  # opens README.md
make run ARGS='examples/rendering.md'     # rendering showcase
```

## Usage

```bash
mdvi [OPTIONS] <PATH>
```

| Option | Description |
| --- | --- |
| `-l`, `--line <N>` | Start at line `N` (1-based). Default: `1` |
| `--width <COLS>` | Maximum content width in columns, excluding the border. Shrinks to fit narrower terminals. Default: full terminal width |
| `--image-protocol <P>` | Image backend: `auto` (default), `symbols`, `halfblocks`, `sixel`, `kitty`, `iterm2` |
| `-h`, `--help` | Show help |
| `-V`, `--version` | Show version |

Examples:

```bash
mdvi docs/spec.md
mdvi --width 80 docs/spec.md
mdvi --line 120 CHANGELOG.md
mdvi --image-protocol symbols README.md
```

## Key bindings

### Movement

| Key | Action |
| --- | --- |
| `j` / `Down` | Down one line |
| `k` / `Up` | Up one line |
| `h` / `Left`, `l` / `Right` | Move cursor left / right; scrolls wide tables and code blocks |
| `w` / `b` | Next / previous word |
| `W` / `B` | Next / previous whitespace-delimited word |
| `d` / `Ctrl-d` | Half page down |
| `u` / `Ctrl-u` | Half page up |
| `Ctrl-f` / `PageDown` | Full page down |
| `Ctrl-b` / `PageUp` | Full page up |
| `g` / `Home` | Jump to top |
| `G` / `End` | Jump to bottom |

### Search

| Key | Action |
| --- | --- |
| `/` | Start a search |
| `Enter` / `Esc` | Submit / cancel search input |
| `n` / `N` | Next / previous match |

Search is literal and case-insensitive. Submit an empty search to clear highlights.

### Selection and other

| Key | Action |
| --- | --- |
| `Ctrl-v` | Toggle rectangular selection |
| `y` | Copy the selection to the clipboard |
| `Esc` | Clear the selection |
| `r` | Reload the file from disk |
| `?` | Toggle the help line |
| `q` | Quit |
| `Ctrl-c` | Quit from any mode |

Copying requires a clipboard service supported by your environment. Resizing the
terminal keeps the cursor anchored to its text; an active selection is cleared when
the width changes, because its screen coordinates no longer match.

## Images

With `--image-protocol auto` (the default), `mdvi` uses your terminal's native
graphics protocol when it detects one. Otherwise it draws images as text: each cell
picks the best of sextant, quadrant, and eighth-block glyphs, and text inside SVGs is
rendered as real terminal text so it stays sharp.

- HTML `<img>` `width` and `height` attributes are used to reserve space before an image finishes loading.
- On macOS, native image protocols can trigger an "Allow Terminal-Initiated Display?"
  prompt. Use `--image-protocol symbols` to render images without it.

## Development

```bash
make help     # list all commands
make check    # format check, tests, and Clippy (warnings are errors)
make fmt      # format Rust source
make test     # run tests only
make debug    # faster development build in target/debug/mdvi
make clean    # remove build artifacts
```

Builds and tests use the committed `Cargo.lock` with `--locked`. CI runs checks and
release builds on Linux and macOS. Override Cargo with `make CARGO=/path/to/cargo`;
standard Cargo variables such as `CARGO_TARGET_DIR` and `CARGO_BUILD_JOBS` are respected.

Changes are tracked in [CHANGELOG.md](CHANGELOG.md).

## License

[MIT](LICENSE)
