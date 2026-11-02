# mdvi
<p align="center"><a href="./README.md">English</a> · <a href="./README.zh-CN.md">简体中文</a> · <a href="./README.ja.md">日本語</a></p>

`mdvi` is a terminal markdown viewer with Vim-style navigation.

It renders markdown into a polished full-screen TUI with fast keyboard navigation, clear typography, and predictable behavior for large files.

<img width="1595" height="582" alt="Screenshot 2026-02-10 at 4 58 34 PM" src="https://github.com/user-attachments/assets/1b2da7a1-b5fb-4169-9d10-bcd97a07ae45" />

## Features

- Full-screen terminal viewer (`crossterm` + `ratatui`)
- Vim-style navigation controls
- Visible cursor and `line:column` cursor position in the status bar
- High-quality markdown rendering via `pulldown-cmark`
- Word wrapping that keeps scrolling, search, selection, and inline images aligned
- Unicode-aware layout and cursor movement, including wide characters and emoji
- Support for:
  - headings
  - lists and task lists
  - blockquotes
  - inline markdown images (`![alt](...)`) and HTML `<img ...>` tags
  - lazy image loading with immediate placeholder layout reservation
  - syntax-highlighted fenced code blocks and inline code
  - links, aligned tables with distinct headers, footnotes, emphasis/strong/strikethrough
- Configurable image backend via `--image-protocol` (`auto`, `symbols`, `halfblocks`, `sixel`, `kitty`, `iterm2`)
- SVG images, rasterized with `resvg`
- High-detail text-mode images when the terminal has no graphics protocol: each cell picks the best
  of sextant, quadrant and eighth-block glyphs, and SVG text is drawn as real, crisp terminal text
- Reload from disk on demand (`r`)
- Start at specific line (`--line`)
- Works with standard terminal keys (arrows, page up/down, home/end)

Notes:
- Local, `file://`, and remote `http://` / `https://` image sources are supported.
- HTML `<img>` `width` and `height` hints are used to reserve image space before lazy loading finishes.
- On macOS, terminal-native image protocols can trigger an "Allow Terminal-Initiated Display?" prompt.
  Use `--image-protocol symbols` if you want image rendering without that dialog.
- Fenced code blocks with language tags (for example, a block tagged `rust`) render with syntax highlighting.
- Code blocks preserve blank lines and expand tabs to four-column stops.
- Resizing the terminal keeps the cursor anchored to its text. An active rectangular
  selection clears when the width changes because its screen coordinates change.

Try the rendering showcase with `make run ARGS='examples/rendering.md'`.

## Install

### Homebrew (tap)

```bash
brew tap taf2/tap
brew install mdvi
```

### From source

Install a current stable Rust toolchain (including Cargo), a C linker, and Make.
From the project root, build an optimized binary with:

```bash
make
./target/release/mdvi README.md
```

The first build downloads dependencies and takes longer; subsequent builds reuse
Cargo's cache. To install `mdvi` into Cargo's bin directory (usually `~/.cargo/bin`):

```bash
make install
```

If Make is unavailable, use `cargo build --release --locked` to build or
`cargo install --locked --path .` to install. Ensure Cargo's bin directory is on
your `PATH` when using the installed command.

### Run without install

```bash
make run
make run ARGS='--image-protocol symbols README.md'
```

## Usage

```bash
mdvi [OPTIONS] <PATH>
```

Examples:

```bash
mdvi docs/spec.md
mdvi --line 120 CHANGELOG.md
mdvi --image-protocol symbols README.md
```

## Navigation

- `j` / `Down`: scroll down one line
- `k` / `Up`: scroll up one line
- `h` / `Left`, `l` / `Right`: move the cursor horizontally
- Wide tables keep their columns aligned; move right/left with `l`/`h` or the arrow keys to scroll across them.
- `w` / `b`: move to the next / previous word
- `W` / `B`: move to the next / previous whitespace-delimited word
- `d`: half-page down (less-style)
- `u`: half-page up (less-style)
- `Ctrl-d`: half-page down
- `Ctrl-u`: half-page up
- `PageDown`: full-page down
- `PageUp`: full-page up
- `Ctrl-f`: full-page down (Vim-style)
- `Ctrl-b`: full-page up (Vim-style)
- `g` / `Home`: jump to top
- `G` / `End`: jump to bottom
- `r`: reload file from disk
- `/`: start search
- `n`: next search match
- `N`: previous search match
- `Enter`: submit search; `Esc`: cancel search input
- `Ctrl-v`: toggle rectangular selection; `y`: copy selection to the clipboard
- `Esc`: clear rectangular selection
- `?`: toggle help line
- `q`: quit
- `Ctrl-c`: quit from any mode

Search is literal and case-insensitive. Submit an empty search to clear highlights.
Clipboard copying requires a clipboard service supported by your environment.

## Why Rust?

Rust is a strong fit for a serious CLI viewer:

- precise terminal control
- excellent performance for large files
- single executable distribution
- mature ecosystem for TUI and markdown parsing

## Development

```bash
make help     # list all commands
make check    # formatting check, tests, and Clippy (warnings are errors)
make fmt      # format Rust source
make test     # run tests only
make debug    # faster development build in target/debug/mdvi
make clean    # remove build artifacts
```

Builds and tests use the committed `Cargo.lock` with `--locked` for reproducibility.
CI runs checks and release builds on Linux and macOS. You can override Cargo with
`make CARGO=/path/to/cargo`; standard Cargo environment variables such as
`CARGO_TARGET_DIR` and `CARGO_BUILD_JOBS` are also respected.

## License

MIT
