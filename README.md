# mdview

`mdview` is a professional terminal markdown viewer with Vim-style navigation.

It renders markdown into a polished full-screen TUI with fast keyboard navigation, clear typography, and predictable behavior for large files.

## Features

- Full-screen terminal viewer (`crossterm` + `ratatui`)
- Vim-style navigation controls
- High-quality markdown rendering via `pulldown-cmark`
- Support for:
  - headings
  - lists and task lists
  - blockquotes
  - code blocks and inline code
  - links, tables, footnotes, emphasis/strong/strikethrough
- Live reload (`r`) when the file changes on disk
- Start at specific line (`--line`)
- Works with standard terminal keys (arrows, page up/down, home/end)

## Install

### From source

```bash
cargo install --path .
```

### Run without install

```bash
cargo run -- README.md
```

## Usage

```bash
mdview [OPTIONS] <PATH>
```

Examples:

```bash
mdview docs/spec.md
mdview --line 120 CHANGELOG.md
```

## Navigation

- `j` / `Down`: scroll down one line
- `k` / `Up`: scroll up one line
- `Ctrl-d`: half-page down
- `Ctrl-u`: half-page up
- `PageDown`: full-page down
- `PageUp`: full-page up
- `g` / `Home`: jump to top
- `G` / `End`: jump to bottom
- `r`: reload file from disk
- `/`: start search
- `n`: next search match
- `N`: previous search match
- `?`: toggle help line
- `q`: quit

## Why Rust?

Rust is a strong fit for a serious CLI viewer:

- precise terminal control
- excellent performance for large files
- single static binary distribution
- mature ecosystem for TUI and markdown parsing

## Development

```bash
cargo test
cargo fmt
cargo clippy -- -D warnings
```

## License

MIT
