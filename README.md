# mdvi

`mdvi` is a terminal markdown viewer with Vim-style navigation.

It renders markdown into a polished full-screen TUI with fast keyboard navigation, clear typography, and predictable behavior for large files.

<img width="1595" height="582" alt="Screenshot 2026-02-10 at 4 58 34 PM" src="https://github.com/user-attachments/assets/1b2da7a1-b5fb-4169-9d10-bcd97a07ae45" />

## Features

- Full-screen terminal viewer (`crossterm` + `ratatui`)
- Vim-style navigation controls
- Visible cursor and `line:column` cursor position in the status bar
- High-quality markdown rendering via `pulldown-cmark`
- Support for:
  - headings
  - lists and task lists
  - blockquotes
  - inline markdown images (`![alt](...)`) and HTML `<img ...>` tags
  - lazy image loading with immediate placeholder layout reservation
  - syntax-highlighted fenced code blocks and inline code
  - links, tables, footnotes, emphasis/strong/strikethrough
- Configurable image backend via `--image-protocol` (`auto`, `halfblocks`, `sixel`, `kitty`, `iterm2`)
- Live reload (`r`) when the file changes on disk
- Start at specific line (`--line`)
- Works with standard terminal keys (arrows, page up/down, home/end)

Notes:
- Local, `file://`, and remote `http://` / `https://` image sources are supported.
- HTML `<img>` `width` and `height` hints are used to reserve image space before lazy loading finishes.
- On macOS, terminal-native image protocols can trigger an "Allow Terminal-Initiated Display?" prompt.
  Use `--image-protocol halfblocks` if you want image rendering without that dialog.
- Fenced code blocks with language tags (for example, a block tagged `rust`) render with syntax highlighting.

## Install

### Homebrew (tap)

```bash
brew tap taf2/tap
brew install mdvi
```

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
mdvi [OPTIONS] <PATH>
```

Examples:

```bash
mdvi docs/spec.md
mdvi --line 120 CHANGELOG.md
mdvi --image-protocol halfblocks README.md
```

## Navigation

- `j` / `Down`: scroll down one line
- `k` / `Up`: scroll up one line
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
