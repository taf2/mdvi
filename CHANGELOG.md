# Changelog

All notable changes to this project will be documented in this file.

## [Unreleased]

## [0.4.0] - 2026-02-10

### Added
- Visible in-document cursor with `line:column` cursor position in the status bar for easier eye tracking.

## [0.3.0] - 2026-02-10

### Added
- Syntax highlighting for fenced code blocks with recognized language tags.

## [0.2.0] - 2026-02-10

### Added
- Inline terminal image rendering for markdown images and HTML `<img ...>` tags.
- Remote image loading for `http://` and `https://` image URLs.
- `--image-protocol` CLI option to force a specific image backend (`auto`, `halfblocks`, `sixel`, `kitty`, `iterm2`).

### Changed
- Markdown image tokens now include visual fallback caption lines when an image cannot be loaded.
- TUI redraw loop now renders on input/resize state changes instead of a fixed idle cadence to reduce flicker while scrolling.

## [0.1.0] - 2026-02-10

### Added
- Vim-style full-page navigation with `Ctrl-f` (down) and `Ctrl-b` (up).
- Visual highlighting for search matches in the document view, with stronger emphasis on the active match.

### Changed
- Renamed the CLI and package from `mdview` to `mdvi`.
- Updated in-app title/help text and README command examples to use `mdvi`.
- Search matches are rebuilt after file reload so navigation and highlights stay accurate.
