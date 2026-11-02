# Changelog

All notable changes to this project will be documented in this file.

## [Unreleased]

## [0.6.9] - 2026-11-02

### Added
- Configurable content width with `--width 72` or `--width 80` (issue #2).
- Simplified Chinese and Japanese README translations.
- SVG image support (local and remote), rasterized with `resvg`.
- Text-mode image renderer (`--image-protocol symbols`, default when no graphics protocol is
  available): per-cell best fit over sextant, quadrant and eighth-block glyphs, with SVG text
  drawn as real terminal characters.

### Fixed
- Code blocks preserve authored line breaks and scroll horizontally instead of wrapping (issue #2).
- Removed the unsupported GNOME Terminal SIXEL configuration hint.
- Images were downscaled with nearest-neighbour sampling; they are now pre-fitted with Lanczos.
- Images were capped at 18 rows; the cap now scales with the viewport height.
- Partially scrolled images no longer jump to an unscaled crop.
- Cell pixel size is measured from the window size when the terminal doesn't report it
  (e.g. VTE), instead of assuming 10x20.

## [0.6.8] - 2026-11-02

### Fixed
- Kept wide table columns aligned instead of wrapping table rows.
- Added horizontal table scrolling with cursor movement and revealed search matches in off-screen columns.
- Preserved table cursor positions on resize and Unicode character alignment when clipping.

## [0.6.7] - 2026-11-02

### Changed
- Improved word wrapping so scrolling, search, rectangular selection, and inline images stay aligned.
- Made layout and cursor movement Unicode-aware, including wide characters and emoji.
- Improved table alignment and header styling, and preserved blank lines and tab spacing in code blocks.
- Kept the cursor anchored to its text when resizing the terminal.

### Added
- Makefile commands for building, installing, and checking the project.
- A rendering showcase and CI checks on Linux and macOS.

## [0.6.3] - 2026-02-13

### Changed
- Cached search-highlighted lines so unchanged redraws reuse prior highlighted output instead of re-running regex highlighting across the whole document.

## [0.6.2] - 2026-02-13

### Changed
- Moved local image dimension probing off the UI thread during load/reload by sending probe jobs to the background image worker.
- Removed per-frame document line cloning before paragraph rendering by transferring ownership of display lines into `Text`.
- Kept scrolling/input responsive while images load by throttling image-load result processing per UI tick.
- Moved image resize/encode work off the UI thread into a dedicated background resize worker so rendering large images no longer blocks navigation.

## [0.6.1] - 2026-02-13

### Changed
- Cached virtual-row counts in `max_scroll` keyed by content width, with invalidation on reload, search-state changes, and image layout updates to avoid rebuilding the full display document on frequent navigation/redraw paths.

## [0.6.0] - 2026-02-11

### Added
- Less-style half-page navigation aliases: `d` (down) and `u` (up), in addition to `Ctrl-d` / `Ctrl-u` (issue #1).

## [0.5.0] - 2026-02-10

### Added
- Lazy image loading with non-blocking background fetch/decode so markdown opens immediately.

### Changed
- Image layout now reserves space before load using HTML `<img width height>` hints when available.
- Images without explicit dimensions use a default placeholder estimate during lazy loading.
- Improved image rendering stability while scrolling past images by fixing partial-viewport draw artifacts and distortion.
- Long image source captions are truncated in display text to reduce wrap bleed in the viewport.

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
