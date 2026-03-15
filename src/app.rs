use std::{
    collections::BTreeMap,
    io::{self, Stdout},
    path::{Path, PathBuf},
    sync::mpsc::{self, Receiver, Sender},
    thread,
    time::Duration,
};

use anyhow::{Context, Result};
use crossterm::{
    event::{self, Event, KeyCode, KeyEvent, KeyEventKind, KeyModifiers},
    execute,
    terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen},
};
use image::ImageReader;
use ratatui::{
    backend::CrosstermBackend,
    layout::{Constraint, Direction, Layout, Rect},
    style::{Modifier, Style},
    text::{Line, Span, Text},
    widgets::{Block, Borders, Clear, Paragraph, Wrap},
    Terminal,
};
use ratatui_image::{
    picker::{Picker, ProtocolType},
    protocol::StatefulProtocol,
    CropOptions, Resize, ResizeEncodeRender,
};
use regex::{Regex, RegexBuilder};
use unicode_width::{UnicodeWidthChar, UnicodeWidthStr};

use crate::{
    renderer::{read_markdown_file, render_markdown, RenderedDoc},
    ImageProtocol,
};

type AppTerminal = Terminal<CrosstermBackend<Stdout>>;
const DEFAULT_IMAGE_HINT_PIXEL_SIZE: (u32, u32) = (900, 500);
const IMAGE_PRELOAD_VIEWPORTS: usize = 2;
const MAX_IMAGE_RESULTS_PER_TICK: usize = 4;
const MAX_CACHED_IMAGE_VARIANTS: usize = 24;

struct App {
    file_path: PathBuf,
    file_content: String,
    doc: RenderedDoc,
    picker: Picker,
    images: Vec<InlineImage>,
    image_index_by_line: BTreeMap<usize, usize>,
    scroll: usize,
    status: String,
    show_help: bool,
    mode: Mode,
    search_query: Option<String>,
    search_regex: Option<Regex>,
    search_matches: Vec<usize>,
    active_match: usize,
    cursor_row: usize,
    cursor_column: usize,
    focused_image: Option<usize>,
    image_loader_tx: Sender<ImageLoadRequest>,
    image_loader_rx: Receiver<ImageLoadResult>,
    image_resize_tx: Sender<ImageResizeRequest>,
    image_resize_rx: Receiver<ImageResizeResult>,
    virtual_row_count_cache: BTreeMap<u16, usize>,
    highlighted_lines_cache: Option<HighlightedLinesCache>,
}

struct InlineImage {
    line_index: usize,
    source: Option<ResolvedImageSource>,
    state: Option<StatefulProtocol>,
    scratch_state: Option<StatefulProtocol>,
    active_variant: Option<ImageResizeVariant>,
    cached_variants: BTreeMap<ImageResizeVariant, StatefulProtocol>,
    resize_pending: bool,
    pending_variant: Option<ImageResizeVariant>,
    has_encoded_state: bool,
    pixel_size: Option<(u32, u32)>,
    hinted_pixel_size: Option<(u32, u32)>,
    load_state: ImageLoadState,
}

struct ImageRenderPlan {
    image_index: usize,
    start_row: usize,
    height: usize,
}

struct DisplayDoc {
    lines: Vec<Line<'static>>,
    image_plans: Vec<ImageRenderPlan>,
    doc_line_for_row: Vec<usize>,
}

struct HighlightedLinesCache {
    query: Option<String>,
    active_match_line: Option<usize>,
    lines: Vec<Line<'static>>,
}

struct ImageVirtualBounds {
    image_index: usize,
    start_row: usize,
    end_row: usize,
}

#[derive(Debug, Clone)]
enum ResolvedImageSource {
    Local(PathBuf),
    Remote(String),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ImageLoadState {
    NotRequested,
    Loading,
    Loaded,
    Failed,
}

#[derive(Debug, Clone)]
struct ImageLoadRequest {
    image_index: usize,
    job: ImageLoadJob,
}

#[derive(Debug)]
struct ImageLoadResult {
    image_index: usize,
    payload: ImageLoadResultPayload,
}

#[derive(Debug, Clone)]
enum ImageLoadJob {
    ProbeLocalDimensions(PathBuf),
    LoadImage(ResolvedImageSource),
}

#[derive(Debug)]
enum ImageLoadResultPayload {
    ProbedDimensions(Option<(u32, u32)>),
    LoadedImage(Option<image::DynamicImage>),
}

struct ImageResizeRequest {
    image_index: usize,
    variant: ImageResizeVariant,
    resize: Resize,
    area: Rect,
    protocol: StatefulProtocol,
}

struct ImageResizeResult {
    image_index: usize,
    variant: ImageResizeVariant,
    protocol: StatefulProtocol,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum ImageResizeMode {
    Fit,
    Crop { clip_top: bool, clip_left: bool },
    Scale,
}

#[derive(Clone, Copy)]
enum WordMotionKind {
    Small,
    Big,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct ImageResizeVariant {
    width: u16,
    height: u16,
    mode: ImageResizeMode,
}

#[derive(Debug, Default)]
enum Mode {
    #[default]
    Normal,
    SearchInput(String),
}

impl App {
    fn new(file_path: PathBuf, start_line: usize, picker: Picker) -> Result<Self> {
        let (image_loader_tx, image_loader_rx) = start_image_loader_thread();
        let (image_resize_tx, image_resize_rx) = start_image_resize_thread();
        let file_content = read_markdown_file(&file_path)?;
        let doc = render_markdown(&file_content)?;
        let images = prepare_images_for_doc(&file_path, &doc);
        let image_index_by_line = images
            .iter()
            .enumerate()
            .map(|(idx, image)| (image.line_index, idx))
            .collect::<BTreeMap<usize, usize>>();

        let scroll = start_line.saturating_sub(1);
        let status = if images.is_empty() {
            format!("{} lines", doc.lines.len())
        } else {
            format!(
                "{} lines, {} images (lazy load)",
                doc.lines.len(),
                images.len()
            )
        };

        let mut app = Self {
            file_path,
            file_content,
            doc,
            picker,
            images,
            image_index_by_line,
            scroll,
            status,
            show_help: false,
            mode: Mode::Normal,
            search_query: None,
            search_regex: None,
            search_matches: Vec::new(),
            active_match: 0,
            cursor_row: scroll,
            cursor_column: 0,
            focused_image: None,
            image_loader_tx,
            image_loader_rx,
            image_resize_tx,
            image_resize_rx,
            virtual_row_count_cache: BTreeMap::new(),
            highlighted_lines_cache: None,
        };
        app.request_local_dimension_probes();
        Ok(app)
    }

    fn reload(&mut self) {
        match read_markdown_file(&self.file_path).and_then(|content| {
            let doc = render_markdown(&content)?;
            Ok((content, doc))
        }) {
            Ok((content, doc)) => {
                let (image_loader_tx, image_loader_rx) = start_image_loader_thread();
                let (image_resize_tx, image_resize_rx) = start_image_resize_thread();
                self.file_content = content;
                self.doc = doc;
                self.images = prepare_images_for_doc(&self.file_path, &self.doc);
                self.image_loader_tx = image_loader_tx;
                self.image_loader_rx = image_loader_rx;
                self.image_resize_tx = image_resize_tx;
                self.image_resize_rx = image_resize_rx;
                self.image_index_by_line = self
                    .images
                    .iter()
                    .enumerate()
                    .map(|(idx, image)| (image.line_index, idx))
                    .collect::<BTreeMap<usize, usize>>();
                self.focused_image = None;
                self.invalidate_highlight_cache();
                self.request_local_dimension_probes();
                if self.search_regex.is_some() {
                    self.rebuild_search_matches();
                }
                self.invalidate_virtual_row_cache();
                self.status = if self.images.is_empty() {
                    format!("reloaded {}", self.file_path.display())
                } else {
                    format!(
                        "reloaded {} ({} images, lazy load)",
                        self.file_path.display(),
                        self.images.len()
                    )
                };
            }
            Err(err) => {
                self.status = format!("reload failed: {err}");
            }
        }
    }

    fn invalidate_virtual_row_cache(&mut self) {
        self.virtual_row_count_cache.clear();
    }

    fn invalidate_highlight_cache(&mut self) {
        self.highlighted_lines_cache = None;
    }

    fn total_virtual_lines(&mut self, content_width: u16) -> usize {
        if let Some(cached_rows) = self.virtual_row_count_cache.get(&content_width).copied() {
            return cached_rows;
        }

        let rows = self.build_display_doc(content_width).lines.len().max(1);
        self.virtual_row_count_cache.insert(content_width, rows);
        rows
    }

    fn max_scroll(&mut self, viewport_height: usize, content_width: u16) -> usize {
        self.total_virtual_lines(content_width)
            .saturating_sub(viewport_height.saturating_sub(1))
    }

    fn clamp_cursor_row(&mut self, content_width: u16) {
        let total_rows = self.total_virtual_lines(content_width).max(1);
        self.cursor_row = self.cursor_row.min(total_rows.saturating_sub(1));
    }

    fn ensure_cursor_visible(&mut self, viewport_height: usize, content_width: u16) {
        self.clamp_cursor_row(content_width);
        let max_scroll = self.max_scroll(viewport_height, content_width);
        if viewport_height == 0 {
            self.scroll = self.cursor_row.min(max_scroll);
            return;
        }

        if self.cursor_row < self.scroll {
            self.scroll = self.cursor_row;
        } else {
            let viewport_bottom = self
                .scroll
                .saturating_add(viewport_height.saturating_sub(1));
            if self.cursor_row > viewport_bottom {
                self.scroll = self
                    .cursor_row
                    .saturating_sub(viewport_height.saturating_sub(1));
            }
        }

        self.scroll = self.scroll.min(max_scroll);
    }

    fn scroll_down(&mut self, n: usize, viewport_height: usize, content_width: u16) {
        self.focused_image = None;
        self.clamp_cursor_row(content_width);
        let total_rows = self.total_virtual_lines(content_width).max(1);
        let max_scroll = self.max_scroll(viewport_height, content_width);
        let next_scroll = self.scroll.saturating_add(n).min(max_scroll);
        let delta = next_scroll.saturating_sub(self.scroll);
        self.scroll = next_scroll;
        self.cursor_row = self
            .cursor_row
            .saturating_add(delta)
            .min(total_rows.saturating_sub(1));
    }

    fn scroll_up(&mut self, n: usize, _viewport_height: usize, content_width: u16) {
        self.focused_image = None;
        self.clamp_cursor_row(content_width);
        let next_scroll = self.scroll.saturating_sub(n);
        let delta = self.scroll.saturating_sub(next_scroll);
        self.scroll = next_scroll;
        self.cursor_row = self.cursor_row.saturating_sub(delta);
    }

    fn jump_top(&mut self) {
        self.focused_image = None;
        self.cursor_row = 0;
        self.scroll = 0;
    }

    fn jump_bottom(&mut self, viewport_height: usize, content_width: u16) {
        self.focused_image = None;
        let total_rows = self.total_virtual_lines(content_width).max(1);
        self.cursor_row = total_rows.saturating_sub(1);
        self.ensure_cursor_visible(viewport_height, content_width);
    }

    fn cursor_doc_line_for_navigation(&mut self, content_width: u16) -> usize {
        self.clamp_cursor_row(content_width);
        self.build_display_doc(content_width)
            .doc_line_for_row
            .get(self.cursor_row)
            .copied()
            .unwrap_or_else(|| self.doc.lines.len().saturating_sub(1))
    }

    fn image_virtual_bounds(&self, content_width: u16) -> Vec<ImageVirtualBounds> {
        let mut bounds = Vec::new();
        let mut accumulated_image_rows = 0usize;

        for (image_index, image) in self.images.iter().enumerate() {
            let height = self.image_height_for(image, content_width);
            if height == 0 {
                continue;
            }

            let start_row = image
                .line_index
                .saturating_add(1)
                .saturating_add(accumulated_image_rows);
            let end_row = start_row.saturating_add(height);
            bounds.push(ImageVirtualBounds {
                image_index,
                start_row,
                end_row,
            });
            accumulated_image_rows = accumulated_image_rows.saturating_add(height);
        }

        bounds
    }

    fn image_bounds_for(
        &self,
        image_index: usize,
        content_width: u16,
    ) -> Option<ImageVirtualBounds> {
        self.image_virtual_bounds(content_width)
            .into_iter()
            .find(|bounds| bounds.image_index == image_index)
    }

    fn image_at_virtual_row(&self, row: usize, content_width: u16) -> Option<usize> {
        self.image_virtual_bounds(content_width)
            .into_iter()
            .find(|bounds| row >= bounds.start_row && row < bounds.end_row)
            .map(|bounds| bounds.image_index)
    }

    fn scroll_down_with_image_focus(&mut self, viewport_height: usize, content_width: u16) {
        self.clamp_cursor_row(content_width);
        let total_rows = self.total_virtual_lines(content_width).max(1);
        if self.cursor_row >= total_rows.saturating_sub(1) {
            return;
        }

        let current_image = self.image_at_virtual_row(self.cursor_row, content_width);
        if let (Some(focused_image), Some(current_image_index)) =
            (self.focused_image, current_image)
        {
            if focused_image == current_image_index {
                if let Some(bounds) = self.image_bounds_for(current_image_index, content_width) {
                    self.cursor_row = bounds.end_row.min(total_rows.saturating_sub(1));
                    self.ensure_cursor_visible(viewport_height, content_width);
                    self.focused_image = None;
                    return;
                }
            }
        }

        self.cursor_row = self
            .cursor_row
            .saturating_add(1)
            .min(total_rows.saturating_sub(1));
        self.ensure_cursor_visible(viewport_height, content_width);
        self.focused_image = self.image_at_virtual_row(self.cursor_row, content_width);
    }

    fn scroll_up_with_image_focus(&mut self, viewport_height: usize, content_width: u16) {
        self.clamp_cursor_row(content_width);
        if self.cursor_row == 0 {
            return;
        }

        let current_image = self.image_at_virtual_row(self.cursor_row, content_width);
        if let (Some(focused_image), Some(current_image_index)) =
            (self.focused_image, current_image)
        {
            if focused_image == current_image_index {
                if let Some(bounds) = self.image_bounds_for(current_image_index, content_width) {
                    self.cursor_row = bounds.start_row.saturating_sub(1);
                    self.ensure_cursor_visible(viewport_height, content_width);
                    self.focused_image = None;
                    return;
                }
            }
        }

        self.cursor_row = self.cursor_row.saturating_sub(1);
        self.ensure_cursor_visible(viewport_height, content_width);
        self.focused_image = self.image_at_virtual_row(self.cursor_row, content_width);
    }

    fn line_text_at(&self, index: usize) -> String {
        self.doc
            .lines
            .get(index)
            .map(|line| {
                line.spans
                    .iter()
                    .map(|span| span.content.as_ref())
                    .collect::<String>()
            })
            .unwrap_or_default()
    }

    fn line_width_at(&self, index: usize) -> usize {
        self.line_text_at(index).width()
    }

    fn max_cursor_column_for_line(&self, index: usize) -> usize {
        self.line_width_at(index).saturating_sub(1)
    }

    fn effective_cursor_column_for_line(&self, index: usize) -> usize {
        self.cursor_column
            .min(self.max_cursor_column_for_line(index))
    }

    fn move_cursor_left(&mut self) {
        self.cursor_column = self.cursor_column.saturating_sub(1);
    }

    fn move_cursor_right(&mut self, line_index: usize) {
        let max_column = self.max_cursor_column_for_line(line_index);
        self.cursor_column = self.cursor_column.saturating_add(1).min(max_column);
    }

    fn byte_index_for_column(text: &str, target_column: usize) -> usize {
        let mut column = 0usize;
        for (idx, ch) in text.char_indices() {
            let width = ch.width().unwrap_or(0).max(1);
            if column >= target_column {
                return idx;
            }
            if column + width > target_column {
                return idx;
            }
            column += width;
        }
        text.len()
    }

    fn column_for_byte_index(text: &str, target_index: usize) -> usize {
        let mut column = 0usize;
        for (idx, ch) in text.char_indices() {
            if idx >= target_index {
                break;
            }
            column += ch.width().unwrap_or(0).max(1);
        }
        column
    }

    fn classify_motion_char(ch: char, kind: WordMotionKind) -> u8 {
        if ch.is_whitespace() {
            0
        } else if matches!(kind, WordMotionKind::Small) && (ch.is_alphanumeric() || ch == '_') {
            1
        } else {
            2
        }
    }

    fn find_next_word_start(text: &str, column: usize, kind: WordMotionKind) -> Option<usize> {
        let chars = text.char_indices().collect::<Vec<_>>();
        if chars.is_empty() {
            return None;
        }

        let mut idx = chars
            .partition_point(|(byte_idx, _)| *byte_idx < Self::byte_index_for_column(text, column));
        if idx >= chars.len() {
            return None;
        }

        let current_class = Self::classify_motion_char(chars[idx].1, kind);
        if current_class == 0 {
            while idx < chars.len() && Self::classify_motion_char(chars[idx].1, kind) == 0 {
                idx += 1;
            }
        } else {
            while idx < chars.len()
                && Self::classify_motion_char(chars[idx].1, kind) == current_class
            {
                idx += 1;
            }
            while idx < chars.len() && Self::classify_motion_char(chars[idx].1, kind) == 0 {
                idx += 1;
            }
        }

        chars
            .get(idx)
            .map(|(byte_idx, _)| Self::column_for_byte_index(text, *byte_idx))
    }

    fn find_prev_word_start(text: &str, column: usize, kind: WordMotionKind) -> Option<usize> {
        let chars = text.char_indices().collect::<Vec<_>>();
        if chars.is_empty() {
            return None;
        }

        let byte_index = Self::byte_index_for_column(text, column);
        let mut idx = chars.partition_point(|(char_idx, _)| *char_idx < byte_index);
        idx = idx.saturating_sub(1);

        while idx > 0 && Self::classify_motion_char(chars[idx].1, kind) == 0 {
            idx -= 1;
        }
        if idx == 0 && Self::classify_motion_char(chars[idx].1, kind) == 0 {
            return None;
        }

        let current_class = Self::classify_motion_char(chars[idx].1, kind);
        while idx > 0 && Self::classify_motion_char(chars[idx - 1].1, kind) == current_class {
            idx -= 1;
        }

        Some(Self::column_for_byte_index(text, chars[idx].0))
    }

    fn move_to_next_word(&mut self, content_width: u16, kind: WordMotionKind) {
        let start_line = self.cursor_doc_line_for_navigation(content_width);
        let start_column = self.effective_cursor_column_for_line(start_line);

        for line_index in start_line..self.doc.lines.len() {
            let text = self.line_text_at(line_index);
            let column = if line_index == start_line {
                start_column
            } else {
                0
            };
            if let Some(next_column) = Self::find_next_word_start(&text, column, kind) {
                self.cursor_column = next_column;
                self.cursor_row = self.virtual_row_for_doc_line(line_index, content_width);
                self.focused_image = None;
                return;
            }
        }
    }

    fn move_to_prev_word(&mut self, content_width: u16, kind: WordMotionKind) {
        let start_line = self.cursor_doc_line_for_navigation(content_width);
        let start_column = self.effective_cursor_column_for_line(start_line);

        for line_index in (0..=start_line).rev() {
            let text = self.line_text_at(line_index);
            let column = if line_index == start_line {
                start_column
            } else {
                text.width()
            };
            if let Some(prev_column) = Self::find_prev_word_start(&text, column, kind) {
                self.cursor_column = prev_column;
                self.cursor_row = self.virtual_row_for_doc_line(line_index, content_width);
                self.focused_image = None;
                return;
            }
        }
    }

    fn search(&mut self, query: &str, viewport_height: usize, content_width: u16) {
        if query.is_empty() {
            self.search_query = None;
            self.search_regex = None;
            self.search_matches.clear();
            self.active_match = 0;
            self.focused_image = None;
            self.invalidate_highlight_cache();
            self.invalidate_virtual_row_cache();
            self.status = "search cleared".to_string();
            return;
        }

        let regex = match RegexBuilder::new(&regex::escape(query))
            .case_insensitive(true)
            .build()
        {
            Ok(regex) => regex,
            Err(err) => {
                self.status = format!("invalid search: {err}");
                return;
            }
        };

        self.search_query = Some(query.to_string());
        self.search_regex = Some(regex);
        self.rebuild_search_matches();
        self.focused_image = None;
        self.invalidate_highlight_cache();
        self.invalidate_virtual_row_cache();

        if self.search_matches.is_empty() {
            self.status = format!("no matches for /{query}");
            return;
        }

        let target_row = self.virtual_row_for_doc_line(self.search_matches[0], content_width);
        self.cursor_row = target_row;
        let max_scroll = self.max_scroll(viewport_height, content_width);
        self.scroll = target_row.min(max_scroll);
        self.status = format!("{} matches for /{query}", self.search_matches.len());
    }

    fn jump_next_match(&mut self, viewport_height: usize, content_width: u16) {
        if self.search_matches.is_empty() {
            self.status = "no active search results".to_string();
            return;
        }
        self.active_match = (self.active_match + 1) % self.search_matches.len();
        self.focused_image = None;
        self.invalidate_highlight_cache();
        let target_row =
            self.virtual_row_for_doc_line(self.search_matches[self.active_match], content_width);
        self.cursor_row = target_row;
        let max_scroll = self.max_scroll(viewport_height, content_width);
        self.scroll = target_row.min(max_scroll);
    }

    fn jump_prev_match(&mut self, viewport_height: usize, content_width: u16) {
        if self.search_matches.is_empty() {
            self.status = "no active search results".to_string();
            return;
        }
        if self.active_match == 0 {
            self.active_match = self.search_matches.len() - 1;
        } else {
            self.active_match -= 1;
        }
        self.focused_image = None;
        self.invalidate_highlight_cache();
        let target_row =
            self.virtual_row_for_doc_line(self.search_matches[self.active_match], content_width);
        self.cursor_row = target_row;
        let max_scroll = self.max_scroll(viewport_height, content_width);
        self.scroll = target_row.min(max_scroll);
    }

    fn active_match_line(&self) -> Option<usize> {
        self.search_matches.get(self.active_match).copied()
    }

    fn highlighted_lines(&mut self) -> Vec<Line<'static>> {
        let Some(regex) = self.search_regex.as_ref() else {
            return self.doc.lines.clone();
        };
        let active_match_line = self.active_match_line();
        let search_query = self.search_query.as_deref();

        if let Some(cache) = self.highlighted_lines_cache.as_ref() {
            if cache.query.as_deref() == search_query
                && cache.active_match_line == active_match_line
            {
                return cache.lines.clone();
            }
        }

        let lines = highlight_lines(&self.doc.lines, regex, active_match_line);
        self.highlighted_lines_cache = Some(HighlightedLinesCache {
            query: search_query.map(ToString::to_string),
            active_match_line,
            lines: lines.clone(),
        });

        lines
    }

    fn rebuild_search_matches(&mut self) {
        self.search_matches.clear();
        self.active_match = 0;
        self.focused_image = None;
        self.invalidate_highlight_cache();

        let Some(regex) = self.search_regex.as_ref() else {
            return;
        };

        for (idx, _) in self.doc.lines.iter().enumerate() {
            if regex.is_match(&self.line_text_at(idx)) {
                self.search_matches.push(idx);
            }
        }
    }

    fn virtual_row_for_doc_line(&self, line_index: usize, content_width: u16) -> usize {
        let mut row = line_index;
        for image in &self.images {
            if image.line_index < line_index {
                row += self.image_height_for(image, content_width);
            }
        }
        row
    }

    fn effective_image_pixel_size(&self, image: &InlineImage) -> Option<(u32, u32)> {
        if let Some(pixel_size) = image.pixel_size {
            return Some(pixel_size);
        }
        if let Some(hinted_pixel_size) = image.hinted_pixel_size {
            return Some(hinted_pixel_size);
        }
        if image.load_state == ImageLoadState::Failed {
            return None;
        }
        Some(DEFAULT_IMAGE_HINT_PIXEL_SIZE)
    }

    fn image_height_for(&self, image: &InlineImage, content_width: u16) -> usize {
        const MAX_IMAGE_CELL_HEIGHT: usize = 18;

        let Some((pixel_width, pixel_height)) = self.effective_image_pixel_size(image) else {
            return 0;
        };
        if pixel_width == 0 || pixel_height == 0 || content_width == 0 {
            return 0;
        }

        let (font_width_cells, font_height_cells) = self.picker.font_size();
        let font_width = u32::from(font_width_cells.max(1));
        let font_height = u32::from(font_height_cells.max(1));

        let max_pixel_width = u32::from(content_width) * font_width;
        let target_pixel_width = pixel_width.min(max_pixel_width).max(1);
        let target_pixel_height = (u64::from(pixel_height) * u64::from(target_pixel_width))
            .div_ceil(u64::from(pixel_width)) as u32;
        let height_cells = target_pixel_height.div_ceil(font_height).max(1) as usize;

        height_cells.min(MAX_IMAGE_CELL_HEIGHT)
    }

    fn image_progress_counts(&self) -> (usize, usize, usize) {
        let mut loaded = 0usize;
        let mut loading = 0usize;
        let mut failed = 0usize;

        for image in &self.images {
            match image.load_state {
                ImageLoadState::Loaded => loaded += 1,
                ImageLoadState::Loading => loading += 1,
                ImageLoadState::Failed => failed += 1,
                ImageLoadState::NotRequested => {}
            }
        }

        (loaded, loading, failed)
    }

    fn request_image_load(&mut self, image_index: usize) {
        let Some(image) = self.images.get_mut(image_index) else {
            return;
        };
        if image.load_state != ImageLoadState::NotRequested {
            return;
        }

        let Some(source) = image.source.clone() else {
            image.resize_pending = false;
            image.pending_variant = None;
            image.active_variant = None;
            image.cached_variants.clear();
            image.scratch_state = None;
            image.state = None;
            image.load_state = ImageLoadState::Failed;
            return;
        };

        if self
            .image_loader_tx
            .send(ImageLoadRequest {
                image_index,
                job: ImageLoadJob::LoadImage(source),
            })
            .is_ok()
        {
            image.resize_pending = false;
            image.pending_variant = None;
            image.active_variant = None;
            image.cached_variants.clear();
            image.scratch_state = None;
            image.state = None;
            image.has_encoded_state = false;
            image.load_state = ImageLoadState::Loading;
        } else {
            image.resize_pending = false;
            image.pending_variant = None;
            image.active_variant = None;
            image.cached_variants.clear();
            image.scratch_state = None;
            image.state = None;
            image.has_encoded_state = false;
            image.load_state = ImageLoadState::Failed;
        }
    }

    fn request_local_dimension_probes(&mut self) {
        let mut probe_requests: Vec<(usize, PathBuf)> = Vec::new();

        for (image_index, image) in self.images.iter().enumerate() {
            if image.hinted_pixel_size.is_some() {
                continue;
            }
            let Some(ResolvedImageSource::Local(path)) = image.source.as_ref() else {
                continue;
            };
            probe_requests.push((image_index, path.clone()));
        }

        for (image_index, path) in probe_requests {
            let _ = self.image_loader_tx.send(ImageLoadRequest {
                image_index,
                job: ImageLoadJob::ProbeLocalDimensions(path),
            });
        }
    }

    fn request_images_near_viewport(&mut self, display_doc: &DisplayDoc, viewport_height: usize) {
        let preload_rows = viewport_height.saturating_mul(IMAGE_PRELOAD_VIEWPORTS);
        let preload_top = self.scroll.saturating_sub(preload_rows);
        let preload_bottom = self
            .scroll
            .saturating_add(viewport_height)
            .saturating_add(preload_rows);

        for plan in &display_doc.image_plans {
            let end_row = plan.start_row.saturating_add(plan.height);
            if end_row < preload_top || plan.start_row > preload_bottom {
                continue;
            }
            self.request_image_load(plan.image_index);
        }
    }

    fn image_resize_variant(resize: &Resize, target_area: Rect) -> ImageResizeVariant {
        let mode = match resize {
            Resize::Fit(_) => ImageResizeMode::Fit,
            Resize::Crop(options) => {
                let options = options.unwrap_or(CropOptions {
                    clip_top: false,
                    clip_left: false,
                });
                ImageResizeMode::Crop {
                    clip_top: options.clip_top,
                    clip_left: options.clip_left,
                }
            }
            Resize::Scale(_) => ImageResizeMode::Scale,
        };

        ImageResizeVariant {
            width: target_area.width,
            height: target_area.height,
            mode,
        }
    }

    fn insert_cached_variant(
        image: &mut InlineImage,
        variant: ImageResizeVariant,
        protocol: StatefulProtocol,
    ) {
        image.cached_variants.insert(variant, protocol);
        while image.cached_variants.len() > MAX_CACHED_IMAGE_VARIANTS {
            let Some(evict_variant) = image.cached_variants.keys().next().copied() else {
                break;
            };
            image.cached_variants.remove(&evict_variant);
        }
    }

    fn activate_cached_variant(image: &mut InlineImage, variant: ImageResizeVariant) -> bool {
        let Some(next_protocol) = image.cached_variants.remove(&variant) else {
            return false;
        };

        let previous_protocol = image.state.replace(next_protocol);
        let previous_variant = image.active_variant;
        image.active_variant = Some(variant);
        image.has_encoded_state = true;

        if let Some(previous_protocol) = previous_protocol {
            if let Some(previous_variant) = previous_variant {
                Self::insert_cached_variant(image, previous_variant, previous_protocol);
            } else if image.scratch_state.is_none() {
                image.scratch_state = Some(previous_protocol);
            }
        }

        true
    }

    fn requested_variant(
        &self,
        image_index: usize,
        area: Rect,
        resize: &Resize,
    ) -> Option<ImageResizeVariant> {
        let image = self.images.get(image_index)?;
        let reference_protocol = image
            .state
            .as_ref()
            .or_else(|| image.scratch_state.as_ref())
            .or_else(|| image.cached_variants.values().next())?;

        let target_area = reference_protocol.size_for(resize.clone(), area);
        if target_area.width == 0 || target_area.height == 0 {
            return None;
        }

        Some(Self::image_resize_variant(resize, target_area))
    }

    fn request_image_resize(&mut self, image_index: usize, area: Rect, resize: Resize) {
        let Some(image) = self.images.get_mut(image_index) else {
            return;
        };
        let Some(reference_protocol) = image
            .state
            .as_ref()
            .or_else(|| image.scratch_state.as_ref())
            .or_else(|| image.cached_variants.values().next())
        else {
            return;
        };
        let target_area = reference_protocol.size_for(resize.clone(), area);
        if target_area.width == 0 || target_area.height == 0 {
            return;
        }
        let variant = Self::image_resize_variant(&resize, target_area);

        if image.state.is_some() && image.active_variant == Some(variant) {
            return;
        }
        if Self::activate_cached_variant(image, variant) {
            return;
        }
        if image.pending_variant == Some(variant) || image.resize_pending {
            return;
        }

        let protocol = if let Some(protocol) = image.scratch_state.take() {
            Some(protocol)
        } else if let Some(cached_variant) = image.cached_variants.keys().next().copied() {
            image.cached_variants.remove(&cached_variant)
        } else if image.active_variant.is_none() {
            image.state.take()
        } else {
            None
        };
        let Some(protocol) = protocol else {
            return;
        };

        match self.image_resize_tx.send(ImageResizeRequest {
            image_index,
            variant,
            resize,
            area: target_area,
            protocol,
        }) {
            Ok(()) => {
                image.resize_pending = true;
                image.pending_variant = Some(variant);
            }
            Err(err) => {
                if image.scratch_state.is_none() {
                    image.scratch_state = Some(err.0.protocol);
                } else if image.state.is_none() {
                    image.state = Some(err.0.protocol);
                }
                image.resize_pending = false;
                image.pending_variant = None;
            }
        }
    }

    fn drain_image_resize_results(&mut self) -> bool {
        let mut changed = false;
        while let Ok(result) = self.image_resize_rx.try_recv() {
            let Some(image) = self.images.get_mut(result.image_index) else {
                continue;
            };
            let was_pending_variant = image.pending_variant == Some(result.variant);
            image.resize_pending = false;
            image.pending_variant = None;
            image.has_encoded_state = true;

            if was_pending_variant || image.active_variant.is_none() {
                let previous_protocol = image.state.replace(result.protocol);
                let previous_variant = image.active_variant;
                image.active_variant = Some(result.variant);

                if let Some(previous_protocol) = previous_protocol {
                    if let Some(previous_variant) = previous_variant {
                        Self::insert_cached_variant(image, previous_variant, previous_protocol);
                    } else if image.scratch_state.is_none() {
                        image.scratch_state = Some(previous_protocol);
                    }
                }
            } else {
                Self::insert_cached_variant(image, result.variant, result.protocol);
            }
            changed = true;
        }
        changed
    }

    fn image_placeholder(image: &InlineImage) -> &'static str {
        if image.resize_pending {
            return "[rendering image...]";
        }

        match image.load_state {
            ImageLoadState::Loading => "[loading image...]",
            ImageLoadState::Failed => "[image unavailable]",
            ImageLoadState::NotRequested => "[image pending]",
            ImageLoadState::Loaded => "[rendering image...]",
        }
    }

    fn drain_image_results(&mut self) -> bool {
        let mut changed = false;
        let mut processed = 0usize;
        while processed < MAX_IMAGE_RESULTS_PER_TICK {
            let Ok(result) = self.image_loader_rx.try_recv() else {
                break;
            };
            let Some(image) = self.images.get_mut(result.image_index) else {
                processed += 1;
                continue;
            };

            match result.payload {
                ImageLoadResultPayload::LoadedImage(Some(dynamic_image)) => {
                    image.pixel_size = Some((dynamic_image.width(), dynamic_image.height()));
                    image.state = Some(self.picker.new_resize_protocol(dynamic_image.clone()));
                    image.scratch_state = Some(self.picker.new_resize_protocol(dynamic_image));
                    image.active_variant = None;
                    image.cached_variants.clear();
                    image.resize_pending = false;
                    image.pending_variant = None;
                    image.has_encoded_state = false;
                    image.load_state = ImageLoadState::Loaded;
                    changed = true;
                }
                ImageLoadResultPayload::LoadedImage(None) => {
                    image.state = None;
                    image.scratch_state = None;
                    image.active_variant = None;
                    image.cached_variants.clear();
                    image.resize_pending = false;
                    image.pending_variant = None;
                    image.has_encoded_state = false;
                    image.load_state = ImageLoadState::Failed;
                    changed = true;
                }
                ImageLoadResultPayload::ProbedDimensions(Some(dimensions)) => {
                    if image.pixel_size.is_none() && image.hinted_pixel_size.is_none() {
                        image.hinted_pixel_size = Some(dimensions);
                        changed = true;
                    }
                }
                ImageLoadResultPayload::ProbedDimensions(None) => {}
            }
            processed += 1;
        }
        if changed {
            self.invalidate_virtual_row_cache();
        }
        changed
    }

    fn build_display_doc(&mut self, content_width: u16) -> DisplayDoc {
        let source_lines = self.highlighted_lines();
        let mut lines: Vec<Line<'static>> = Vec::with_capacity(source_lines.len());
        let mut image_plans = Vec::new();
        let mut doc_line_for_row: Vec<usize> = Vec::with_capacity(source_lines.len());

        for (line_index, line) in source_lines.into_iter().enumerate() {
            lines.push(line);
            doc_line_for_row.push(line_index);

            if let Some(image_index) = self.image_index_by_line.get(&line_index).copied() {
                let height = self.image_height_for(&self.images[image_index], content_width);
                if height > 0 {
                    let start_row = lines.len();
                    for _ in 0..height {
                        lines.push(Line::default());
                        doc_line_for_row.push(line_index);
                    }
                    image_plans.push(ImageRenderPlan {
                        image_index,
                        start_row,
                        height,
                    });
                }
            }
        }

        DisplayDoc {
            lines,
            image_plans,
            doc_line_for_row,
        }
    }
}

struct TuiGuard {
    terminal: AppTerminal,
}

impl TuiGuard {
    fn setup() -> Result<Self> {
        enable_raw_mode().context("failed to enable raw mode")?;
        let mut stdout = io::stdout();
        execute!(stdout, EnterAlternateScreen).context("failed to enter alternate screen")?;

        let backend = CrosstermBackend::new(stdout);
        let terminal = Terminal::new(backend).context("failed to initialize terminal")?;

        Ok(Self { terminal })
    }

    fn terminal_mut(&mut self) -> &mut AppTerminal {
        &mut self.terminal
    }
}

impl Drop for TuiGuard {
    fn drop(&mut self) {
        let _ = disable_raw_mode();
        let _ = execute!(self.terminal.backend_mut(), LeaveAlternateScreen);
        let _ = self.terminal.show_cursor();
    }
}

pub fn run(file_path: PathBuf, start_line: usize, image_protocol: ImageProtocol) -> Result<()> {
    let mut tui = TuiGuard::setup()?;
    let mut picker = Picker::from_query_stdio().unwrap_or_else(|_| Picker::from_fontsize((10, 20)));
    if let Some(protocol_type) = protocol_override(image_protocol) {
        picker.set_protocol_type(protocol_type);
    }
    let mut app = App::new(file_path, start_line, picker)?;
    let mut should_redraw = true;

    loop {
        if app.drain_image_results() {
            should_redraw = true;
        }
        if app.drain_image_resize_results() {
            should_redraw = true;
        }

        if should_redraw {
            let terminal = tui.terminal_mut();
            terminal.draw(|frame| {
                let size = frame.area();

                let chunks = Layout::default()
                    .direction(Direction::Vertical)
                    .constraints([Constraint::Min(1), Constraint::Length(1)])
                    .split(size);

                let title = format!(" mdvi {} ", app.file_path.display());
                let content_block = Block::default().borders(Borders::ALL).title(title);
                let content_inner = content_block.inner(chunks[0]);
                let content_width = content_inner.width;
                let viewport_height = usize::from(content_inner.height);
                let mut display_doc = app.build_display_doc(content_width);
                let total_rows = display_doc.lines.len().max(1);
                let total_doc_lines = app.doc.lines.len().max(1);
                let max_scroll = app.max_scroll(viewport_height, content_width);
                app.clamp_cursor_row(content_width);
                app.ensure_cursor_visible(viewport_height, content_width);
                app.scroll = app.scroll.min(max_scroll);
                app.request_images_near_viewport(&display_doc, viewport_height);

                let cursor_virtual_row = app.cursor_row.min(total_rows.saturating_sub(1));
                let cursor_doc_line = display_doc
                    .doc_line_for_row
                    .get(cursor_virtual_row)
                    .copied()
                    .unwrap_or_else(|| app.doc.lines.len().saturating_sub(1));
                let cursor_screen_row = cursor_virtual_row.saturating_sub(app.scroll);
                let cursor_column = app.effective_cursor_column_for_line(cursor_doc_line);

                let text = Text::from(std::mem::take(&mut display_doc.lines));
                let paragraph = Paragraph::new(text)
                    .block(content_block)
                    .wrap(Wrap { trim: false })
                    .scroll((app.scroll as u16, 0));

                frame.render_widget(paragraph, chunks[0]);

                for plan in &display_doc.image_plans {
                    let plan_top = plan.start_row;
                    let plan_bottom = plan.start_row.saturating_add(plan.height);
                    let view_top = app.scroll;
                    let view_bottom = app.scroll.saturating_add(viewport_height);

                    if plan_bottom <= view_top || plan_top >= view_bottom {
                        continue;
                    }
                    let visible_top = plan_top.max(view_top);
                    let visible_bottom = plan_bottom.min(view_bottom);
                    let visible_height = visible_bottom.saturating_sub(visible_top);
                    if visible_height == 0 {
                        continue;
                    }
                    let is_fully_visible = plan_top >= view_top && plan_bottom <= view_bottom;
                    let image_y = content_inner
                        .y
                        .saturating_add((visible_top - view_top) as u16);
                    let image_area = Rect::new(
                        content_inner.x,
                        image_y,
                        content_inner.width,
                        visible_height as u16,
                    );
                    if image_area.width == 0 || image_area.height == 0 {
                        continue;
                    }
                    frame.render_widget(Clear, image_area);
                    let resize = if is_fully_visible {
                        Resize::Fit(None)
                    } else {
                        Resize::Crop(Some(CropOptions {
                            clip_top: visible_top > plan_top,
                            clip_left: false,
                        }))
                    };
                    let requested_variant =
                        app.requested_variant(plan.image_index, image_area, &resize);
                    app.request_image_resize(plan.image_index, image_area, resize);
                    let has_requested_variant = requested_variant
                        .is_some_and(|variant| app.images[plan.image_index].active_variant == Some(variant));

                    if has_requested_variant {
                        if let Some(image_state) = app.images[plan.image_index].state.as_mut() {
                            image_state.render(image_area, frame.buffer_mut());
                        } else {
                            let placeholder = App::image_placeholder(&app.images[plan.image_index]);
                            frame.render_widget(
                                Paragraph::new(Line::from(Span::styled(
                                    format!("  {placeholder}"),
                                    Style::default().add_modifier(Modifier::DIM),
                                ))),
                                image_area,
                            );
                        }
                    } else {
                        let placeholder = App::image_placeholder(&app.images[plan.image_index]);
                        frame.render_widget(
                            Paragraph::new(Line::from(Span::styled(
                                format!("  {placeholder}"),
                                Style::default().add_modifier(Modifier::DIM),
                            ))),
                            image_area,
                        );
                    }

                    if app.focused_image == Some(plan.image_index) {
                        frame.render_widget(
                            Block::default()
                                .borders(Borders::ALL)
                                .border_style(Style::default().add_modifier(Modifier::BOLD)),
                            image_area,
                        );
                    }
                }

                let status = if app.show_help {
                    "q quit | h/l,j/k move | w/W,b/B words | arrows move | g/G top/bottom | d/u,Ctrl-d/u half-page | Ctrl-f/b page | / search | n/N next/prev | r reload | ? help"
                        .to_string()
                } else {
                    match &app.mode {
                        Mode::Normal => {
                            let (loaded_images, loading_images, failed_images) =
                                app.image_progress_counts();
                            let mut right = format!(
                                "{}  |  row {} / {}  |  cursor {}:{}  |  ? help",
                                app.status,
                                app.scroll.saturating_add(1).min(total_rows),
                                total_rows,
                                cursor_doc_line.saturating_add(1).min(total_doc_lines),
                                cursor_column.saturating_add(1)
                            );
                            if !app.images.is_empty() {
                                right.push_str(&format!(
                                    "  |  images {loaded_images}/{}, loading {loading_images}, failed {failed_images}",
                                    app.images.len()
                                ));
                            }
                            if let Some(focused_image) = app.focused_image {
                                right.push_str(&format!(
                                    "  |  image {} selected",
                                    focused_image.saturating_add(1)
                                ));
                            }
                            if !app.search_matches.is_empty() {
                                right.push_str(&format!(
                                    "  |  match {} / {}",
                                    app.active_match + 1,
                                    app.search_matches.len()
                                ));
                            }
                            right
                        }
                        Mode::SearchInput(query) => format!("/{query}"),
                    }
                };

                let status_line = Line::from(vec![Span::styled(
                    status,
                    Style::default().add_modifier(Modifier::DIM),
                )]);
                frame.render_widget(Paragraph::new(status_line), chunks[1]);

                match &app.mode {
                    Mode::SearchInput(query) => {
                        let max_x = chunks[1]
                            .x
                            .saturating_add(chunks[1].width.saturating_sub(1));
                        let cursor_x = chunks[1]
                            .x
                            .saturating_add(1 + query.as_str().width() as u16)
                            .min(max_x);
                        frame.set_cursor_position((cursor_x, chunks[1].y));
                    }
                    Mode::Normal => {
                        if viewport_height > 0 && content_inner.width > 0 {
                            let cursor_y =
                                content_inner.y.saturating_add(cursor_screen_row as u16);
                            let cursor_x = content_inner
                                .x
                                .saturating_add(cursor_column as u16)
                                .min(content_inner.x + content_inner.width.saturating_sub(1));
                            frame.set_cursor_position((cursor_x, cursor_y));
                        }
                    }
                }
            })?;
            should_redraw = false;
        }

        if !event::poll(Duration::from_millis(80))? {
            continue;
        }

        match event::read()? {
            Event::Resize(..) => {
                should_redraw = true;
            }
            Event::Key(key) => {
                if key.kind != KeyEventKind::Press {
                    continue;
                }

                let size = tui.terminal_mut().size()?;
                let viewport_height = size.height.saturating_sub(3) as usize;
                let content_width = size.width.saturating_sub(2);
                let half_page = (viewport_height / 2).max(1);
                let full_page = viewport_height.max(1);

                if handle_key_event(
                    &mut app,
                    key,
                    viewport_height,
                    content_width,
                    half_page,
                    full_page,
                ) {
                    break;
                }
                should_redraw = true;
            }
            _ => {}
        }
    }

    Ok(())
}

fn protocol_override(image_protocol: ImageProtocol) -> Option<ProtocolType> {
    match image_protocol {
        ImageProtocol::Auto => None,
        ImageProtocol::Halfblocks => Some(ProtocolType::Halfblocks),
        ImageProtocol::Sixel => Some(ProtocolType::Sixel),
        ImageProtocol::Kitty => Some(ProtocolType::Kitty),
        ImageProtocol::Iterm2 => Some(ProtocolType::Iterm2),
    }
}

fn handle_key_event(
    app: &mut App,
    key: KeyEvent,
    viewport_height: usize,
    content_width: u16,
    half_page: usize,
    full_page: usize,
) -> bool {
    if let Mode::SearchInput(query) = &mut app.mode {
        match key.code {
            KeyCode::Esc => {
                app.mode = Mode::Normal;
            }
            KeyCode::Enter | KeyCode::Char('\n') | KeyCode::Char('\r') => {
                let q = query.clone();
                app.search(&q, viewport_height, content_width);
                app.mode = Mode::Normal;
            }
            KeyCode::Backspace => {
                query.pop();
            }
            KeyCode::Char(ch) if !key.modifiers.contains(KeyModifiers::CONTROL) => {
                query.push(ch);
            }
            _ => {}
        }
        return false;
    }

    match (key.code, key.modifiers) {
        (KeyCode::Char('q'), _) => true,
        (KeyCode::Char('?'), _) => {
            app.show_help = !app.show_help;
            false
        }
        (KeyCode::Char('r'), _) => {
            app.reload();
            false
        }
        (KeyCode::Char('/'), _) => {
            app.mode = Mode::SearchInput(String::new());
            false
        }
        (KeyCode::Char('n'), _) => {
            app.jump_next_match(viewport_height, content_width);
            false
        }
        (KeyCode::Char('N'), _) => {
            app.jump_prev_match(viewport_height, content_width);
            false
        }
        (KeyCode::Char('w'), mods) if !mods.contains(KeyModifiers::CONTROL) => {
            app.move_to_next_word(content_width, WordMotionKind::Small);
            app.ensure_cursor_visible(viewport_height, content_width);
            false
        }
        (KeyCode::Char('W'), mods) if !mods.contains(KeyModifiers::CONTROL) => {
            app.move_to_next_word(content_width, WordMotionKind::Big);
            app.ensure_cursor_visible(viewport_height, content_width);
            false
        }
        (KeyCode::Char('b'), mods) if !mods.contains(KeyModifiers::CONTROL) => {
            app.move_to_prev_word(content_width, WordMotionKind::Small);
            app.ensure_cursor_visible(viewport_height, content_width);
            false
        }
        (KeyCode::Char('B'), mods) if !mods.contains(KeyModifiers::CONTROL) => {
            app.move_to_prev_word(content_width, WordMotionKind::Big);
            app.ensure_cursor_visible(viewport_height, content_width);
            false
        }
        (KeyCode::Char('h'), _) | (KeyCode::Left, _) => {
            app.move_cursor_left();
            false
        }
        (KeyCode::Char('l'), _) | (KeyCode::Right, _) => {
            let cursor_doc_line = app.cursor_doc_line_for_navigation(content_width);
            app.move_cursor_right(cursor_doc_line);
            false
        }
        (KeyCode::Char('j'), _) | (KeyCode::Down, _) => {
            app.scroll_down_with_image_focus(viewport_height, content_width);
            false
        }
        (KeyCode::Char('k'), _) | (KeyCode::Up, _) => {
            app.scroll_up_with_image_focus(viewport_height, content_width);
            false
        }
        (KeyCode::PageDown, _) => {
            app.scroll_down(full_page, viewport_height, content_width);
            false
        }
        (KeyCode::PageUp, _) => {
            app.scroll_up(full_page, viewport_height, content_width);
            false
        }
        (KeyCode::Char('f'), KeyModifiers::CONTROL) => {
            app.scroll_down(full_page, viewport_height, content_width);
            false
        }
        (KeyCode::Char('b'), KeyModifiers::CONTROL) => {
            app.scroll_up(full_page, viewport_height, content_width);
            false
        }
        (KeyCode::Char('d'), KeyModifiers::CONTROL) => {
            app.scroll_down(half_page, viewport_height, content_width);
            false
        }
        (KeyCode::Char('d'), KeyModifiers::NONE) => {
            app.scroll_down(half_page, viewport_height, content_width);
            false
        }
        (KeyCode::Char('u'), KeyModifiers::CONTROL) => {
            app.scroll_up(half_page, viewport_height, content_width);
            false
        }
        (KeyCode::Char('u'), KeyModifiers::NONE) => {
            app.scroll_up(half_page, viewport_height, content_width);
            false
        }
        (KeyCode::Char('g'), _) | (KeyCode::Home, _) => {
            app.jump_top();
            false
        }
        (KeyCode::Char('G'), _) | (KeyCode::End, _) => {
            app.jump_bottom(viewport_height, content_width);
            false
        }
        _ => false,
    }
}

fn prepare_images_for_doc(markdown_path: &Path, doc: &RenderedDoc) -> Vec<InlineImage> {
    doc.images
        .iter()
        .map(|image| {
            let source = resolve_image_source(markdown_path, &image.src);
            let load_state = if source.is_some() {
                ImageLoadState::NotRequested
            } else {
                ImageLoadState::Failed
            };

            InlineImage {
                line_index: image.line_index,
                source,
                state: None,
                scratch_state: None,
                active_variant: None,
                cached_variants: BTreeMap::new(),
                resize_pending: false,
                pending_variant: None,
                has_encoded_state: false,
                pixel_size: None,
                hinted_pixel_size: image.hinted_pixel_size,
                load_state,
            }
        })
        .collect()
}

fn start_image_loader_thread() -> (Sender<ImageLoadRequest>, Receiver<ImageLoadResult>) {
    let (request_tx, request_rx) = mpsc::channel::<ImageLoadRequest>();
    let (result_tx, result_rx) = mpsc::channel::<ImageLoadResult>();

    let _ = thread::Builder::new()
        .name("mdvi-image-loader".to_string())
        .spawn(move || image_loader_worker(request_rx, result_tx));

    (request_tx, result_rx)
}

fn image_loader_worker(request_rx: Receiver<ImageLoadRequest>, result_tx: Sender<ImageLoadResult>) {
    let remote_client = reqwest::blocking::Client::builder()
        .timeout(Duration::from_secs(15))
        .user_agent(format!("mdvi/{}", env!("CARGO_PKG_VERSION")))
        .build()
        .ok();

    while let Ok(request) = request_rx.recv() {
        let payload = match request.job {
            ImageLoadJob::ProbeLocalDimensions(path) => {
                ImageLoadResultPayload::ProbedDimensions(probe_local_image_dimensions(&path))
            }
            ImageLoadJob::LoadImage(source) => ImageLoadResultPayload::LoadedImage(
                load_dynamic_image(&source, remote_client.as_ref()),
            ),
        };
        if result_tx
            .send(ImageLoadResult {
                image_index: request.image_index,
                payload,
            })
            .is_err()
        {
            break;
        }
    }
}

fn start_image_resize_thread() -> (Sender<ImageResizeRequest>, Receiver<ImageResizeResult>) {
    let (request_tx, request_rx) = mpsc::channel::<ImageResizeRequest>();
    let (result_tx, result_rx) = mpsc::channel::<ImageResizeResult>();

    let _ = thread::Builder::new()
        .name("mdvi-image-resizer".to_string())
        .spawn(move || image_resize_worker(request_rx, result_tx));

    (request_tx, result_rx)
}

fn image_resize_worker(
    request_rx: Receiver<ImageResizeRequest>,
    result_tx: Sender<ImageResizeResult>,
) {
    while let Ok(mut request) = request_rx.recv() {
        request
            .protocol
            .resize_encode(&request.resize, request.area);
        if result_tx
            .send(ImageResizeResult {
                image_index: request.image_index,
                variant: request.variant,
                protocol: request.protocol,
            })
            .is_err()
        {
            break;
        }
    }
}

fn probe_local_image_dimensions(path: &Path) -> Option<(u32, u32)> {
    let reader = ImageReader::open(path).ok()?;
    reader.into_dimensions().ok()
}

fn load_dynamic_image(
    source: &ResolvedImageSource,
    remote_client: Option<&reqwest::blocking::Client>,
) -> Option<image::DynamicImage> {
    match source {
        ResolvedImageSource::Local(path) => ImageReader::open(path).ok()?.decode().ok(),
        ResolvedImageSource::Remote(url) => {
            let client = remote_client?;
            let response = client.get(url).send().ok()?.error_for_status().ok()?;
            let bytes = response.bytes().ok()?;
            image::load_from_memory(&bytes).ok()
        }
    }
}

fn resolve_image_source(markdown_path: &Path, src: &str) -> Option<ResolvedImageSource> {
    let src = src.trim();
    if src.is_empty() {
        return None;
    }
    if src.starts_with("http://") || src.starts_with("https://") {
        return Some(ResolvedImageSource::Remote(src.to_string()));
    }
    if let Some(path) = src.strip_prefix("file://") {
        return Some(ResolvedImageSource::Local(PathBuf::from(path)));
    }
    if src.contains("://") {
        return None;
    }

    let path = PathBuf::from(src);
    if path.is_absolute() {
        Some(ResolvedImageSource::Local(path))
    } else {
        Some(ResolvedImageSource::Local(
            markdown_path
                .parent()
                .unwrap_or_else(|| Path::new("."))
                .join(path),
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_app(lines: usize) -> App {
        let (image_loader_tx, _request_rx) = mpsc::channel::<ImageLoadRequest>();
        let (_result_tx, image_loader_rx) = mpsc::channel::<ImageLoadResult>();
        let (image_resize_tx, _resize_request_rx) = mpsc::channel::<ImageResizeRequest>();
        let (_resize_result_tx, image_resize_rx) = mpsc::channel::<ImageResizeResult>();
        let mut doc_lines = Vec::with_capacity(lines);
        for idx in 0..lines {
            doc_lines.push(Line::from(format!("Line {}", idx + 1)));
        }

        App {
            file_path: PathBuf::from("test.md"),
            file_content: String::new(),
            doc: RenderedDoc {
                lines: doc_lines,
                images: Vec::new(),
            },
            picker: Picker::from_fontsize((8, 16)),
            images: Vec::new(),
            image_index_by_line: BTreeMap::new(),
            scroll: 0,
            status: String::new(),
            show_help: false,
            mode: Mode::Normal,
            search_query: None,
            search_regex: None,
            search_matches: Vec::new(),
            active_match: 0,
            cursor_row: 0,
            cursor_column: 0,
            focused_image: None,
            image_loader_tx,
            image_loader_rx,
            image_resize_tx,
            image_resize_rx,
            virtual_row_count_cache: BTreeMap::new(),
            highlighted_lines_cache: None,
        }
    }

    fn line_text(line: &Line<'static>) -> String {
        line.spans
            .iter()
            .map(|span| span.content.as_ref())
            .collect::<String>()
    }

    fn app_with_image_for_navigation() -> App {
        let mut app = test_app(24);
        app.images = vec![InlineImage {
            line_index: 8,
            source: Some(ResolvedImageSource::Remote(
                "https://example.com/image.png".to_string(),
            )),
            state: None,
            scratch_state: None,
            active_variant: None,
            cached_variants: BTreeMap::new(),
            resize_pending: false,
            pending_variant: None,
            has_encoded_state: false,
            pixel_size: None,
            hinted_pixel_size: Some((160, 160)),
            load_state: ImageLoadState::Loaded,
        }];
        app.image_index_by_line = BTreeMap::from([(8usize, 0usize)]);
        app
    }

    #[test]
    fn scroll_clamps_to_bottom() {
        let mut app = test_app(100);
        app.scroll_down(1000, 20, 80);
        let max_scroll = app.max_scroll(20, 80);
        assert_eq!(app.scroll, max_scroll);
    }

    #[test]
    fn max_scroll_cache_is_keyed_by_content_width() {
        let mut app = test_app(60);

        let _ = app.max_scroll(20, 80);
        let _ = app.max_scroll(20, 100);

        assert_eq!(app.virtual_row_count_cache.get(&80).copied(), Some(60));
        assert_eq!(app.virtual_row_count_cache.get(&100).copied(), Some(60));
        assert_eq!(app.virtual_row_count_cache.len(), 2);
    }

    #[test]
    fn max_scroll_cache_invalidates_when_search_changes() {
        let mut app = test_app(20);
        let _ = app.max_scroll(10, 80);
        assert!(!app.virtual_row_count_cache.is_empty());

        app.search("not present", 10, 80);
        assert!(app.virtual_row_count_cache.is_empty());
    }

    #[test]
    fn max_scroll_cache_invalidates_after_image_layout_update() {
        let (image_loader_tx, _request_rx) = mpsc::channel::<ImageLoadRequest>();
        let (result_tx, image_loader_rx) = mpsc::channel::<ImageLoadResult>();
        let mut app = test_app(5);
        app.images = vec![InlineImage {
            line_index: 2,
            source: Some(ResolvedImageSource::Remote(
                "https://example.com/image.png".to_string(),
            )),
            state: None,
            scratch_state: None,
            active_variant: None,
            cached_variants: BTreeMap::new(),
            resize_pending: false,
            pending_variant: None,
            has_encoded_state: false,
            pixel_size: None,
            hinted_pixel_size: None,
            load_state: ImageLoadState::Loading,
        }];
        app.image_index_by_line = BTreeMap::from([(2usize, 0usize)]);
        app.image_loader_tx = image_loader_tx;
        app.image_loader_rx = image_loader_rx;

        let _ = app.max_scroll(10, 80);
        assert!(!app.virtual_row_count_cache.is_empty());

        result_tx
            .send(ImageLoadResult {
                image_index: 0,
                payload: ImageLoadResultPayload::LoadedImage(Some(image::DynamicImage::new_rgb8(
                    320, 200,
                ))),
            })
            .expect("image result should enqueue");

        assert!(app.drain_image_results());
        assert!(app.virtual_row_count_cache.is_empty());
    }

    #[test]
    fn local_dimension_probes_are_queued_for_unhinted_local_images() {
        let (image_loader_tx, request_rx) = mpsc::channel::<ImageLoadRequest>();
        let mut app = test_app(2);
        app.images = vec![
            InlineImage {
                line_index: 0,
                source: Some(ResolvedImageSource::Local(PathBuf::from(
                    "/tmp/image-a.png",
                ))),
                state: None,
                scratch_state: None,
                active_variant: None,
                cached_variants: BTreeMap::new(),
                resize_pending: false,
                pending_variant: None,
                has_encoded_state: false,
                pixel_size: None,
                hinted_pixel_size: None,
                load_state: ImageLoadState::NotRequested,
            },
            InlineImage {
                line_index: 1,
                source: Some(ResolvedImageSource::Remote(
                    "https://example.com/image-b.png".to_string(),
                )),
                state: None,
                scratch_state: None,
                active_variant: None,
                cached_variants: BTreeMap::new(),
                resize_pending: false,
                pending_variant: None,
                has_encoded_state: false,
                pixel_size: None,
                hinted_pixel_size: None,
                load_state: ImageLoadState::NotRequested,
            },
        ];
        app.image_loader_tx = image_loader_tx;

        app.request_local_dimension_probes();

        let request = request_rx
            .try_recv()
            .expect("probe request should be queued");
        assert_eq!(request.image_index, 0);
        match request.job {
            ImageLoadJob::ProbeLocalDimensions(path) => {
                assert_eq!(path, PathBuf::from("/tmp/image-a.png"));
            }
            ImageLoadJob::LoadImage(_) => panic!("expected dimension probe request"),
        }
        assert!(request_rx.try_recv().is_err());
    }

    #[test]
    fn probe_results_update_hints_and_invalidate_cache() {
        let (image_loader_tx, _request_rx) = mpsc::channel::<ImageLoadRequest>();
        let (result_tx, image_loader_rx) = mpsc::channel::<ImageLoadResult>();
        let mut app = test_app(2);
        app.images = vec![InlineImage {
            line_index: 0,
            source: Some(ResolvedImageSource::Local(PathBuf::from(
                "/tmp/image-c.png",
            ))),
            state: None,
            scratch_state: None,
            active_variant: None,
            cached_variants: BTreeMap::new(),
            resize_pending: false,
            pending_variant: None,
            has_encoded_state: false,
            pixel_size: None,
            hinted_pixel_size: None,
            load_state: ImageLoadState::NotRequested,
        }];
        app.image_loader_tx = image_loader_tx;
        app.image_loader_rx = image_loader_rx;
        app.virtual_row_count_cache.insert(80, 20);

        result_tx
            .send(ImageLoadResult {
                image_index: 0,
                payload: ImageLoadResultPayload::ProbedDimensions(Some((640, 360))),
            })
            .expect("probe result should enqueue");

        assert!(app.drain_image_results());
        assert_eq!(app.images[0].hinted_pixel_size, Some((640, 360)));
        assert_eq!(app.images[0].load_state, ImageLoadState::NotRequested);
        assert!(app.virtual_row_count_cache.is_empty());
    }

    #[test]
    fn drain_image_results_limits_per_tick() {
        let (image_loader_tx, _request_rx) = mpsc::channel::<ImageLoadRequest>();
        let (result_tx, image_loader_rx) = mpsc::channel::<ImageLoadResult>();
        let mut app = test_app(8);
        app.images = (0..8)
            .map(|line_index| InlineImage {
                line_index,
                source: Some(ResolvedImageSource::Remote(
                    "https://example.com/image.png".to_string(),
                )),
                state: None,
                scratch_state: None,
                active_variant: None,
                cached_variants: BTreeMap::new(),
                resize_pending: false,
                pending_variant: None,
                has_encoded_state: false,
                pixel_size: None,
                hinted_pixel_size: None,
                load_state: ImageLoadState::Loading,
            })
            .collect();
        app.image_loader_tx = image_loader_tx;
        app.image_loader_rx = image_loader_rx;

        for image_index in 0..8 {
            result_tx
                .send(ImageLoadResult {
                    image_index,
                    payload: ImageLoadResultPayload::LoadedImage(Some(
                        image::DynamicImage::new_rgb8(320, 200),
                    )),
                })
                .expect("image result should enqueue");
        }

        assert!(app.drain_image_results());
        let loaded_count = app
            .images
            .iter()
            .filter(|image| image.load_state == ImageLoadState::Loaded)
            .count();
        assert_eq!(loaded_count, MAX_IMAGE_RESULTS_PER_TICK);
    }

    #[test]
    fn request_image_resize_moves_work_off_ui_thread() {
        let (image_resize_tx, resize_request_rx) = mpsc::channel::<ImageResizeRequest>();
        let mut app = test_app(1);
        app.images = vec![InlineImage {
            line_index: 0,
            source: Some(ResolvedImageSource::Remote(
                "https://example.com/image.png".to_string(),
            )),
            state: Some(
                app.picker
                    .new_resize_protocol(image::DynamicImage::new_rgb8(320, 200)),
            ),
            scratch_state: Some(
                app.picker
                    .new_resize_protocol(image::DynamicImage::new_rgb8(320, 200)),
            ),
            active_variant: None,
            cached_variants: BTreeMap::new(),
            resize_pending: false,
            pending_variant: None,
            has_encoded_state: false,
            pixel_size: Some((320, 200)),
            hinted_pixel_size: None,
            load_state: ImageLoadState::Loaded,
        }];
        app.image_resize_tx = image_resize_tx;

        app.request_image_resize(0, Rect::new(0, 0, 40, 10), Resize::Fit(None));

        assert!(app.images[0].resize_pending);
        assert!(app.images[0].state.is_some());
        assert_eq!(
            app.images[0].pending_variant,
            Some(ImageResizeVariant {
                width: 32,
                height: 10,
                mode: ImageResizeMode::Fit,
            })
        );
        let request = resize_request_rx
            .try_recv()
            .expect("resize request should be queued");
        assert_eq!(request.image_index, 0);
    }

    #[test]
    fn resize_results_restore_render_state() {
        let (image_resize_tx, _resize_request_rx) = mpsc::channel::<ImageResizeRequest>();
        let (resize_result_tx, image_resize_rx) = mpsc::channel::<ImageResizeResult>();
        let mut app = test_app(1);
        app.images = vec![InlineImage {
            line_index: 0,
            source: Some(ResolvedImageSource::Remote(
                "https://example.com/image.png".to_string(),
            )),
            state: None,
            scratch_state: None,
            active_variant: None,
            cached_variants: BTreeMap::new(),
            resize_pending: true,
            pending_variant: Some(ImageResizeVariant {
                width: 40,
                height: 10,
                mode: ImageResizeMode::Fit,
            }),
            has_encoded_state: true,
            pixel_size: Some((320, 200)),
            hinted_pixel_size: None,
            load_state: ImageLoadState::Loaded,
        }];
        app.image_resize_tx = image_resize_tx;
        app.image_resize_rx = image_resize_rx;

        resize_result_tx
            .send(ImageResizeResult {
                image_index: 0,
                variant: ImageResizeVariant {
                    width: 40,
                    height: 10,
                    mode: ImageResizeMode::Fit,
                },
                protocol: app
                    .picker
                    .new_resize_protocol(image::DynamicImage::new_rgb8(320, 200)),
            })
            .expect("resize result should enqueue");

        assert!(app.drain_image_resize_results());
        assert!(!app.images[0].resize_pending);
        assert!(app.images[0].state.is_some());
    }

    #[test]
    fn request_image_resize_uses_cached_variant_without_queueing() {
        let (image_resize_tx, resize_request_rx) = mpsc::channel::<ImageResizeRequest>();
        let mut app = test_app(1);
        let cached_variant = ImageResizeVariant {
            width: 32,
            height: 10,
            mode: ImageResizeMode::Fit,
        };
        app.images = vec![InlineImage {
            line_index: 0,
            source: Some(ResolvedImageSource::Remote(
                "https://example.com/image.png".to_string(),
            )),
            state: Some(
                app.picker
                    .new_resize_protocol(image::DynamicImage::new_rgb8(320, 200)),
            ),
            scratch_state: Some(
                app.picker
                    .new_resize_protocol(image::DynamicImage::new_rgb8(320, 200)),
            ),
            active_variant: None,
            cached_variants: BTreeMap::from([(
                cached_variant,
                app.picker
                    .new_resize_protocol(image::DynamicImage::new_rgb8(320, 200)),
            )]),
            resize_pending: false,
            pending_variant: None,
            has_encoded_state: false,
            pixel_size: Some((320, 200)),
            hinted_pixel_size: None,
            load_state: ImageLoadState::Loaded,
        }];
        app.image_resize_tx = image_resize_tx;

        app.request_image_resize(0, Rect::new(0, 0, 40, 10), Resize::Fit(None));

        assert_eq!(app.images[0].active_variant, Some(cached_variant));
        assert!(app.images[0].state.is_some());
        assert!(resize_request_rx.try_recv().is_err());
    }

    #[test]
    fn resize_result_for_pending_variant_becomes_active_immediately() {
        let (image_resize_tx, _resize_request_rx) = mpsc::channel::<ImageResizeRequest>();
        let (resize_result_tx, image_resize_rx) = mpsc::channel::<ImageResizeResult>();
        let mut app = test_app(1);
        let previous_variant = ImageResizeVariant {
            width: 32,
            height: 10,
            mode: ImageResizeMode::Fit,
        };
        let pending_variant = ImageResizeVariant {
            width: 32,
            height: 9,
            mode: ImageResizeMode::Crop {
                clip_top: true,
                clip_left: false,
            },
        };
        app.images = vec![InlineImage {
            line_index: 0,
            source: Some(ResolvedImageSource::Remote(
                "https://example.com/image.png".to_string(),
            )),
            state: Some(
                app.picker
                    .new_resize_protocol(image::DynamicImage::new_rgb8(320, 200)),
            ),
            scratch_state: None,
            active_variant: Some(previous_variant),
            cached_variants: BTreeMap::new(),
            resize_pending: true,
            pending_variant: Some(pending_variant),
            has_encoded_state: true,
            pixel_size: Some((320, 200)),
            hinted_pixel_size: None,
            load_state: ImageLoadState::Loaded,
        }];
        app.image_resize_tx = image_resize_tx;
        app.image_resize_rx = image_resize_rx;

        resize_result_tx
            .send(ImageResizeResult {
                image_index: 0,
                variant: pending_variant,
                protocol: app
                    .picker
                    .new_resize_protocol(image::DynamicImage::new_rgb8(320, 200)),
            })
            .expect("resize result should enqueue");

        assert!(app.drain_image_resize_results());
        assert_eq!(app.images[0].active_variant, Some(pending_variant));
        assert!(app.images[0]
            .cached_variants
            .contains_key(&previous_variant));
    }

    #[test]
    fn scroll_up_saturates() {
        let mut app = test_app(50);
        app.scroll = 10;
        app.cursor_row = 10;
        app.scroll_up(500, 20, 80);
        assert_eq!(app.scroll, 0);
        assert_eq!(app.cursor_row, 0);
    }

    #[test]
    fn scroll_down_focuses_image_then_jumps_past() {
        let mut app = app_with_image_for_navigation();
        let viewport_height = 10;
        let content_width = 80;

        app.scroll = 3;
        app.cursor_row = 8;
        app.scroll_down_with_image_focus(viewport_height, content_width);
        assert_eq!(app.cursor_row, 9);
        assert_eq!(app.scroll, 3);
        assert_eq!(app.focused_image, Some(0));

        app.scroll_down_with_image_focus(viewport_height, content_width);
        assert_eq!(app.cursor_row, 19);
        assert_eq!(app.scroll, 10);
        assert_eq!(app.focused_image, None);
    }

    #[test]
    fn scroll_up_focuses_image_then_jumps_before() {
        let mut app = app_with_image_for_navigation();
        let viewport_height = 10;
        let content_width = 80;

        app.scroll = 10;
        app.cursor_row = 19;
        app.scroll_up_with_image_focus(viewport_height, content_width);
        assert_eq!(app.cursor_row, 18);
        assert_eq!(app.scroll, 10);
        assert_eq!(app.focused_image, Some(0));

        app.scroll_up_with_image_focus(viewport_height, content_width);
        assert_eq!(app.cursor_row, 8);
        assert_eq!(app.scroll, 8);
        assert_eq!(app.focused_image, None);
    }

    #[test]
    fn search_finds_and_jumps_to_first_match() {
        let mut app = test_app(20);
        app.doc.lines[5] = Line::from("alpha keyword");
        app.doc.lines[12] = Line::from("beta keyword");

        app.search("keyword", 10, 80);
        assert_eq!(app.search_matches, vec![5, 12]);
        assert_eq!(app.scroll, 5);
        assert_eq!(app.cursor_row, 5);
    }

    #[test]
    fn next_match_wraps_around() {
        let mut app = test_app(30);
        app.search_matches = vec![3, 8];
        app.active_match = 1;

        app.jump_next_match(10, 80);
        assert_eq!(app.active_match, 0);
        assert_eq!(app.scroll, 3);
        assert_eq!(app.cursor_row, 3);
    }

    #[test]
    fn highlighted_lines_cache_reuses_when_search_state_is_unchanged() {
        let mut app = test_app(6);
        app.doc.lines[2] = Line::from("alpha keyword");
        app.search("keyword", 10, 80);

        let _ = app.highlighted_lines();
        assert!(app.highlighted_lines_cache.is_some());

        app.doc.lines[2] = Line::from("mutated but cache should still be reused");
        let highlighted = app.highlighted_lines();
        assert_eq!(line_text(&highlighted[2]), "alpha keyword");
    }

    #[test]
    fn highlighted_lines_cache_tracks_active_match_line() {
        let mut app = test_app(20);
        app.doc.lines[5] = Line::from("alpha keyword");
        app.doc.lines[12] = Line::from("beta keyword");
        app.search("keyword", 10, 80);

        let _ = app.highlighted_lines();
        assert_eq!(
            app.highlighted_lines_cache
                .as_ref()
                .and_then(|cache| cache.active_match_line),
            Some(5)
        );

        app.jump_next_match(10, 80);
        let _ = app.highlighted_lines();
        assert_eq!(
            app.highlighted_lines_cache
                .as_ref()
                .and_then(|cache| cache.active_match_line),
            Some(12)
        );
    }

    #[test]
    fn ctrl_f_and_ctrl_b_scroll_full_page() {
        let mut app = test_app(200);
        let viewport_height = 20;
        let full_page = 20;
        let half_page = 10;

        let _ = handle_key_event(
            &mut app,
            KeyEvent::new(KeyCode::Char('f'), KeyModifiers::CONTROL),
            viewport_height,
            80,
            half_page,
            full_page,
        );
        assert_eq!(app.scroll, 20);

        let _ = handle_key_event(
            &mut app,
            KeyEvent::new(KeyCode::Char('b'), KeyModifiers::CONTROL),
            viewport_height,
            80,
            half_page,
            full_page,
        );
        assert_eq!(app.scroll, 0);
    }

    #[test]
    fn d_and_u_scroll_half_page_without_ctrl() {
        let mut app = test_app(200);
        let viewport_height = 20;
        let full_page = 20;
        let half_page = 10;

        let _ = handle_key_event(
            &mut app,
            KeyEvent::new(KeyCode::Char('d'), KeyModifiers::NONE),
            viewport_height,
            80,
            half_page,
            full_page,
        );
        assert_eq!(app.scroll, 10);

        let _ = handle_key_event(
            &mut app,
            KeyEvent::new(KeyCode::Char('u'), KeyModifiers::NONE),
            viewport_height,
            80,
            half_page,
            full_page,
        );
        assert_eq!(app.scroll, 0);
    }

    #[test]
    fn h_and_l_move_cursor_column_within_current_line() {
        let mut app = test_app(20);
        app.scroll = 1;
        app.cursor_row = 10;
        let viewport_height = 20;
        let full_page = 20;
        let half_page = 10;
        let cursor_doc_line = app.cursor_doc_line_for_navigation(80);
        app.doc.lines[cursor_doc_line] = Line::from("abcdef");

        let _ = handle_key_event(
            &mut app,
            KeyEvent::new(KeyCode::Char('l'), KeyModifiers::NONE),
            viewport_height,
            80,
            half_page,
            full_page,
        );
        let _ = handle_key_event(
            &mut app,
            KeyEvent::new(KeyCode::Right, KeyModifiers::NONE),
            viewport_height,
            80,
            half_page,
            full_page,
        );
        assert_eq!(app.cursor_column, 2);

        let _ = handle_key_event(
            &mut app,
            KeyEvent::new(KeyCode::Char('h'), KeyModifiers::NONE),
            viewport_height,
            80,
            half_page,
            full_page,
        );
        assert_eq!(app.cursor_column, 1);
    }

    #[test]
    fn cursor_column_clamps_to_line_width() {
        let mut app = test_app(20);
        app.scroll = 1;
        app.cursor_row = 10;
        let viewport_height = 20;
        let full_page = 20;
        let half_page = 10;
        let cursor_doc_line = app.cursor_doc_line_for_navigation(80);
        app.doc.lines[cursor_doc_line] = Line::from("abc");

        for _ in 0..10 {
            let _ = handle_key_event(
                &mut app,
                KeyEvent::new(KeyCode::Char('l'), KeyModifiers::NONE),
                viewport_height,
                80,
                half_page,
                full_page,
            );
        }

        assert_eq!(app.cursor_column, 2);
        assert_eq!(app.effective_cursor_column_for_line(cursor_doc_line), 2);
    }

    #[test]
    fn initial_cursor_starts_at_top_of_document() {
        let app = test_app(20);
        assert_eq!(app.scroll, 0);
        assert_eq!(app.cursor_row, 0);
        assert_eq!(app.cursor_column, 0);
    }

    #[test]
    fn w_and_b_move_by_small_words() {
        let mut app = test_app(5);
        app.doc.lines[0] = Line::from("alpha, beta gamma");
        let viewport_height = 10;
        let full_page = 10;
        let half_page = 5;

        let _ = handle_key_event(
            &mut app,
            KeyEvent::new(KeyCode::Char('w'), KeyModifiers::NONE),
            viewport_height,
            80,
            half_page,
            full_page,
        );
        assert_eq!(app.cursor_column, 5);

        let _ = handle_key_event(
            &mut app,
            KeyEvent::new(KeyCode::Char('w'), KeyModifiers::NONE),
            viewport_height,
            80,
            half_page,
            full_page,
        );
        assert_eq!(app.cursor_column, 7);

        let _ = handle_key_event(
            &mut app,
            KeyEvent::new(KeyCode::Char('b'), KeyModifiers::NONE),
            viewport_height,
            80,
            half_page,
            full_page,
        );
        assert_eq!(app.cursor_column, 5);
    }

    #[test]
    fn big_word_motions_move_across_whitespace_delimited_words() {
        let mut app = test_app(5);
        app.doc.lines[0] = Line::from("alpha, beta gamma");
        let viewport_height = 10;
        let full_page = 10;
        let half_page = 5;

        let _ = handle_key_event(
            &mut app,
            KeyEvent::new(KeyCode::Char('W'), KeyModifiers::SHIFT),
            viewport_height,
            80,
            half_page,
            full_page,
        );
        assert_eq!(app.cursor_column, 7);

        let _ = handle_key_event(
            &mut app,
            KeyEvent::new(KeyCode::Char('B'), KeyModifiers::SHIFT),
            viewport_height,
            80,
            half_page,
            full_page,
        );
        assert_eq!(app.cursor_column, 0);
    }

    #[test]
    fn search_highlight_applies_reverse_modifier() {
        let regex = RegexBuilder::new(&regex::escape("mdvi"))
            .case_insensitive(true)
            .build()
            .expect("regex should compile");
        let line = Line::from(vec![
            Span::styled("hello ".to_string(), Style::default()),
            Span::styled(
                "mdvi".to_string(),
                Style::default().add_modifier(Modifier::BOLD),
            ),
            Span::styled(" world".to_string(), Style::default()),
        ]);

        let highlighted = highlight_lines(&[line], &regex, Some(0));
        let spans = &highlighted[0].spans;
        let mdvi_span = spans
            .iter()
            .find(|span| span.content.as_ref() == "mdvi")
            .expect("highlighted span exists");

        assert!(mdvi_span.style.add_modifier.contains(Modifier::REVERSED));
    }

    #[test]
    fn resolve_image_source_supports_https_urls() {
        let source = resolve_image_source(
            Path::new("/tmp/doc.md"),
            "https://example.com/assets/preview.png",
        )
        .expect("should resolve");
        match source {
            ResolvedImageSource::Remote(url) => {
                assert_eq!(url, "https://example.com/assets/preview.png")
            }
            ResolvedImageSource::Local(_) => panic!("expected remote source"),
        }
    }

    #[test]
    fn resolve_image_source_resolves_relative_local_paths() {
        let source =
            resolve_image_source(Path::new("/tmp/docs/readme.md"), "images/diagram.png").unwrap();
        match source {
            ResolvedImageSource::Local(path) => {
                assert_eq!(path, PathBuf::from("/tmp/docs/images/diagram.png"));
            }
            ResolvedImageSource::Remote(_) => panic!("expected local source"),
        }
    }

    #[test]
    fn image_protocol_override_mapping() {
        assert_eq!(protocol_override(ImageProtocol::Auto), None);
        assert_eq!(
            protocol_override(ImageProtocol::Halfblocks),
            Some(ProtocolType::Halfblocks)
        );
        assert_eq!(
            protocol_override(ImageProtocol::Sixel),
            Some(ProtocolType::Sixel)
        );
        assert_eq!(
            protocol_override(ImageProtocol::Kitty),
            Some(ProtocolType::Kitty)
        );
        assert_eq!(
            protocol_override(ImageProtocol::Iterm2),
            Some(ProtocolType::Iterm2)
        );
    }

    #[test]
    fn image_height_uses_html_hint_before_image_load() {
        let app = test_app(1);
        let image = InlineImage {
            line_index: 0,
            source: Some(ResolvedImageSource::Remote(
                "https://example.com/image.png".to_string(),
            )),
            state: None,
            scratch_state: None,
            active_variant: None,
            cached_variants: BTreeMap::new(),
            resize_pending: false,
            pending_variant: None,
            has_encoded_state: false,
            pixel_size: None,
            hinted_pixel_size: Some((1708, 1040)),
            load_state: ImageLoadState::NotRequested,
        };

        assert!(app.image_height_for(&image, 80) > 0);
    }

    #[test]
    fn image_height_collapses_after_failed_load_without_hint() {
        let app = test_app(1);
        let image = InlineImage {
            line_index: 0,
            source: Some(ResolvedImageSource::Remote(
                "https://example.com/image.png".to_string(),
            )),
            state: None,
            scratch_state: None,
            active_variant: None,
            cached_variants: BTreeMap::new(),
            resize_pending: false,
            pending_variant: None,
            has_encoded_state: false,
            pixel_size: None,
            hinted_pixel_size: None,
            load_state: ImageLoadState::Failed,
        };

        assert_eq!(app.image_height_for(&image, 80), 0);
    }
}

fn highlight_lines(
    lines: &[Line<'static>],
    regex: &Regex,
    active_match_line: Option<usize>,
) -> Vec<Line<'static>> {
    lines
        .iter()
        .enumerate()
        .map(|(idx, line)| {
            let line_text = line
                .spans
                .iter()
                .map(|span| span.content.as_ref())
                .collect::<String>();
            let ranges = regex
                .find_iter(&line_text)
                .map(|m| (m.start(), m.end()))
                .collect::<Vec<_>>();
            if ranges.is_empty() {
                return line.clone();
            }

            let highlight_style = if Some(idx) == active_match_line {
                Style::default().add_modifier(Modifier::REVERSED | Modifier::BOLD)
            } else {
                Style::default().add_modifier(Modifier::REVERSED)
            };

            apply_highlight_ranges(line, &ranges, highlight_style)
        })
        .collect()
}

fn apply_highlight_ranges(
    line: &Line<'static>,
    ranges: &[(usize, usize)],
    highlight_style: Style,
) -> Line<'static> {
    let mut out_spans: Vec<Span<'static>> = Vec::new();
    let mut range_idx = 0usize;
    let mut global_offset = 0usize;

    for span in &line.spans {
        let text = span.content.as_ref();
        let span_start = global_offset;
        let span_end = global_offset + text.len();
        let mut local_cursor = 0usize;

        while range_idx < ranges.len() {
            let (match_start, match_end) = ranges[range_idx];

            if match_end <= span_start {
                range_idx += 1;
                continue;
            }

            if match_start >= span_end {
                break;
            }

            let overlap_start = match_start.max(span_start);
            let overlap_end = match_end.min(span_end);
            let overlap_local_start = overlap_start - span_start;
            let overlap_local_end = overlap_end - span_start;

            if overlap_local_start > local_cursor {
                out_spans.push(Span::styled(
                    text[local_cursor..overlap_local_start].to_string(),
                    span.style,
                ));
            }

            if overlap_local_end > overlap_local_start {
                out_spans.push(Span::styled(
                    text[overlap_local_start..overlap_local_end].to_string(),
                    span.style.patch(highlight_style),
                ));
                local_cursor = overlap_local_end;
            }

            if match_end <= span_end {
                range_idx += 1;
            } else {
                break;
            }
        }

        if local_cursor < text.len() {
            out_spans.push(Span::styled(text[local_cursor..].to_string(), span.style));
        }

        global_offset = span_end;
    }

    Line::from(out_spans)
}
