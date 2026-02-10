use std::{
    collections::BTreeMap,
    io::{self, Stdout},
    path::{Path, PathBuf},
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
    widgets::{Block, Borders, Paragraph, Wrap},
    Terminal,
};
use ratatui_image::{
    picker::{Picker, ProtocolType},
    protocol::StatefulProtocol,
    Resize, StatefulImage,
};
use regex::{Regex, RegexBuilder};
use unicode_width::UnicodeWidthStr;

use crate::{
    renderer::{read_markdown_file, render_markdown, RenderedDoc},
    ImageProtocol,
};

type AppTerminal = Terminal<CrosstermBackend<Stdout>>;

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
}

struct InlineImage {
    line_index: usize,
    state: Option<StatefulProtocol>,
    pixel_size: Option<(u32, u32)>,
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

enum ResolvedImageSource {
    Local(PathBuf),
    Remote(String),
}

#[derive(Debug, Default)]
enum Mode {
    #[default]
    Normal,
    SearchInput(String),
}

impl App {
    fn new(file_path: PathBuf, start_line: usize, picker: Picker) -> Result<Self> {
        let file_content = read_markdown_file(&file_path)?;
        let doc = render_markdown(&file_content)?;
        let images = load_images_for_doc(&file_path, &doc, &picker);
        let image_index_by_line = images
            .iter()
            .enumerate()
            .map(|(idx, image)| (image.line_index, idx))
            .collect::<BTreeMap<usize, usize>>();
        let loaded_images = images.iter().filter(|image| image.state.is_some()).count();
        let image_failures = images.len().saturating_sub(loaded_images);

        let scroll = start_line.saturating_sub(1);
        let status = if images.is_empty() {
            format!("{} lines", doc.lines.len())
        } else if image_failures == 0 {
            format!("{} lines, {} images", doc.lines.len(), loaded_images)
        } else {
            format!(
                "{} lines, {} images ({} failed)",
                doc.lines.len(),
                loaded_images,
                image_failures
            )
        };

        Ok(Self {
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
        })
    }

    fn reload(&mut self) {
        match read_markdown_file(&self.file_path).and_then(|content| {
            let doc = render_markdown(&content)?;
            Ok((content, doc))
        }) {
            Ok((content, doc)) => {
                self.file_content = content;
                self.doc = doc;
                self.images = load_images_for_doc(&self.file_path, &self.doc, &self.picker);
                self.image_index_by_line = self
                    .images
                    .iter()
                    .enumerate()
                    .map(|(idx, image)| (image.line_index, idx))
                    .collect::<BTreeMap<usize, usize>>();
                if self.search_regex.is_some() {
                    self.rebuild_search_matches();
                }
                let loaded_images = self
                    .images
                    .iter()
                    .filter(|image| image.state.is_some())
                    .count();
                let image_failures = self.images.len().saturating_sub(loaded_images);
                self.status = if self.images.is_empty() {
                    format!("reloaded {}", self.file_path.display())
                } else if image_failures == 0 {
                    format!(
                        "reloaded {} ({loaded_images} images)",
                        self.file_path.display()
                    )
                } else {
                    format!(
                        "reloaded {} ({loaded_images} images, {image_failures} failed)",
                        self.file_path.display()
                    )
                };
            }
            Err(err) => {
                self.status = format!("reload failed: {err}");
            }
        }
    }

    fn total_virtual_lines(&self, content_width: u16) -> usize {
        self.build_display_doc(content_width).lines.len().max(1)
    }

    fn max_scroll(&self, viewport_height: usize, content_width: u16) -> usize {
        self.total_virtual_lines(content_width)
            .saturating_sub(viewport_height.saturating_sub(1))
    }

    fn scroll_down(&mut self, n: usize, viewport_height: usize, content_width: u16) {
        let max = self.max_scroll(viewport_height, content_width);
        self.scroll = self.scroll.saturating_add(n).min(max);
    }

    fn scroll_up(&mut self, n: usize) {
        self.scroll = self.scroll.saturating_sub(n);
    }

    fn jump_top(&mut self) {
        self.scroll = 0;
    }

    fn jump_bottom(&mut self, viewport_height: usize, content_width: u16) {
        self.scroll = self.max_scroll(viewport_height, content_width);
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

    fn search(&mut self, query: &str, viewport_height: usize, content_width: u16) {
        if query.is_empty() {
            self.search_query = None;
            self.search_regex = None;
            self.search_matches.clear();
            self.active_match = 0;
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

        if self.search_matches.is_empty() {
            self.status = format!("no matches for /{query}");
            return;
        }

        self.scroll = self
            .virtual_row_for_doc_line(self.search_matches[0], content_width)
            .min(self.max_scroll(viewport_height, content_width));
        self.status = format!("{} matches for /{query}", self.search_matches.len());
    }

    fn jump_next_match(&mut self, viewport_height: usize, content_width: u16) {
        if self.search_matches.is_empty() {
            self.status = "no active search results".to_string();
            return;
        }
        self.active_match = (self.active_match + 1) % self.search_matches.len();
        self.scroll = self
            .virtual_row_for_doc_line(self.search_matches[self.active_match], content_width)
            .min(self.max_scroll(viewport_height, content_width));
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
        self.scroll = self
            .virtual_row_for_doc_line(self.search_matches[self.active_match], content_width)
            .min(self.max_scroll(viewport_height, content_width));
    }

    fn active_match_line(&self) -> Option<usize> {
        self.search_matches.get(self.active_match).copied()
    }

    fn highlighted_lines(&self) -> Vec<Line<'static>> {
        let Some(regex) = self.search_regex.as_ref() else {
            return self.doc.lines.clone();
        };

        highlight_lines(&self.doc.lines, regex, self.active_match_line())
    }

    fn rebuild_search_matches(&mut self) {
        self.search_matches.clear();
        self.active_match = 0;

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

    fn image_height_for(&self, image: &InlineImage, content_width: u16) -> usize {
        const MAX_IMAGE_CELL_HEIGHT: usize = 18;

        if image.state.is_none() {
            return 0;
        }
        let Some((pixel_width, pixel_height)) = image.pixel_size else {
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

    fn build_display_doc(&self, content_width: u16) -> DisplayDoc {
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
                let display_doc = app.build_display_doc(content_width);
                let total_rows = display_doc.lines.len().max(1);
                let total_doc_lines = app.doc.lines.len().max(1);
                app.scroll = app
                    .scroll
                    .min(app.max_scroll(viewport_height, content_width));

                let cursor_virtual_row = if viewport_height == 0 {
                    app.scroll.min(total_rows.saturating_sub(1))
                } else {
                    let preferred_cursor_screen_row =
                        (viewport_height / 2).min(viewport_height.saturating_sub(1));
                    let max_visible_screen_row = total_rows
                        .saturating_sub(1)
                        .saturating_sub(app.scroll)
                        .min(viewport_height.saturating_sub(1));
                    app.scroll
                        .saturating_add(preferred_cursor_screen_row.min(max_visible_screen_row))
                };
                let cursor_doc_line = display_doc
                    .doc_line_for_row
                    .get(cursor_virtual_row)
                    .copied()
                    .unwrap_or_else(|| app.doc.lines.len().saturating_sub(1));
                let cursor_screen_row = cursor_virtual_row.saturating_sub(app.scroll);

                let text = Text::from(display_doc.lines.clone());
                let paragraph = Paragraph::new(text)
                    .block(content_block)
                    .wrap(Wrap { trim: false })
                    .scroll((app.scroll as u16, 0));

                frame.render_widget(paragraph, chunks[0]);

                for plan in &display_doc.image_plans {
                    if plan.start_row < app.scroll
                        || plan.start_row.saturating_add(plan.height)
                            > app.scroll.saturating_add(viewport_height)
                    {
                        continue;
                    }
                    let image_y = content_inner
                        .y
                        .saturating_add((plan.start_row - app.scroll) as u16);
                    let image_area = Rect::new(
                        content_inner.x,
                        image_y,
                        content_inner.width,
                        plan.height as u16,
                    );
                    if image_area.width == 0 || image_area.height == 0 {
                        continue;
                    }
                    if let Some(image_state) = app.images[plan.image_index].state.as_mut() {
                        frame.render_stateful_widget(
                            StatefulImage::default().resize(Resize::Fit(None)),
                            image_area,
                            image_state,
                        );
                    }
                }

                let status = if app.show_help {
                    "q quit | j/k move | g/G top/bottom | Ctrl-d/u half-page | Ctrl-f/b page | / search | n/N next/prev | r reload | ? help"
                        .to_string()
                } else {
                    match &app.mode {
                        Mode::Normal => {
                            let mut right = format!(
                                "{}  |  row {} / {}  |  cursor {}:{}  |  ? help",
                                app.status,
                                app.scroll.saturating_add(1).min(total_rows),
                                total_rows,
                                cursor_doc_line.saturating_add(1).min(total_doc_lines),
                                1
                            );
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
                            frame.set_cursor_position((content_inner.x, cursor_y));
                        }
                    }
                }
            })?;
            should_redraw = false;
        }

        if !event::poll(Duration::from_millis(250))? {
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
        (KeyCode::Char('j'), _) | (KeyCode::Down, _) => {
            app.scroll_down(1, viewport_height, content_width);
            false
        }
        (KeyCode::Char('k'), _) | (KeyCode::Up, _) => {
            app.scroll_up(1);
            false
        }
        (KeyCode::PageDown, _) => {
            app.scroll_down(full_page, viewport_height, content_width);
            false
        }
        (KeyCode::PageUp, _) => {
            app.scroll_up(full_page);
            false
        }
        (KeyCode::Char('f'), KeyModifiers::CONTROL) => {
            app.scroll_down(full_page, viewport_height, content_width);
            false
        }
        (KeyCode::Char('b'), KeyModifiers::CONTROL) => {
            app.scroll_up(full_page);
            false
        }
        (KeyCode::Char('d'), KeyModifiers::CONTROL) => {
            app.scroll_down(half_page, viewport_height, content_width);
            false
        }
        (KeyCode::Char('u'), KeyModifiers::CONTROL) => {
            app.scroll_up(half_page);
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

fn load_images_for_doc(
    markdown_path: &Path,
    doc: &RenderedDoc,
    picker: &Picker,
) -> Vec<InlineImage> {
    let remote_client = reqwest::blocking::Client::builder()
        .timeout(Duration::from_secs(15))
        .user_agent(format!("mdvi/{}", env!("CARGO_PKG_VERSION")))
        .build()
        .ok();

    doc.images
        .iter()
        .map(|image| {
            let mut inline = InlineImage {
                line_index: image.line_index,
                state: None,
                pixel_size: None,
            };

            let Some(source) = resolve_image_source(markdown_path, &image.src) else {
                return inline;
            };

            match source {
                ResolvedImageSource::Local(path) => {
                    if let Ok(reader) = ImageReader::open(&path) {
                        if let Ok(dynamic_image) = reader.decode() {
                            inline.pixel_size =
                                Some((dynamic_image.width(), dynamic_image.height()));
                            inline.state = Some(picker.new_resize_protocol(dynamic_image));
                        }
                    }
                }
                ResolvedImageSource::Remote(url) => {
                    if let Some(client) = &remote_client {
                        if let Ok(response) = client
                            .get(url)
                            .send()
                            .and_then(|resp| resp.error_for_status())
                        {
                            if let Ok(bytes) = response.bytes() {
                                if let Ok(dynamic_image) = image::load_from_memory(&bytes) {
                                    inline.pixel_size =
                                        Some((dynamic_image.width(), dynamic_image.height()));
                                    inline.state = Some(picker.new_resize_protocol(dynamic_image));
                                }
                            }
                        }
                    }
                }
            }

            inline
        })
        .collect()
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
        }
    }

    #[test]
    fn scroll_clamps_to_bottom() {
        let mut app = test_app(100);
        app.scroll_down(1000, 20, 80);
        assert_eq!(app.scroll, app.max_scroll(20, 80));
    }

    #[test]
    fn scroll_up_saturates() {
        let mut app = test_app(50);
        app.scroll = 10;
        app.scroll_up(500);
        assert_eq!(app.scroll, 0);
    }

    #[test]
    fn search_finds_and_jumps_to_first_match() {
        let mut app = test_app(20);
        app.doc.lines[5] = Line::from("alpha keyword");
        app.doc.lines[12] = Line::from("beta keyword");

        app.search("keyword", 10, 80);
        assert_eq!(app.search_matches, vec![5, 12]);
        assert_eq!(app.scroll, 5);
    }

    #[test]
    fn next_match_wraps_around() {
        let mut app = test_app(30);
        app.search_matches = vec![3, 8];
        app.active_match = 1;

        app.jump_next_match(10, 80);
        assert_eq!(app.active_match, 0);
        assert_eq!(app.scroll, 3);
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
