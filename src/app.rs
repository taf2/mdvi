use std::{
    io::{self, Stdout},
    path::PathBuf,
    time::Duration,
};

use anyhow::{Context, Result};
use crossterm::{
    event::{self, Event, KeyCode, KeyEvent, KeyEventKind, KeyModifiers},
    execute,
    terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen},
};
use ratatui::{
    backend::CrosstermBackend,
    layout::{Constraint, Direction, Layout},
    style::{Modifier, Style},
    text::{Line, Span, Text},
    widgets::{Block, Borders, Paragraph, Wrap},
    Terminal,
};

use crate::renderer::{read_markdown_file, render_markdown, RenderedDoc};

type AppTerminal = Terminal<CrosstermBackend<Stdout>>;

#[derive(Debug)]
struct App {
    file_path: PathBuf,
    file_content: String,
    doc: RenderedDoc,
    scroll: usize,
    status: String,
    show_help: bool,
    mode: Mode,
    search_matches: Vec<usize>,
    active_match: usize,
}

#[derive(Debug, Default)]
enum Mode {
    #[default]
    Normal,
    SearchInput(String),
}

impl App {
    fn new(file_path: PathBuf, start_line: usize) -> Result<Self> {
        let file_content = read_markdown_file(&file_path)?;
        let doc = render_markdown(&file_content)?;

        let scroll = start_line
            .saturating_sub(1)
            .min(doc.lines.len().saturating_sub(1));
        let status = format!("{} lines", doc.lines.len());

        Ok(Self {
            file_path,
            file_content,
            doc,
            scroll,
            status,
            show_help: false,
            mode: Mode::Normal,
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
                self.scroll = self.scroll.min(self.doc.lines.len().saturating_sub(1));
                self.status = format!("reloaded {}", self.file_path.display());
            }
            Err(err) => {
                self.status = format!("reload failed: {err}");
            }
        }
    }

    fn max_scroll(&self, viewport_height: usize) -> usize {
        self.doc
            .lines
            .len()
            .saturating_sub(viewport_height.saturating_sub(1))
    }

    fn scroll_down(&mut self, n: usize, viewport_height: usize) {
        let max = self.max_scroll(viewport_height);
        self.scroll = self.scroll.saturating_add(n).min(max);
    }

    fn scroll_up(&mut self, n: usize) {
        self.scroll = self.scroll.saturating_sub(n);
    }

    fn jump_top(&mut self) {
        self.scroll = 0;
    }

    fn jump_bottom(&mut self, viewport_height: usize) {
        self.scroll = self.max_scroll(viewport_height);
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

    fn search(&mut self, query: &str, viewport_height: usize) {
        self.search_matches.clear();
        self.active_match = 0;

        if query.is_empty() {
            self.status = "search cleared".to_string();
            return;
        }

        let query_lc = query.to_lowercase();
        for (idx, _) in self.doc.lines.iter().enumerate() {
            let line = self.line_text_at(idx);
            if line.to_lowercase().contains(&query_lc) {
                self.search_matches.push(idx);
            }
        }

        if self.search_matches.is_empty() {
            self.status = format!("no matches for /{query}");
            return;
        }

        self.scroll = self.search_matches[0].min(self.max_scroll(viewport_height));
        self.status = format!("{} matches for /{query}", self.search_matches.len());
    }

    fn jump_next_match(&mut self, viewport_height: usize) {
        if self.search_matches.is_empty() {
            self.status = "no active search results".to_string();
            return;
        }
        self.active_match = (self.active_match + 1) % self.search_matches.len();
        self.scroll = self.search_matches[self.active_match].min(self.max_scroll(viewport_height));
    }

    fn jump_prev_match(&mut self, viewport_height: usize) {
        if self.search_matches.is_empty() {
            self.status = "no active search results".to_string();
            return;
        }
        if self.active_match == 0 {
            self.active_match = self.search_matches.len() - 1;
        } else {
            self.active_match -= 1;
        }
        self.scroll = self.search_matches[self.active_match].min(self.max_scroll(viewport_height));
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

pub fn run(file_path: PathBuf, start_line: usize) -> Result<()> {
    let mut app = App::new(file_path, start_line)?;
    let mut tui = TuiGuard::setup()?;

    loop {
        let terminal = tui.terminal_mut();
        terminal.draw(|frame| {
            let size = frame.area();

            let chunks = Layout::default()
                .direction(Direction::Vertical)
                .constraints([Constraint::Min(1), Constraint::Length(1)])
                .split(size);

            let viewport_height = chunks[0].height as usize;
            app.scroll = app.scroll.min(app.max_scroll(viewport_height));

            let title = format!(" mdview {} ", app.file_path.display());
            let content_block = Block::default().borders(Borders::ALL).title(title);

            let text = Text::from(app.doc.lines.clone());
            let paragraph = Paragraph::new(text)
                .block(content_block)
                .wrap(Wrap { trim: false })
                .scroll((app.scroll as u16, 0));

            frame.render_widget(paragraph, chunks[0]);

            let status = if app.show_help {
                "q quit | j/k move | g/G top/bottom | Ctrl-d/u half-page | / search | n/N next/prev | r reload | ? help"
                    .to_string()
            } else {
                match &app.mode {
                    Mode::Normal => {
                        let mut right = format!(
                            "{}  |  line {} / {}  |  ? help",
                            app.status,
                            app.scroll + 1,
                            app.doc.lines.len()
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
        })?;

        if !event::poll(Duration::from_millis(200))? {
            continue;
        }

        if let Event::Key(key) = event::read()? {
            if key.kind != KeyEventKind::Press {
                continue;
            }

            let size = tui.terminal_mut().size()?;
            let viewport_height = size.height.saturating_sub(2) as usize;
            let half_page = (viewport_height / 2).max(1);
            let full_page = viewport_height.max(1);

            if handle_key_event(&mut app, key, viewport_height, half_page, full_page) {
                break;
            }
        }
    }

    Ok(())
}

fn handle_key_event(
    app: &mut App,
    key: KeyEvent,
    viewport_height: usize,
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
                app.search(&q, viewport_height);
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
            app.jump_next_match(viewport_height);
            false
        }
        (KeyCode::Char('N'), _) => {
            app.jump_prev_match(viewport_height);
            false
        }
        (KeyCode::Char('j'), _) | (KeyCode::Down, _) => {
            app.scroll_down(1, viewport_height);
            false
        }
        (KeyCode::Char('k'), _) | (KeyCode::Up, _) => {
            app.scroll_up(1);
            false
        }
        (KeyCode::PageDown, _) => {
            app.scroll_down(full_page, viewport_height);
            false
        }
        (KeyCode::PageUp, _) => {
            app.scroll_up(full_page);
            false
        }
        (KeyCode::Char('d'), KeyModifiers::CONTROL) => {
            app.scroll_down(half_page, viewport_height);
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
            app.jump_bottom(viewport_height);
            false
        }
        _ => false,
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
            doc: RenderedDoc { lines: doc_lines },
            scroll: 0,
            status: String::new(),
            show_help: false,
            mode: Mode::Normal,
            search_matches: Vec::new(),
            active_match: 0,
        }
    }

    #[test]
    fn scroll_clamps_to_bottom() {
        let mut app = test_app(100);
        app.scroll_down(1000, 20);
        assert_eq!(app.scroll, app.max_scroll(20));
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

        app.search("keyword", 10);
        assert_eq!(app.search_matches, vec![5, 12]);
        assert_eq!(app.scroll, 5);
    }

    #[test]
    fn next_match_wraps_around() {
        let mut app = test_app(30);
        app.search_matches = vec![3, 8];
        app.active_match = 1;

        app.jump_next_match(10);
        assert_eq!(app.active_match, 0);
        assert_eq!(app.scroll, 3);
    }
}
