use crate::html::{HtmlTranslator, RenderEvent};
use anyhow::{Context, Result};
use pulldown_cmark::{Alignment, CodeBlockKind, Event, HeadingLevel, Options, Parser, Tag, TagEnd};
use ratatui::{
    layout::Alignment as LineAlignment,
    style::{Color, Modifier, Style},
    text::{Line, Span},
};
use std::sync::OnceLock;
use syntect::{
    easy::HighlightLines,
    highlighting::{FontStyle, Style as SyntectStyle, Theme, ThemeSet},
    parsing::SyntaxSet,
};
use unicode_width::UnicodeWidthStr;

#[derive(Debug, Clone)]
pub struct RenderedDoc {
    pub lines: Vec<Line<'static>>,
    pub images: Vec<RenderedImage>,
    pub tables: Vec<std::ops::Range<usize>>,
    pub code_blocks: Vec<std::ops::Range<usize>>,
}

#[derive(Debug, Clone)]
pub struct RenderedImage {
    pub src: String,
    pub line_index: usize,
    pub hinted_pixel_size: Option<(u32, u32)>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ListKind {
    Ordered(u64),
    Unordered,
}

struct CodeHighlightAssets {
    syntax_set: SyntaxSet,
    theme: Theme,
}

/// Keep cells separate until every column's display width is known.
struct TableLayout {
    alignments: Vec<Alignment>,
    rows: Vec<Vec<Line<'static>>>,
    current_row: Vec<Line<'static>>,
}

impl TableLayout {
    fn new(alignments: Vec<Alignment>) -> Self {
        Self {
            alignments,
            rows: Vec::new(),
            current_row: Vec::new(),
        }
    }

    fn finish_row(&mut self) {
        self.rows.push(std::mem::take(&mut self.current_row));
    }

    fn render(self) -> Vec<Line<'static>> {
        let columns = self
            .rows
            .iter()
            .map(Vec::len)
            .max()
            .unwrap_or(0)
            .max(self.alignments.len());
        let widths: Vec<usize> = (0..columns)
            .map(|column| {
                self.rows
                    .iter()
                    .filter_map(|row| row.get(column))
                    .map(Line::width)
                    .max()
                    .unwrap_or(0)
                    .max(1)
            })
            .collect();
        let border_style = Style::default().add_modifier(Modifier::DIM);
        let border = |left: &str, joint: &str, right: &str| {
            Line::from(Span::styled(
                format!(
                    "{left}{}{right}",
                    widths
                        .iter()
                        .map(|w| "─".repeat(w + 2))
                        .collect::<Vec<_>>()
                        .join(joint)
                ),
                border_style,
            ))
        };
        let mut lines = vec![border("┌", "┬", "┐")];
        for (row_index, row) in self.rows.iter().enumerate() {
            let mut spans = vec![Span::styled("│", border_style)];
            for (column, width) in widths.iter().enumerate() {
                let cell = row.get(column).cloned().unwrap_or_default();
                let padding = width.saturating_sub(cell.width());
                let left = match self
                    .alignments
                    .get(column)
                    .copied()
                    .unwrap_or(Alignment::None)
                {
                    Alignment::Right => padding,
                    Alignment::Center => padding / 2,
                    _ => 0,
                };
                spans.push(Span::raw(" ".repeat(left + 1)));
                spans.extend(cell.spans.into_iter().map(|mut span| {
                    if row_index == 0 {
                        span.style = span.style.add_modifier(Modifier::BOLD);
                    }
                    span
                }));
                spans.push(Span::raw(" ".repeat(padding - left + 1)));
                spans.push(Span::styled("│", border_style));
            }
            lines.push(Line::from(spans));
            if row_index == 0 {
                lines.push(border("├", "┼", "┤"));
            }
        }
        lines.push(border("└", "┴", "┘"));
        lines
    }
}

fn expand_tabs(text: &str, tab_width: usize) -> String {
    let mut out = String::new();
    let mut column = 0;
    for part in text.split_inclusive('\t') {
        let content = part.trim_end_matches('\t');
        out.push_str(content);
        column += content.width();
        if part.ends_with('\t') {
            let spaces = tab_width - column % tab_width;
            out.push_str(&" ".repeat(spaces));
            column += spaces;
        }
    }
    out
}

pub fn render_markdown(input: &str) -> Result<RenderedDoc> {
    let mut options = Options::empty();
    options.insert(Options::ENABLE_TABLES);
    options.insert(Options::ENABLE_FOOTNOTES);
    options.insert(Options::ENABLE_STRIKETHROUGH);
    options.insert(Options::ENABLE_TASKLISTS);

    let parser = Parser::new_ext(input, options);

    let mut lines: Vec<Line<'static>> = Vec::new();
    let mut images: Vec<RenderedImage> = Vec::new();
    let mut current_spans: Vec<Span<'static>> = Vec::new();

    let mut style_stack = vec![Style::default()];
    let mut list_stack: Vec<ListKind> = Vec::new();
    let mut in_code_block = false;
    let mut code_text = String::new();
    let mut code_blocks = Vec::new();
    let mut code_block_start = 0;
    let mut active_code_highlighter: Option<HighlightLines<'static>> = None;
    let mut blockquote_starts = Vec::new();
    let mut pending_link: Option<(String, usize)> = None;
    let mut table: Option<TableLayout> = None;
    let mut tables = Vec::new();
    let mut pending_image: Option<(String, String)> = None;
    let soft_break_as_space = true;

    fn push_line(lines: &mut Vec<Line<'static>>, spans: &mut Vec<Span<'static>>) {
        if spans.is_empty() {
            lines.push(Line::default());
        } else {
            lines.push(Line::from(std::mem::take(spans)));
        }
    }

    fn blank_line(lines: &mut Vec<Line<'static>>, spans: &mut Vec<Span<'static>>) {
        if !spans.is_empty() {
            push_line(lines, spans);
        }
        if lines.last().is_some_and(|l| !l.spans.is_empty()) {
            lines.push(Line::default());
        }
    }

    fn indent_for_lists(list_stack: &[ListKind]) -> String {
        if list_stack.is_empty() {
            return String::new();
        }
        "  ".repeat(list_stack.len().saturating_sub(1))
    }

    fn append_image_entry(
        lines: &mut Vec<Line<'static>>,
        images: &mut Vec<RenderedImage>,
        src: String,
        alt_raw: String,
        hinted_pixel_size: Option<(u32, u32)>,
        trailing_blank: bool,
    ) {
        let alt = alt_raw.trim().to_string();
        let line_index = lines.len();
        let src_label = display_src_label(&src);
        let mut spans = Vec::new();
        spans.push(Span::styled(
            "[image] ".to_string(),
            Style::default().add_modifier(Modifier::DIM | Modifier::BOLD),
        ));
        if alt.is_empty() {
            spans.push(Span::styled(
                src_label.clone(),
                Style::default().add_modifier(Modifier::UNDERLINED),
            ));
        } else {
            spans.push(Span::raw(alt));
        }
        lines.push(Line::from(spans));
        images.push(RenderedImage {
            src,
            line_index,
            hinted_pixel_size,
        });
        if trailing_blank {
            lines.push(Line::default());
        }
    }

    let mut html = HtmlTranslator::new();
    let mut queue: Vec<RenderEvent<'_>> = Vec::new();
    let mut align_starts: Vec<(usize, LineAlignment)> = Vec::new();

    for md_event in parser.map(Some).chain(std::iter::once(None)) {
        match md_event {
            None => html.finish(&mut queue),
            Some(event) if pending_image.is_some() => queue.push(RenderEvent::Md(event)),
            // Block HTML never appears inside a markdown table; only inline HTML can.
            Some(Event::Html(text)) => html.feed(&text, false, false, &mut queue),
            Some(Event::InlineHtml(text)) => html.feed(&text, true, table.is_some(), &mut queue),
            Some(Event::Start(Tag::HtmlBlock) | Event::End(TagEnd::HtmlBlock)) => {
                html.block_boundary(&mut queue)
            }
            Some(
                event @ Event::End(
                    TagEnd::Paragraph
                    | TagEnd::Heading(_)
                    | TagEnd::Item
                    | TagEnd::TableCell
                    | TagEnd::BlockQuote(_)
                    | TagEnd::FootnoteDefinition,
                ),
            ) => {
                html.close_inline(&mut queue);
                queue.push(RenderEvent::Md(event));
            }
            Some(event) => queue.push(RenderEvent::Md(event)),
        }

        for event in queue.drain(..) {
            let event = match event {
                RenderEvent::Md(event) => event,
                RenderEvent::BlockBreak => {
                    if !current_spans.is_empty() {
                        push_line(&mut lines, &mut current_spans);
                    }
                    continue;
                }
                RenderEvent::BlockEnd => {
                    blank_line(&mut lines, &mut current_spans);
                    continue;
                }
                RenderEvent::AlignStart(alignment) => {
                    if !current_spans.is_empty() {
                        push_line(&mut lines, &mut current_spans);
                    }
                    align_starts.push((lines.len(), alignment));
                    continue;
                }
                RenderEvent::AlignEnd => {
                    if !current_spans.is_empty() {
                        push_line(&mut lines, &mut current_spans);
                    }
                    if let Some((start, alignment)) = align_starts.pop() {
                        let laid_out = |index: usize| {
                            code_blocks
                                .iter()
                                .chain(tables.iter())
                                .any(|range: &std::ops::Range<usize>| range.contains(&index))
                                || images.iter().any(|image| image.line_index == index)
                        };
                        for (index, line) in lines.iter_mut().enumerate().skip(start) {
                            if line.alignment.is_none() && !laid_out(index) {
                                line.alignment = Some(alignment);
                            }
                        }
                    }
                    continue;
                }
                RenderEvent::StyleStart(modifier) => {
                    let base = *style_stack.last().unwrap_or(&Style::default());
                    style_stack.push(base.add_modifier(modifier));
                    continue;
                }
                RenderEvent::StyleEnd => {
                    if style_stack.len() > 1 {
                        style_stack.pop();
                    }
                    continue;
                }
                RenderEvent::Image {
                    src,
                    alt,
                    hinted_pixel_size,
                } => {
                    blank_line(&mut lines, &mut current_spans);
                    append_image_entry(&mut lines, &mut images, src, alt, hinted_pixel_size, true);
                    continue;
                }
            };
            if pending_image.is_some() {
                match event {
                    Event::End(TagEnd::Image) => {
                        let (src, alt_raw) = pending_image.take().expect("pending image exists");
                        append_image_entry(&mut lines, &mut images, src, alt_raw, None, true);
                    }
                    Event::Text(text)
                    | Event::Code(text)
                    | Event::Html(text)
                    | Event::InlineHtml(text)
                    | Event::InlineMath(text)
                    | Event::DisplayMath(text) => {
                        if let Some((_, alt)) = pending_image.as_mut() {
                            alt.push_str(&text);
                        }
                    }
                    Event::SoftBreak | Event::HardBreak => {
                        if let Some((_, alt)) = pending_image.as_mut() {
                            alt.push(' ');
                        }
                    }
                    _ => {}
                }
                continue;
            }

            match event {
                Event::Start(tag) => match tag {
                    Tag::Paragraph => {}
                    Tag::Heading { level, .. } => {
                        blank_line(&mut lines, &mut current_spans);
                        let style = match level {
                            HeadingLevel::H1 => {
                                Style::default().add_modifier(Modifier::BOLD | Modifier::UNDERLINED)
                            }
                            HeadingLevel::H2 => Style::default().add_modifier(Modifier::BOLD),
                            HeadingLevel::H3 | HeadingLevel::H4 => {
                                Style::default().add_modifier(Modifier::BOLD | Modifier::ITALIC)
                            }
                            HeadingLevel::H5 | HeadingLevel::H6 => {
                                Style::default().add_modifier(Modifier::ITALIC)
                            }
                        };
                        style_stack.push(style);
                    }
                    Tag::BlockQuote(_) => {
                        blank_line(&mut lines, &mut current_spans);
                        blockquote_starts.push(lines.len());
                    }
                    Tag::CodeBlock(kind) => {
                        blank_line(&mut lines, &mut current_spans);
                        in_code_block = true;
                        code_block_start = lines.len();
                        let code_block_info = match kind {
                            CodeBlockKind::Fenced(lang) => {
                                let lang = lang.trim();
                                if lang.is_empty() {
                                    None
                                } else {
                                    Some(lang.to_string())
                                }
                            }
                            CodeBlockKind::Indented => None,
                        };
                        let code_block_language = code_block_info
                            .as_deref()
                            .and_then(extract_code_block_language_token)
                            .map(ToString::to_string);
                        active_code_highlighter =
                            new_code_highlighter(code_block_language.as_deref());

                        let header = match &code_block_info {
                            Some(lang) => format!("```{lang}"),
                            None => "```".to_string(),
                        };
                        lines.push(Line::from(Span::styled(
                            header,
                            Style::default().add_modifier(Modifier::DIM),
                        )));
                    }
                    Tag::List(start) => {
                        let kind = match start {
                            Some(v) => ListKind::Ordered(v),
                            None => ListKind::Unordered,
                        };
                        list_stack.push(kind);
                    }
                    Tag::Item => {
                        if !current_spans.is_empty() {
                            push_line(&mut lines, &mut current_spans);
                        }
                        let indent = indent_for_lists(&list_stack);
                        let marker = match list_stack.last_mut() {
                            Some(ListKind::Ordered(n)) => {
                                let m = format!("{n}. ");
                                *n += 1;
                                m
                            }
                            Some(ListKind::Unordered) | None => "• ".to_string(),
                        };
                        current_spans.push(Span::raw(format!("{indent}{marker}")));
                    }
                    Tag::Emphasis => {
                        let base = *style_stack.last().unwrap_or(&Style::default());
                        style_stack.push(base.add_modifier(Modifier::ITALIC));
                    }
                    Tag::Strong => {
                        let base = *style_stack.last().unwrap_or(&Style::default());
                        style_stack.push(base.add_modifier(Modifier::BOLD));
                    }
                    Tag::Strikethrough => {
                        let base = *style_stack.last().unwrap_or(&Style::default());
                        style_stack.push(base.add_modifier(Modifier::CROSSED_OUT));
                    }
                    Tag::Link { dest_url, .. } => {
                        let base = *style_stack.last().unwrap_or(&Style::default());
                        style_stack.push(base.add_modifier(Modifier::UNDERLINED));
                        pending_link = Some((dest_url.to_string(), current_spans.len()));
                    }
                    Tag::Image { dest_url, .. } => {
                        blank_line(&mut lines, &mut current_spans);
                        pending_image = Some((dest_url.to_string(), String::new()));
                    }
                    Tag::Table(alignments) => {
                        blank_line(&mut lines, &mut current_spans);
                        table = Some(TableLayout::new(alignments));
                    }
                    Tag::TableHead => {}
                    Tag::TableRow => {}
                    Tag::TableCell => {}
                    Tag::FootnoteDefinition(name) => {
                        blank_line(&mut lines, &mut current_spans);
                        current_spans.push(Span::styled(
                            format!("[^{name}] "),
                            Style::default().add_modifier(Modifier::DIM),
                        ));
                    }
                    _ => {}
                },
                Event::End(tag) => match tag {
                    TagEnd::Paragraph => {
                        push_line(&mut lines, &mut current_spans);
                        lines.push(Line::default());
                    }
                    TagEnd::Heading(_) => {
                        push_line(&mut lines, &mut current_spans);
                        style_stack.pop();
                        lines.push(Line::default());
                    }
                    TagEnd::BlockQuote(_) => {
                        if !current_spans.is_empty() {
                            push_line(&mut lines, &mut current_spans);
                        }
                        if let Some(start) = blockquote_starts.pop() {
                            for line in &mut lines[start..] {
                                line.spans.insert(
                                    0,
                                    Span::styled(
                                        "│ ",
                                        Style::default().add_modifier(Modifier::DIM),
                                    ),
                                );
                            }
                        }
                        blank_line(&mut lines, &mut current_spans);
                    }
                    TagEnd::CodeBlock => {
                        append_code_block_text(
                            &code_text,
                            &mut current_spans,
                            &mut lines,
                            &mut active_code_highlighter,
                            Style::default(),
                        );
                        code_text.clear();
                        if !current_spans.is_empty() {
                            push_line(&mut lines, &mut current_spans);
                        }
                        lines.push(Line::from(Span::styled(
                            "```".to_string(),
                            Style::default().add_modifier(Modifier::DIM),
                        )));
                        lines.push(Line::default());
                        code_blocks.push(code_block_start..lines.len() - 1);
                        in_code_block = false;
                        active_code_highlighter = None;
                    }
                    TagEnd::List(_) => {
                        list_stack.pop();
                        if list_stack.is_empty() {
                            blank_line(&mut lines, &mut current_spans);
                        }
                    }
                    TagEnd::Item => {
                        if !current_spans.is_empty() {
                            push_line(&mut lines, &mut current_spans);
                        }
                    }
                    TagEnd::Emphasis | TagEnd::Strong | TagEnd::Strikethrough => {
                        style_stack.pop();
                    }
                    TagEnd::Link => {
                        style_stack.pop();
                        if let Some((link, start)) = pending_link.take() {
                            let label: String = current_spans
                                .get(start..)
                                .unwrap_or_default()
                                .iter()
                                .map(|s| s.content.as_ref())
                                .collect();
                            if label != link {
                                current_spans.push(Span::styled(
                                    format!(" ({link})"),
                                    Style::default().add_modifier(Modifier::DIM),
                                ));
                            }
                        }
                    }
                    TagEnd::Image => {}
                    TagEnd::Table => {
                        if let Some(mut table) = table.take() {
                            if !table.current_row.is_empty() {
                                table.finish_row();
                            }
                            let start = lines.len();
                            lines.extend(table.render());
                            tables.push(start..lines.len());
                        }
                        blank_line(&mut lines, &mut current_spans);
                    }
                    TagEnd::TableHead | TagEnd::TableRow => {
                        if let Some(table) = table.as_mut() {
                            table.finish_row();
                        }
                    }
                    TagEnd::TableCell => {
                        if let Some(table) = table.as_mut() {
                            table
                                .current_row
                                .push(Line::from(std::mem::take(&mut current_spans)));
                        }
                    }
                    _ => {}
                },
                Event::Text(text) => {
                    let base = *style_stack.last().unwrap_or(&Style::default());
                    if in_code_block {
                        code_text.push_str(&text);
                    } else {
                        current_spans.push(Span::styled(text.to_string(), base));
                    }
                }
                Event::Code(text) => {
                    let base = *style_stack.last().unwrap_or(&Style::default());
                    current_spans.push(Span::styled(
                        format!("`{text}`"),
                        base.add_modifier(Modifier::BOLD),
                    ));
                }
                Event::SoftBreak => {
                    if soft_break_as_space {
                        current_spans.push(Span::raw(" ".to_string()));
                    } else {
                        push_line(&mut lines, &mut current_spans);
                    }
                }
                Event::HardBreak => {
                    push_line(&mut lines, &mut current_spans);
                }
                Event::Rule => {
                    blank_line(&mut lines, &mut current_spans);
                    lines.push(Line::from(Span::styled(
                        "─".repeat(72),
                        Style::default().add_modifier(Modifier::DIM),
                    )));
                }
                Event::TaskListMarker(done) => {
                    let marker = if done { "[x] " } else { "[ ] " };
                    current_spans.push(Span::styled(
                        marker.to_string(),
                        Style::default().add_modifier(Modifier::BOLD),
                    ));
                }
                Event::FootnoteReference(name) => {
                    current_spans.push(Span::styled(
                        format!("[^{name}]"),
                        Style::default().add_modifier(Modifier::DIM),
                    ));
                }
                Event::InlineMath(text) | Event::DisplayMath(text) => {
                    current_spans.push(Span::styled(
                        format!("${text}$"),
                        Style::default().add_modifier(Modifier::ITALIC),
                    ));
                }
                Event::Html(_) | Event::InlineHtml(_) => {}
            }
        }
    }

    if !current_spans.is_empty() {
        push_line(&mut lines, &mut current_spans);
    }

    if let Some((src, alt_raw)) = pending_image.take() {
        append_image_entry(&mut lines, &mut images, src, alt_raw, None, false);
    }

    while lines.last().map(|l| l.spans.is_empty()).unwrap_or(false) {
        lines.pop();
    }

    if lines.is_empty() {
        lines.push(Line::from(Span::styled(
            "(empty markdown file)".to_string(),
            Style::default().add_modifier(Modifier::DIM),
        )));
    }

    Ok(RenderedDoc {
        lines,
        images,
        tables,
        code_blocks,
    })
}

fn code_highlight_assets() -> &'static CodeHighlightAssets {
    static ASSETS: OnceLock<CodeHighlightAssets> = OnceLock::new();

    ASSETS.get_or_init(|| {
        let syntax_set = SyntaxSet::load_defaults_newlines();
        let theme_set = ThemeSet::load_defaults();
        let theme = theme_set
            .themes
            .get("base16-ocean.dark")
            .or_else(|| theme_set.themes.values().next())
            .cloned()
            .expect("syntect bundled themes are available");

        CodeHighlightAssets { syntax_set, theme }
    })
}

fn extract_code_block_language_token(info: &str) -> Option<&str> {
    let first = info.split_whitespace().next().unwrap_or_default().trim();
    if first.is_empty() {
        return None;
    }
    let token = first
        .split([',', '{'])
        .next()
        .unwrap_or_default()
        .trim_matches('.');
    if token.is_empty() {
        None
    } else {
        Some(token)
    }
}

fn display_src_label(src: &str) -> String {
    const MAX_URL_DISPLAY_CHARS: usize = 96;
    let src_chars = src.chars().count();
    if src_chars <= MAX_URL_DISPLAY_CHARS {
        src.to_string()
    } else {
        let truncated = src.chars().take(MAX_URL_DISPLAY_CHARS).collect::<String>();
        format!("{truncated}...")
    }
}

fn lookup_syntax_for_language<'a>(
    syntax_set: &'a SyntaxSet,
    language: &str,
) -> Option<&'a syntect::parsing::SyntaxReference> {
    syntax_set
        .find_syntax_by_token(language)
        .or_else(|| syntax_set.find_syntax_by_extension(language))
        .or_else(|| syntax_set.find_syntax_by_name(language))
}

fn new_code_highlighter(language: Option<&str>) -> Option<HighlightLines<'static>> {
    let language = language?;
    let assets = code_highlight_assets();
    let syntax = lookup_syntax_for_language(&assets.syntax_set, language)?;
    Some(HighlightLines::new(syntax, &assets.theme))
}

fn append_code_block_text(
    text: &str,
    current_spans: &mut Vec<Span<'static>>,
    lines: &mut Vec<Line<'static>>,
    active_code_highlighter: &mut Option<HighlightLines<'static>>,
    base_style: Style,
) {
    let prefix_style = base_style.add_modifier(Modifier::DIM);
    for line in text.split_inclusive('\n') {
        let line = line.strip_suffix('\n').unwrap_or(line);
        let line = expand_tabs(line, 4);
        let highlighted = active_code_highlighter
            .as_mut()
            .map(|highlighter| highlight_code_line(&format!("{line}\n"), highlighter))
            .unwrap_or_default();
        if !line.is_empty() {
            current_spans.push(Span::styled("  ".to_string(), prefix_style));
            if highlighted.is_empty() {
                current_spans.push(Span::styled(line.to_string(), prefix_style));
            } else {
                current_spans.extend(highlighted);
            }
        }
        push_line_or_blank(lines, current_spans);
    }
}

fn highlight_code_line(
    line: &str,
    highlighter: &mut HighlightLines<'static>,
) -> Vec<Span<'static>> {
    let assets = code_highlight_assets();
    match highlighter.highlight_line(line, &assets.syntax_set) {
        Ok(highlighted) => highlighted
            .into_iter()
            .filter_map(|(style, text)| {
                let text = text.trim_end_matches('\n');
                (!text.is_empty())
                    .then(|| Span::styled(text.to_string(), syntect_style_to_ratatui(style)))
            })
            .collect(),
        Err(_) => Vec::new(),
    }
}

fn syntect_style_to_ratatui(style: SyntectStyle) -> Style {
    let mut out = Style::default().fg(Color::Rgb(
        style.foreground.r,
        style.foreground.g,
        style.foreground.b,
    ));
    if style.font_style.contains(FontStyle::BOLD) {
        out = out.add_modifier(Modifier::BOLD);
    }
    if style.font_style.contains(FontStyle::ITALIC) {
        out = out.add_modifier(Modifier::ITALIC);
    }
    if style.font_style.contains(FontStyle::UNDERLINE) {
        out = out.add_modifier(Modifier::UNDERLINED);
    }
    out
}

fn push_line_or_blank(lines: &mut Vec<Line<'static>>, spans: &mut Vec<Span<'static>>) {
    if spans.is_empty() {
        lines.push(Line::default());
    } else {
        lines.push(Line::from(std::mem::take(spans)));
    }
}

pub fn read_markdown_file(path: &std::path::Path) -> Result<String> {
    std::fs::read_to_string(path)
        .with_context(|| format!("failed to read file: {}", path.display()))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn text(input: &str) -> Vec<String> {
        render_markdown(input)
            .unwrap()
            .lines
            .iter()
            .map(ToString::to_string)
            .collect()
    }

    #[test]
    fn tables_have_aligned_columns_distinct_headers_and_empty_cells() {
        let markdown = "| Name | Count |\n| :--- | ---: |\n| 界 | 12 |\n| longer | |";
        assert_eq!(
            text(markdown),
            [
                "┌────────┬───────┐",
                "│ Name   │ Count │",
                "├────────┼───────┤",
                "│ 界     │    12 │",
                "│ longer │       │",
                "└────────┴───────┘",
            ]
        );
        let doc = render_markdown(markdown).unwrap();
        assert!(doc
            .lines
            .iter()
            .all(|line| line.width() == doc.lines[0].width()));
        assert!(
            doc.lines[1]
                .spans
                .iter()
                .any(|span| span.content == "Name"
                    && span.style.add_modifier.contains(Modifier::BOLD))
        );
    }

    #[test]
    fn table_cells_retain_inline_formatting() {
        let doc = render_markdown("| A | B |\n|---|:---:|\n| **bold** | *italics* |").unwrap();
        assert!(
            doc.lines[3]
                .spans
                .iter()
                .any(|span| span.content == "bold"
                    && span.style.add_modifier.contains(Modifier::BOLD))
        );
        assert!(doc.lines[3]
            .spans
            .iter()
            .any(|span| span.content == "italics"
                && span.style.add_modifier.contains(Modifier::ITALIC)));
    }

    #[test]
    fn code_block_ranges_include_fences_and_indented_blocks_only() {
        let doc =
            render_markdown("before\n\n```text\nwide code\n```\n\nafter\n\n    indented code\n")
                .unwrap();
        assert_eq!(doc.code_blocks.len(), 2);
        for block in &doc.code_blocks {
            assert!(doc.lines[block.start].to_string().starts_with("```"));
            assert_eq!(doc.lines[block.end - 1].to_string(), "```");
            assert_eq!(block.len(), 3);
        }
    }

    #[test]
    fn code_blocks_preserve_blank_lines_and_expand_tabs() {
        assert_eq!(
            text("```text\nfirst\n\n\tthird\n```"),
            ["```text", "  first", "", "      third", "```"]
        );
        assert_eq!(text("```\n```"), ["```", "```"]);
    }

    #[test]
    fn code_highlighting_resets_single_line_comments() {
        let doc = render_markdown("```rust\n// comment\nlet value = 42;\n```").unwrap();
        let code: Vec<_> = doc.lines[2]
            .spans
            .iter()
            .filter(|s| !s.content.trim().is_empty())
            .collect();
        assert!(
            code.len() > 1,
            "code after a comment must still be tokenized"
        );
        assert!(code.iter().any(|s| s.style != doc.lines[1].spans[1].style));
    }

    #[test]
    fn nested_lists_do_not_insert_blank_rows_between_items() {
        assert_eq!(
            text("- parent\n  - child\n- sibling"),
            ["• parent", "  • child", "• sibling"]
        );
    }

    #[test]
    fn quotes_decorate_nested_blocks_and_inline_code() {
        let lines = text("> `code`  \n> next\n>\n> > nested\n>\n> - item");
        assert_eq!(lines[0], "│ `code`");
        assert!(lines.iter().any(|line| line == "│ next"));
        assert!(lines.iter().any(|line| line == "│ │ nested"));
        assert!(lines.iter().any(|line| line == "│ • item"));
    }

    #[test]
    fn autolinks_are_not_duplicated() {
        assert_eq!(text("<https://example.com>"), ["https://example.com"]);
        assert_eq!(
            text("[site](https://example.com)"),
            ["site (https://example.com)"]
        );
        render_markdown("[![preview](image.png)](https://example.com)").unwrap();
    }

    #[test]
    fn markdown_images_are_extracted_for_runtime_rendering() {
        let doc = render_markdown("![Alt Text](images/sample.png)").expect("render succeeds");
        assert_eq!(doc.images.len(), 1);
        assert_eq!(doc.images[0].src, "images/sample.png");
        assert_eq!(doc.images[0].hinted_pixel_size, None);

        let caption_line = &doc.lines[doc.images[0].line_index];
        let caption_text = caption_line
            .spans
            .iter()
            .map(|span| span.content.as_ref())
            .collect::<String>();
        assert!(caption_text.contains("[image]"));
    }

    #[test]
    fn html_img_tags_are_extracted_for_runtime_rendering() {
        let input = r#"<img alt="Preview" width="1708" height="1040" src="https://example.com/preview.png" />"#;
        let doc = render_markdown(input).expect("render succeeds");
        assert_eq!(doc.images.len(), 1);
        assert_eq!(doc.images[0].src, "https://example.com/preview.png");
        assert_eq!(doc.images[0].hinted_pixel_size, Some((1708, 1040)));

        let caption_line = &doc.lines[doc.images[0].line_index];
        let caption_text = caption_line
            .spans
            .iter()
            .map(|span| span.content.as_ref())
            .collect::<String>();
        assert!(caption_text.contains("Preview"));
    }

    #[test]
    fn long_image_urls_are_truncated_in_caption_display() {
        let input = "![](https://example.com/this/is/a/very/long/path/that/should/be/truncated/when/rendered/in/the/caption/to/avoid/wrapping/issues.png)";
        let doc = render_markdown(input).expect("render succeeds");
        let caption_line = &doc.lines[doc.images[0].line_index];
        let caption_text = caption_line
            .spans
            .iter()
            .map(|span| span.content.as_ref())
            .collect::<String>();

        assert!(caption_text.contains("..."));
        assert!(caption_text.contains("[image]"));
        assert!(doc.images[0]
            .src
            .contains("very/long/path/that/should/be/truncated"));
    }

    #[test]
    fn fenced_code_block_with_known_language_is_syntax_highlighted() {
        let input = "```rust\nfn main() { println!(\"hi\"); }\n```";
        let doc = render_markdown(input).expect("render succeeds");

        let code_line = doc
            .lines
            .iter()
            .find(|line| {
                line.spans
                    .iter()
                    .map(|span| span.content.as_ref())
                    .collect::<String>()
                    .contains("fn main()")
            })
            .expect("code line should exist");

        assert!(
            code_line
                .spans
                .iter()
                .any(|span| matches!(span.style.fg, Some(Color::Rgb(_, _, _)))),
            "expected at least one syntax-colored span"
        );
    }

    #[test]
    fn fenced_code_block_with_unknown_language_falls_back_to_dim_text() {
        let input = "```unknown-lang\nlet value = 42;\n```";
        let doc = render_markdown(input).expect("render succeeds");

        let code_span = doc
            .lines
            .iter()
            .flat_map(|line| line.spans.iter())
            .find(|span| span.content.as_ref().contains("let value = 42;"))
            .expect("fallback code span should exist");

        assert!(code_span.style.add_modifier.contains(Modifier::DIM));
    }

    #[test]
    fn html_blocks_render_like_markdown() {
        let doc = render_markdown(
            "<h2>Title</h2>\n<p>Some <b>bold</b> &amp; <code>code</code><br>next</p>\n<ul>\n  <li>one\n  <li>two</li>\n</ul>\n<!-- hidden -->",
        )
        .unwrap();
        let lines: Vec<String> = doc.lines.iter().map(ToString::to_string).collect();
        assert_eq!(
            lines,
            [
                "Title",
                "",
                "Some bold & `code`",
                "next",
                "",
                "• one",
                "• two"
            ]
        );
        assert!(doc.lines[2]
            .spans
            .iter()
            .any(|s| s.content.trim() == "bold" && s.style.add_modifier.contains(Modifier::BOLD)));
    }

    #[test]
    fn inline_html_styles_and_links() {
        assert_eq!(
            text("Press <kbd>q</kbd> to <a href=\"https://x.y\">quit</a> <!-- c -->now"),
            ["Press  q  to quit (https://x.y) now"]
        );
        let doc = render_markdown("a <u>b</u> c").unwrap();
        assert!(doc.lines[0]
            .spans
            .iter()
            .any(|s| s.content == "b" && s.style.add_modifier.contains(Modifier::UNDERLINED)));
        assert!(!doc.lines[0]
            .spans
            .iter()
            .any(|s| s.content == " c" && s.style.add_modifier.contains(Modifier::UNDERLINED)));
    }

    #[test]
    fn html_alignment_spans_markdown_blocks() {
        let doc = render_markdown(
            "<div align=\"center\">\n\n# Title\n\ntext\n\n</div>\n\nafter\n\n<p align=\"right\">r</p>",
        )
        .unwrap();
        let find = |t: &str| {
            doc.lines
                .iter()
                .find(|l| l.to_string() == t)
                .unwrap()
                .alignment
        };
        assert_eq!(find("Title"), Some(LineAlignment::Center));
        assert_eq!(find("text"), Some(LineAlignment::Center));
        assert_eq!(find("after"), None);
        assert_eq!(find("r"), Some(LineAlignment::Right));
    }

    #[test]
    fn html_tables_and_details() {
        assert_eq!(
            text("<table>\n<tr><th>Key<th>Action</tr>\n<tr><td><kbd>j</kbd></td><td>down<br>one</td></tr>\n</table>"),
            [
                "┌─────┬──────────┐",
                "│ Key │ Action   │",
                "├─────┼──────────┤",
                "│  j  │ down one │",
                "└─────┴──────────┘",
            ]
        );
        assert_eq!(
            text("<details>\n<summary>More</summary>\n\nHidden *text*\n\n</details>"),
            ["▼ More", "", "Hidden text"]
        );
    }

    #[test]
    fn html_pre_preserves_whitespace() {
        assert_eq!(
            text("<pre lang=\"rust\">\nfn  main() {\n    &lt;x&gt;\n}\n</pre>"),
            ["```rust", "  fn  main() {", "      <x>", "  }", "```"]
        );
    }
}
