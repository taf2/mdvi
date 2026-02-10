use anyhow::{Context, Result};
use pulldown_cmark::{CodeBlockKind, Event, HeadingLevel, Options, Parser, Tag, TagEnd};
use ratatui::{
    style::{Modifier, Style},
    text::{Line, Span},
};

#[derive(Debug, Clone)]
pub struct RenderedDoc {
    pub lines: Vec<Line<'static>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ListKind {
    Ordered(u64),
    Unordered,
}

pub fn render_markdown(input: &str) -> Result<RenderedDoc> {
    let mut options = Options::empty();
    options.insert(Options::ENABLE_TABLES);
    options.insert(Options::ENABLE_FOOTNOTES);
    options.insert(Options::ENABLE_STRIKETHROUGH);
    options.insert(Options::ENABLE_TASKLISTS);

    let parser = Parser::new_ext(input, options);

    let mut lines: Vec<Line<'static>> = Vec::new();
    let mut current_spans: Vec<Span<'static>> = Vec::new();

    let mut style_stack = vec![Style::default()];
    let mut list_stack: Vec<ListKind> = Vec::new();
    let mut in_code_block = false;
    let mut in_blockquote = 0usize;
    let mut pending_link: Option<String> = None;
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
        if !lines.last().map(|l| l.spans.is_empty()).unwrap_or(false) {
            lines.push(Line::default());
        }
    }

    fn indent_for_lists(list_stack: &[ListKind]) -> String {
        if list_stack.is_empty() {
            return String::new();
        }
        "  ".repeat(list_stack.len().saturating_sub(1))
    }

    for event in parser {
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
                    in_blockquote += 1;
                    if current_spans.is_empty() {
                        let prefix = format!("{}│ ", "  ".repeat(in_blockquote.saturating_sub(1)));
                        current_spans.push(Span::styled(
                            prefix,
                            Style::default().add_modifier(Modifier::DIM),
                        ));
                    }
                }
                Tag::CodeBlock(kind) => {
                    blank_line(&mut lines, &mut current_spans);
                    in_code_block = true;
                    let code_block_lang = match kind {
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
                    let header = match &code_block_lang {
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
                    pending_link = Some(dest_url.to_string());
                }
                Tag::Image { dest_url, .. } => {
                    let base = *style_stack.last().unwrap_or(&Style::default());
                    style_stack.push(base.add_modifier(Modifier::BOLD));
                    current_spans.push(Span::styled("[image: ".to_string(), base));
                    current_spans.push(Span::styled(
                        dest_url.to_string(),
                        base.add_modifier(Modifier::UNDERLINED),
                    ));
                    current_spans.push(Span::styled("]".to_string(), base));
                }
                Tag::Table(_) => {
                    blank_line(&mut lines, &mut current_spans);
                }
                Tag::TableHead => {}
                Tag::TableRow => {}
                Tag::TableCell => {
                    if !current_spans.is_empty() {
                        current_spans.push(Span::raw(" │ ".to_string()));
                    }
                }
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
                    in_blockquote = in_blockquote.saturating_sub(1);
                    lines.push(Line::default());
                }
                TagEnd::CodeBlock => {
                    if !current_spans.is_empty() {
                        push_line(&mut lines, &mut current_spans);
                    }
                    lines.push(Line::from(Span::styled(
                        "```".to_string(),
                        Style::default().add_modifier(Modifier::DIM),
                    )));
                    lines.push(Line::default());
                    in_code_block = false;
                }
                TagEnd::List(_) => {
                    list_stack.pop();
                    lines.push(Line::default());
                }
                TagEnd::Item => {
                    push_line(&mut lines, &mut current_spans);
                }
                TagEnd::Emphasis | TagEnd::Strong | TagEnd::Strikethrough => {
                    style_stack.pop();
                }
                TagEnd::Link => {
                    style_stack.pop();
                    if let Some(link) = pending_link.take() {
                        current_spans.push(Span::styled(
                            format!(" ({link})"),
                            Style::default().add_modifier(Modifier::DIM),
                        ));
                    }
                }
                TagEnd::Image => {
                    style_stack.pop();
                }
                TagEnd::Table => {
                    if !current_spans.is_empty() {
                        push_line(&mut lines, &mut current_spans);
                    }
                    lines.push(Line::default());
                }
                TagEnd::TableRow => {
                    push_line(&mut lines, &mut current_spans);
                }
                TagEnd::TableCell => {}
                _ => {}
            },
            Event::Text(text) => {
                let base = *style_stack.last().unwrap_or(&Style::default());
                if in_code_block {
                    let text = text.to_string();
                    for line in text.split('\n') {
                        if !line.is_empty() {
                            current_spans.push(Span::styled(
                                format!("  {line}"),
                                base.add_modifier(Modifier::DIM),
                            ));
                        }
                        push_line(&mut lines, &mut current_spans);
                    }
                } else {
                    if current_spans.is_empty() && in_blockquote > 0 {
                        let prefix = format!("{}│ ", "  ".repeat(in_blockquote.saturating_sub(1)));
                        current_spans.push(Span::styled(
                            prefix,
                            Style::default().add_modifier(Modifier::DIM),
                        ));
                    }
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
            Event::Html(text) => {
                current_spans.push(Span::styled(
                    text.to_string(),
                    Style::default().add_modifier(Modifier::DIM),
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
                push_line(&mut lines, &mut current_spans);
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
            Event::InlineHtml(text) => {
                current_spans.push(Span::styled(
                    text.to_string(),
                    Style::default().add_modifier(Modifier::DIM),
                ));
            }
        }
    }

    if !current_spans.is_empty() {
        push_line(&mut lines, &mut current_spans);
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

    Ok(RenderedDoc { lines })
}

pub fn read_markdown_file(path: &std::path::Path) -> Result<String> {
    std::fs::read_to_string(path)
        .with_context(|| format!("failed to read file: {}", path.display()))
}
