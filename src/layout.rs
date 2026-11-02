use std::ops::Range;

use ratatui::text::{Line, Text};
use unicode_segmentation::UnicodeSegmentation;
use unicode_width::UnicodeWidthStr;

/// Wrap once, before navigation and image placement, using terminal cell widths.
/// Byte ranges retain the relationship between the document and displayed rows.
pub fn wrapped_ranges(text: &str, width: u16) -> Vec<Range<usize>> {
    let width = usize::from(width.max(1));
    let mut ranges = Vec::new();
    let mut start = 0;
    while start < text.len() {
        let mut cells = 0;
        let mut end = start;
        let mut break_at = None;
        let mut has_text = false;
        for (offset, grapheme) in text[start..].grapheme_indices(true) {
            let next = start + offset + grapheme.len();
            if cells + grapheme.width() > width && end > start {
                break;
            }
            cells += grapheme.width();
            end = next;
            if grapheme.chars().all(char::is_whitespace) {
                if has_text {
                    break_at = Some(end);
                }
            } else {
                has_text = true;
            }
            if cells > width {
                // Even a one-cell terminal must make progress over a wide glyph.
                break;
            }
        }
        if end < text.len() {
            end = break_at.unwrap_or(end);
        }
        ranges.push(start..end);
        start = end;
    }
    if ranges.is_empty() {
        ranges.push(0..0);
    }
    ranges
}

pub fn wrap_line(line: &Line<'static>, width: u16) -> Vec<Line<'static>> {
    let text: String = line
        .spans
        .iter()
        .map(|span| span.content.as_ref())
        .collect();
    wrapped_ranges(&text, width)
        .into_iter()
        .map(|range| {
            let mut row = Line::default().style(line.style);
            row.alignment = line.alignment;
            let mut offset = 0;
            for span in &line.spans {
                let end = offset + span.content.len();
                let from = offset.max(range.start);
                let to = end.min(range.end);
                if from < to {
                    let mut part = span.clone();
                    part.content = span.content[from - offset..to - offset].to_string().into();
                    row.spans.push(part);
                }
                offset = end;
            }
            row
        })
        .collect()
}

pub fn viewport_text(lines: Vec<Line<'static>>, scroll: usize, height: usize) -> Text<'static> {
    Text::from(
        lines
            .into_iter()
            .skip(scroll)
            .take(height)
            .collect::<Vec<_>>(),
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use ratatui::{
        style::{Modifier, Style},
        text::Span,
    };

    #[test]
    fn wraps_words_without_losing_text_or_styles() {
        let style = Style::default().add_modifier(Modifier::BOLD);
        let line = Line::from(vec![Span::raw("one "), Span::styled("two three", style)]);
        let rows = wrap_line(&line, 8);
        assert_eq!(
            rows.iter().map(ToString::to_string).collect::<Vec<_>>(),
            ["one two ", "three"]
        );
        assert_eq!(rows[1].spans[0].style, style);
    }

    #[test]
    fn preserves_graphemes_and_counts_wide_characters() {
        for text in ["界界界", "e\u{301}e\u{301}e\u{301}", "👩‍💻👩‍💻"] {
            let ranges = wrapped_ranges(text, 2);
            assert!(ranges.iter().all(|r| text[r.clone()].width() <= 2));
            assert_eq!(
                ranges.iter().map(|r| &text[r.clone()]).collect::<String>(),
                text
            );
        }
    }

    #[test]
    fn tiny_widths_and_long_words_make_progress() {
        let empty = wrapped_ranges("", 0);
        assert_eq!(empty.len(), 1);
        assert_eq!(empty[0], 0..0);
        assert_eq!(wrapped_ranges("界a", 0), [0..3, 3..4]);
        assert_eq!(wrapped_ranges("abcdef", 2), [0..2, 2..4, 4..6]);
    }

    #[test]
    fn terminal_buffer_renders_wrapped_rows_once() {
        use ratatui::{backend::TestBackend, widgets::Paragraph, Terminal};
        let source = Line::from("alpha beta gamma delta");
        let rows = wrap_line(&source, 11);
        let mut terminal = Terminal::new(TestBackend::new(11, 2)).unwrap();
        terminal
            .draw(|frame| {
                frame.render_widget(
                    Paragraph::new(viewport_text(rows.clone(), 0, 2)),
                    frame.area(),
                )
            })
            .unwrap();
        terminal
            .backend()
            .assert_buffer_lines(["alpha beta ", "gamma delta"]);
    }

    #[test]
    fn scrolling_beyond_u16_limit_keeps_the_correct_document_rows() {
        use ratatui::{backend::TestBackend, widgets::Paragraph, Terminal};
        let lines = (0..65_540)
            .map(|i| Line::from(format!("row {i}")))
            .collect();
        let text = viewport_text(lines, 65_536, 2);
        let mut terminal = Terminal::new(TestBackend::new(10, 2)).unwrap();
        terminal
            .draw(|frame| frame.render_widget(Paragraph::new(text.clone()), frame.area()))
            .unwrap();
        terminal
            .backend()
            .assert_buffer_lines(["row 65536 ", "row 65537 "]);
    }
}
