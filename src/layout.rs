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

/// Crop by terminal cells, padding partial wide glyphs to keep columns aligned.
pub fn clip_line(line: &Line<'static>, left: usize, width: usize) -> Line<'static> {
    let text: String = line
        .spans
        .iter()
        .map(|span| span.content.as_ref())
        .collect();
    let mut out = Line::default().style(line.style);
    let right = left.saturating_add(width);
    let mut column = 0;
    let mut span_index = 0;
    let mut span_start = 0;
    for (byte, grapheme) in text.grapheme_indices(true) {
        let end_column = column + grapheme.width();
        if column >= right {
            break;
        }
        if end_column > left {
            if column < left || end_column > right {
                out.spans.push(ratatui::text::Span::raw(
                    " ".repeat(end_column.min(right) - column.max(left)),
                ));
            } else {
                let end = byte + grapheme.len();
                while span_index < line.spans.len() {
                    let span = &line.spans[span_index];
                    let span_end = span_start + span.content.len();
                    let from = byte.max(span_start);
                    let to = end.min(span_end);
                    if from < to {
                        out.spans.push(ratatui::text::Span::styled(
                            text[from..to].to_string(),
                            span.style,
                        ));
                    }
                    if span_end >= end {
                        break;
                    }
                    span_start = span_end;
                    span_index += 1;
                }
            }
        }
        column = end_column;
    }
    out
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
    fn horizontal_clipping_preserves_cells_and_styles() {
        let style = Style::default().add_modifier(Modifier::BOLD);
        let line = Line::from(vec![Span::raw("ab"), Span::styled("界e\u{301}xyz", style)]);
        let clipped = clip_line(&line, 3, 4);
        assert_eq!(clipped.to_string(), " e\u{301}xy");
        assert_eq!(clipped.width(), 4);
        assert_eq!(clipped.spans.last().unwrap().style, style);
        assert_eq!(clip_line(&line, 1, 2).to_string(), "b ");
    }

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
