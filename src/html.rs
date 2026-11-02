//! A deliberately small HTML renderer for the HTML commonly embedded in markdown
//! (READMEs in particular). HTML is tokenized and translated into the same events
//! the markdown renderer already understands, so `<b>`, `<h1>`, `<ul>`, `<table>`
//! and friends look identical to their markdown equivalents. Anything unknown is
//! rendered transparently: the tags disappear and their text remains.

use pulldown_cmark::{CodeBlockKind, CowStr, Event, HeadingLevel, LinkType, Tag, TagEnd};
use ratatui::{layout::Alignment, style::Modifier};

#[derive(Debug, Clone, PartialEq)]
pub enum RenderEvent<'a> {
    Md(Event<'a>),
    /// Finish the current line, if it has content.
    BlockBreak,
    /// Finish the current line and leave a blank line after it.
    BlockEnd,
    AlignStart(Alignment),
    AlignEnd,
    StyleStart(Modifier),
    StyleEnd,
    Image {
        src: String,
        alt: String,
        hinted_pixel_size: Option<(u32, u32)>,
    },
}

#[derive(Debug, Clone, PartialEq)]
enum Token {
    Start {
        name: String,
        attrs: Vec<(String, String)>,
        self_closing: bool,
    },
    End(String),
    Text(String),
}

struct OpenElement {
    name: String,
    closers: Vec<RenderEvent<'static>>,
    /// Opened from inline HTML inside a markdown block; closed when that block ends.
    inline: bool,
}

#[derive(Default)]
pub struct HtmlTranslator {
    stack: Vec<OpenElement>,
    pending_space: bool,
    at_block_start: bool,
    pre_depth: usize,
    pre_fresh: bool,
}

const VOID: &[&str] = &[
    "area", "base", "br", "col", "embed", "hr", "img", "input", "link", "meta", "param", "source",
    "track", "wbr",
];

const SKIP_CONTENT: &[&str] = &["script", "style", "title", "template", "head", "textarea"];

const BLOCK: &[&str] = &[
    "address",
    "article",
    "aside",
    "blockquote",
    "center",
    "details",
    "div",
    "dl",
    "fieldset",
    "figcaption",
    "figure",
    "footer",
    "form",
    "h1",
    "h2",
    "h3",
    "h4",
    "h5",
    "h6",
    "header",
    "hr",
    "main",
    "nav",
    "ol",
    "p",
    "pre",
    "section",
    "table",
    "ul",
];

impl HtmlTranslator {
    pub fn new() -> Self {
        Self {
            at_block_start: true,
            ..Self::default()
        }
    }

    /// Called at the edges of a markdown HTML block so whitespace does not leak
    /// between unrelated blocks.
    pub fn block_boundary<'a>(&mut self, out: &mut Vec<RenderEvent<'a>>) {
        self.pending_space = false;
        self.at_block_start = true;
        out.push(RenderEvent::BlockBreak);
    }

    /// Close inline elements left open when the markdown block containing them ends.
    pub fn close_inline<'a>(&mut self, out: &mut Vec<RenderEvent<'a>>) {
        while self.stack.last().is_some_and(|e| e.inline) {
            self.close_through(self.stack.len() - 1, out);
        }
    }

    pub fn finish<'a>(&mut self, out: &mut Vec<RenderEvent<'a>>) {
        if !self.stack.is_empty() {
            self.close_through(0, out);
        }
    }

    pub fn feed<'a>(
        &mut self,
        html: &str,
        inline: bool,
        in_md_table: bool,
        out: &mut Vec<RenderEvent<'a>>,
    ) {
        for token in tokenize(html) {
            match token {
                Token::Text(text) => self.text(&text, in_md_table, out),
                Token::Start {
                    name,
                    attrs,
                    self_closing,
                } => self.start(name, &attrs, self_closing, inline, in_md_table, out),
                Token::End(name) => self.end(&name, in_md_table, out),
            }
        }
    }

    fn position(&self, name: &str) -> Option<usize> {
        self.stack.iter().rposition(|e| e.name == name)
    }

    /// Index of the innermost open element among `names`, if it is `target`.
    fn innermost_is(&self, names: &[&str], target: &[&str]) -> Option<usize> {
        let index = self
            .stack
            .iter()
            .rposition(|e| names.contains(&e.name.as_str()))?;
        target
            .contains(&self.stack[index].name.as_str())
            .then_some(index)
    }

    fn in_cell(&self, in_md_table: bool) -> bool {
        in_md_table
            || self
                .innermost_is(&["td", "th", "table"], &["td", "th"])
                .is_some()
    }

    fn close_through<'a>(&mut self, index: usize, out: &mut Vec<RenderEvent<'a>>) {
        while self.stack.len() > index {
            let element = self.stack.pop().expect("stack is longer than index");
            if element.name == "pre" {
                self.pre_depth = self.pre_depth.saturating_sub(1);
            }
            for closer in element.closers {
                self.note_block_state(&closer);
                out.push(closer);
            }
        }
    }

    fn note_block_state(&mut self, event: &RenderEvent<'_>) {
        let block = match event {
            RenderEvent::Md(Event::Start(Tag::Link { .. }) | Event::End(TagEnd::Link)) => false,
            RenderEvent::Md(Event::Start(_) | Event::End(_) | Event::HardBreak | Event::Rule) => {
                true
            }
            RenderEvent::BlockBreak
            | RenderEvent::BlockEnd
            | RenderEvent::AlignStart(_)
            | RenderEvent::AlignEnd
            | RenderEvent::Image { .. } => true,
            _ => false,
        };
        if block {
            self.at_block_start = true;
            self.pending_space = false;
        }
    }

    fn emit<'a>(&mut self, event: RenderEvent<'static>, out: &mut Vec<RenderEvent<'a>>) {
        self.note_block_state(&event);
        out.push(event);
    }

    fn text<'a>(&mut self, text: &str, in_md_table: bool, out: &mut Vec<RenderEvent<'a>>) {
        if self.pre_depth > 0 {
            let text = if std::mem::take(&mut self.pre_fresh) {
                text.strip_prefix("\r\n")
                    .or_else(|| text.strip_prefix('\n'))
                    .unwrap_or(text)
            } else {
                text
            };
            if !text.is_empty() {
                out.push(RenderEvent::Md(Event::Text(CowStr::from(text.to_string()))));
            }
            return;
        }
        // Stray text between table rows/cells would otherwise leak into a cell.
        if !in_md_table
            && self
                .innermost_is(&["td", "th", "table"], &["table"])
                .is_some()
        {
            return;
        }
        let mut collapsed = String::new();
        let leading = text.starts_with(|c: char| c.is_ascii_whitespace());
        for word in text.split_ascii_whitespace() {
            if !collapsed.is_empty() {
                collapsed.push(' ');
            }
            collapsed.push_str(word);
        }
        if collapsed.is_empty() {
            if !text.is_empty() {
                self.pending_space = true;
            }
            return;
        }
        let mut chunk = String::new();
        if (leading || self.pending_space) && !self.at_block_start {
            chunk.push(' ');
        }
        chunk.push_str(&collapsed);
        self.pending_space = text.ends_with(|c: char| c.is_ascii_whitespace());
        self.at_block_start = false;
        out.push(RenderEvent::Md(Event::Text(CowStr::from(chunk))));
    }

    fn start<'a>(
        &mut self,
        name: String,
        attrs: &[(String, String)],
        self_closing: bool,
        inline: bool,
        in_md_table: bool,
        out: &mut Vec<RenderEvent<'a>>,
    ) {
        let attr = |key: &str| {
            attrs
                .iter()
                .find(|(k, _)| k == key)
                .map(|(_, v)| v.as_str())
        };

        // Implied end tags, as browsers apply them.
        if BLOCK.contains(&name.as_str()) {
            if let Some(index) = self.position("p") {
                self.close_through(index, out);
            }
        }
        let implied = match name.as_str() {
            "li" => self.innermost_is(&["li", "ul", "ol"], &["li"]),
            "td" | "th" => self.innermost_is(&["td", "th", "tr", "table"], &["td", "th"]),
            "tr" => self.innermost_is(&["tr", "table"], &["tr"]),
            "dt" | "dd" => self.innermost_is(&["dt", "dd", "dl"], &["dt", "dd"]),
            _ => None,
        };
        if let Some(index) = implied {
            self.close_through(index, out);
        }

        let in_cell = self.in_cell(in_md_table);
        let mut closers: Vec<RenderEvent<'static>> = Vec::new();
        let mut opened: Vec<RenderEvent<'static>> = Vec::new();

        let style = |modifier: Modifier,
                     opened: &mut Vec<RenderEvent<'static>>,
                     closers: &mut Vec<RenderEvent<'static>>| {
            opened.push(RenderEvent::StyleStart(modifier));
            closers.push(RenderEvent::StyleEnd);
        };
        let text = |s: &str| RenderEvent::Md(Event::Text(CowStr::from(s.to_string())));

        match name.as_str() {
            "br" => {
                if in_cell {
                    self.pending_space = true;
                } else {
                    self.emit(RenderEvent::Md(Event::HardBreak), out);
                }
                return;
            }
            "hr" => {
                if !in_cell {
                    self.emit(RenderEvent::Md(Event::Rule), out);
                    self.emit(RenderEvent::BlockEnd, out);
                }
                return;
            }
            "img" => {
                let Some(src) = attr("src").filter(|s| !s.is_empty()) else {
                    return;
                };
                let alt = attr("alt").unwrap_or_default().trim().to_string();
                if in_cell {
                    let label = if alt.is_empty() { "image" } else { &alt };
                    self.emit_inline_text(&format!("[{label}]"), out);
                } else {
                    let dimension = |key: &str| {
                        attr(key)
                            .map(|v| v.trim().trim_end_matches("px"))
                            .and_then(|v| v.parse::<u32>().ok())
                            .filter(|v| *v > 0)
                    };
                    let hinted_pixel_size = dimension("width").zip(dimension("height"));
                    self.emit(
                        RenderEvent::Image {
                            src: src.to_string(),
                            alt,
                            hinted_pixel_size,
                        },
                        out,
                    );
                }
                return;
            }
            "input" => {
                if attr("type").is_some_and(|t| t.eq_ignore_ascii_case("checkbox")) {
                    let marker = if attr("checked").is_some() {
                        "[x]"
                    } else {
                        "[ ]"
                    };
                    self.emit_inline_text(marker, out);
                    self.pending_space = true;
                }
                return;
            }
            _ if VOID.contains(&name.as_str()) => return,
            _ => {}
        }

        if self_closing {
            return;
        }

        // Block elements cannot be laid out inside a table cell row; keep only
        // their inline content there.
        let block_allowed = !in_cell;

        if block_allowed && BLOCK.contains(&name.as_str()) {
            let align = if name == "center" {
                Some(Alignment::Center)
            } else {
                match attr("align").map(str::to_ascii_lowercase).as_deref() {
                    Some("center" | "middle") => Some(Alignment::Center),
                    Some("right") => Some(Alignment::Right),
                    _ => None,
                }
            };
            if let Some(align) = align {
                opened.push(RenderEvent::AlignStart(align));
            }
        }

        match name.as_str() {
            "b" | "strong" => style(Modifier::BOLD, &mut opened, &mut closers),
            "i" | "em" | "cite" | "var" | "dfn" | "q" => {
                style(Modifier::ITALIC, &mut opened, &mut closers)
            }
            "u" | "ins" | "abbr" => style(Modifier::UNDERLINED, &mut opened, &mut closers),
            "s" | "strike" | "del" => style(Modifier::CROSSED_OUT, &mut opened, &mut closers),
            "mark" => style(Modifier::REVERSED, &mut opened, &mut closers),
            "small" | "sub" | "sup" => style(Modifier::DIM, &mut opened, &mut closers),
            "code" | "tt" | "samp" if self.pre_depth == 0 => {
                style(Modifier::BOLD, &mut opened, &mut closers);
                opened.push(text("`"));
                closers.insert(0, text("`"));
            }
            "kbd" => {
                style(Modifier::REVERSED, &mut opened, &mut closers);
                opened.push(text(" "));
                closers.insert(0, text(" "));
            }
            "a" => {
                if let Some(href) = attr("href").filter(|h| !h.is_empty()) {
                    opened.push(RenderEvent::Md(Event::Start(Tag::Link {
                        link_type: LinkType::Inline,
                        dest_url: CowStr::from(href.to_string()),
                        title: CowStr::from(attr("title").unwrap_or_default().to_string()),
                        id: CowStr::from(String::new()),
                    })));
                    closers.push(RenderEvent::Md(Event::End(TagEnd::Link)));
                }
            }
            "h1" | "h2" | "h3" | "h4" | "h5" | "h6" if block_allowed => {
                let level = match name.as_str() {
                    "h1" => HeadingLevel::H1,
                    "h2" => HeadingLevel::H2,
                    "h3" => HeadingLevel::H3,
                    "h4" => HeadingLevel::H4,
                    "h5" => HeadingLevel::H5,
                    _ => HeadingLevel::H6,
                };
                opened.push(RenderEvent::Md(Event::Start(Tag::Heading {
                    level,
                    id: None,
                    classes: Vec::new(),
                    attrs: Vec::new(),
                })));
                closers.push(RenderEvent::Md(Event::End(TagEnd::Heading(level))));
            }
            "h1" | "h2" | "h3" | "h4" | "h5" | "h6" | "th" if in_cell => {
                style(Modifier::BOLD, &mut opened, &mut closers)
            }
            "p" if block_allowed => {
                opened.insert(0, RenderEvent::BlockBreak);
                closers.push(RenderEvent::BlockEnd);
            }
            "ul" | "ol" if block_allowed => {
                let ordered = name == "ol";
                let start = attr("start")
                    .and_then(|s| s.trim().parse::<u64>().ok())
                    .unwrap_or(1);
                opened.insert(0, RenderEvent::BlockBreak);
                opened.push(RenderEvent::Md(Event::Start(Tag::List(
                    ordered.then_some(start),
                ))));
                closers.push(RenderEvent::Md(Event::End(TagEnd::List(ordered))));
            }
            "li" if block_allowed => {
                opened.push(RenderEvent::Md(Event::Start(Tag::Item)));
                closers.push(RenderEvent::Md(Event::End(TagEnd::Item)));
            }
            "blockquote" if block_allowed => {
                opened.push(RenderEvent::Md(Event::Start(Tag::BlockQuote(None))));
                closers.push(RenderEvent::Md(Event::End(TagEnd::BlockQuote(None))));
            }
            "pre" if block_allowed => {
                let language = attr("lang").map(str::to_string).or_else(|| {
                    attr("class").and_then(|class| {
                        class.split_ascii_whitespace().find_map(|c| {
                            c.strip_prefix("language-")
                                .or_else(|| c.strip_prefix("lang-"))
                                .map(str::to_string)
                        })
                    })
                });
                opened.push(RenderEvent::Md(Event::Start(Tag::CodeBlock(
                    CodeBlockKind::Fenced(CowStr::from(language.unwrap_or_default())),
                ))));
                closers.push(RenderEvent::Md(Event::End(TagEnd::CodeBlock)));
                self.pre_depth += 1;
                self.pre_fresh = true;
            }
            "table" if block_allowed => {
                opened.push(RenderEvent::Md(Event::Start(Tag::Table(Vec::new()))));
                closers.push(RenderEvent::Md(Event::End(TagEnd::Table)));
            }
            "tr" if self.position("table").is_some() && !in_md_table => {
                closers.push(RenderEvent::Md(Event::End(TagEnd::TableRow)));
            }
            "td" | "th" if self.position("table").is_some() && !in_md_table => {
                opened.push(RenderEvent::Md(Event::Start(Tag::TableCell)));
                if name == "th" {
                    style(Modifier::BOLD, &mut opened, &mut closers);
                }
                closers.push(RenderEvent::Md(Event::End(TagEnd::TableCell)));
            }
            "summary" if block_allowed => {
                opened.push(RenderEvent::BlockBreak);
                opened.push(text("▼ "));
                style(Modifier::BOLD, &mut opened, &mut closers);
                closers.push(RenderEvent::BlockEnd);
            }
            "dt" if block_allowed => {
                opened.push(RenderEvent::BlockBreak);
                style(Modifier::BOLD, &mut opened, &mut closers);
                closers.push(RenderEvent::BlockBreak);
            }
            "dd" if block_allowed => {
                opened.push(RenderEvent::BlockBreak);
                opened.push(text("    "));
                closers.push(RenderEvent::BlockBreak);
            }
            _ if block_allowed && BLOCK.contains(&name.as_str()) => {
                opened.insert(0, RenderEvent::BlockBreak);
                closers.push(RenderEvent::BlockBreak);
            }
            _ => {}
        }

        if opened
            .iter()
            .any(|event| matches!(event, RenderEvent::AlignStart(_)))
        {
            closers.push(RenderEvent::AlignEnd);
        }
        for event in opened {
            match event {
                RenderEvent::Md(Event::Text(marker)) => self.emit_inline_text(&marker, out),
                event => self.emit(event, out),
            }
        }
        self.stack.push(OpenElement {
            name,
            closers,
            inline,
        });
    }

    fn emit_inline_text<'a>(&mut self, text: &str, out: &mut Vec<RenderEvent<'a>>) {
        let mut chunk = String::new();
        if self.pending_space && !self.at_block_start {
            chunk.push(' ');
        }
        chunk.push_str(text);
        self.pending_space = false;
        self.at_block_start = false;
        out.push(RenderEvent::Md(Event::Text(CowStr::from(chunk))));
    }

    fn end<'a>(&mut self, name: &str, in_md_table: bool, out: &mut Vec<RenderEvent<'a>>) {
        if name == "br" {
            self.start(name.to_string(), &[], true, false, in_md_table, out);
            return;
        }
        if let Some(index) = self.position(name) {
            self.close_through(index, out);
        }
    }
}

fn tokenize(input: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut text = String::new();
    let mut rest = input;

    let flush = |text: &mut String, tokens: &mut Vec<Token>| {
        if !text.is_empty() {
            tokens.push(Token::Text(decode_entities(&std::mem::take(text))));
        }
    };

    while let Some(lt) = rest.find('<') {
        text.push_str(&rest[..lt]);
        rest = &rest[lt..];

        if let Some(after) = rest.strip_prefix("<!--") {
            flush(&mut text, &mut tokens);
            rest = after.find("-->").map_or("", |end| &after[end + 3..]);
            continue;
        }
        if rest.starts_with("<!") || rest.starts_with("<?") {
            flush(&mut text, &mut tokens);
            rest = rest.find('>').map_or("", |end| &rest[end + 1..]);
            continue;
        }
        let (is_end, body) = match rest.strip_prefix("</") {
            Some(body) => (true, body),
            None => (false, &rest[1..]),
        };
        let name_len = body
            .find(|c: char| !(c.is_ascii_alphanumeric() || c == '-' || c == ':'))
            .unwrap_or(body.len());
        if name_len == 0 || !body.starts_with(|c: char| c.is_ascii_alphabetic()) {
            text.push('<');
            rest = &rest[1..];
            continue;
        }
        let name = body[..name_len].to_ascii_lowercase();
        let Some((attrs, self_closing, consumed)) = parse_attributes(&body[name_len..]) else {
            // Unterminated tag: treat the remainder as text.
            text.push_str(rest);
            rest = "";
            break;
        };
        rest = &body[name_len + consumed..];
        flush(&mut text, &mut tokens);

        if is_end {
            tokens.push(Token::End(name));
            continue;
        }
        if SKIP_CONTENT.contains(&name.as_str()) && !self_closing {
            let close = format!("</{name}");
            rest = find_ascii_case_insensitive(rest, &close).map_or("", |at| {
                rest[at..].find('>').map_or("", |end| &rest[at + end + 1..])
            });
            continue;
        }
        tokens.push(Token::Start {
            name,
            attrs,
            self_closing,
        });
    }
    text.push_str(rest);
    flush(&mut text, &mut tokens);
    tokens
}

fn find_ascii_case_insensitive(haystack: &str, needle: &str) -> Option<usize> {
    let needle = needle.as_bytes();
    haystack
        .as_bytes()
        .windows(needle.len())
        .position(|window| window.eq_ignore_ascii_case(needle))
}

/// Parse attributes up to and including the closing `>`.
/// Returns the attributes, whether the tag self-closes, and bytes consumed.
type Attributes = Vec<(String, String)>;

fn parse_attributes(input: &str) -> Option<(Attributes, bool, usize)> {
    let bytes = input.as_bytes();
    let mut attrs = Vec::new();
    let mut i = 0;
    let mut self_closing = false;
    loop {
        while i < bytes.len() && bytes[i].is_ascii_whitespace() {
            i += 1;
        }
        match bytes.get(i)? {
            b'>' => return Some((attrs, self_closing, i + 1)),
            b'/' => {
                self_closing = true;
                i += 1;
                continue;
            }
            _ => {}
        }
        self_closing = false;
        let start = i;
        while i < bytes.len()
            && !matches!(bytes[i], b'=' | b'>' | b'/')
            && !bytes[i].is_ascii_whitespace()
        {
            i += 1;
        }
        let key = input[start..i].to_ascii_lowercase();
        while i < bytes.len() && bytes[i].is_ascii_whitespace() {
            i += 1;
        }
        let mut value = String::new();
        if bytes.get(i) == Some(&b'=') {
            i += 1;
            while i < bytes.len() && bytes[i].is_ascii_whitespace() {
                i += 1;
            }
            match bytes.get(i)? {
                quote @ (b'"' | b'\'') => {
                    let end = input[i + 1..].find(*quote as char)? + i + 1;
                    value = decode_entities(&input[i + 1..end]);
                    i = end + 1;
                }
                _ => {
                    let start = i;
                    while i < bytes.len() && bytes[i] != b'>' && !bytes[i].is_ascii_whitespace() {
                        i += 1;
                    }
                    value = decode_entities(&input[start..i]);
                }
            }
        }
        if !key.is_empty() {
            attrs.push((key, value));
        }
    }
}

pub fn decode_entities(input: &str) -> String {
    if !input.contains('&') {
        return input.to_string();
    }
    let mut out = String::with_capacity(input.len());
    let mut rest = input;
    while let Some(amp) = rest.find('&') {
        out.push_str(&rest[..amp]);
        rest = &rest[amp..];
        let decoded = rest[1..]
            .find(';')
            .filter(|end| *end > 0 && *end <= 32)
            .and_then(|end| decode_entity(&rest[1..1 + end]).map(|c| (c, end + 2)));
        match decoded {
            Some((c, len)) => {
                out.push(c);
                rest = &rest[len..];
            }
            None => {
                out.push('&');
                rest = &rest[1..];
            }
        }
    }
    out.push_str(rest);
    out
}

fn decode_entity(entity: &str) -> Option<char> {
    if let Some(number) = entity.strip_prefix('#') {
        let code = match number.strip_prefix(['x', 'X']) {
            Some(hex) => u32::from_str_radix(hex, 16).ok()?,
            None => number.parse().ok()?,
        };
        return char::from_u32(code).filter(|c| *c != '\0');
    }
    Some(match entity {
        "amp" => '&',
        "lt" => '<',
        "gt" => '>',
        "quot" => '"',
        "apos" => '\'',
        "nbsp" => '\u{a0}',
        "ensp" | "emsp" | "thinsp" => ' ',
        "copy" => '©',
        "reg" => '®',
        "trade" => '™',
        "hellip" => '…',
        "mdash" => '—',
        "ndash" => '–',
        "middot" => '·',
        "bull" => '•',
        "laquo" => '«',
        "raquo" => '»',
        "lsquo" => '‘',
        "rsquo" => '’',
        "ldquo" => '“',
        "rdquo" => '”',
        "larr" => '←',
        "rarr" => '→',
        "uarr" => '↑',
        "darr" => '↓',
        "harr" => '↔',
        "times" => '×',
        "divide" => '÷',
        "deg" => '°',
        "plusmn" => '±',
        "para" => '¶',
        "sect" => '§',
        "check" => '✓',
        "star" => '☆',
        "hearts" => '♥',
        "euro" => '€',
        "pound" => '£',
        "yen" => '¥',
        "cent" => '¢',
        _ => return None,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tokenizer_handles_attributes_comments_and_raw_text() {
        let tokens = tokenize(
            r#"<!-- hi --><p align=center class='x'>a &amp; b<br/><script>if (a<b) {}</script></p>"#,
        );
        assert_eq!(
            tokens,
            vec![
                Token::Start {
                    name: "p".into(),
                    attrs: vec![
                        ("align".into(), "center".into()),
                        ("class".into(), "x".into())
                    ],
                    self_closing: false,
                },
                Token::Text("a & b".into()),
                Token::Start {
                    name: "br".into(),
                    attrs: vec![],
                    self_closing: true,
                },
                Token::End("p".into()),
            ]
        );
    }

    #[test]
    fn stray_angle_brackets_are_text() {
        assert_eq!(tokenize("a < b"), vec![Token::Text("a < b".into())]);
        assert_eq!(
            tokenize("<unterminated"),
            vec![Token::Text("<unterminated".into())]
        );
    }

    #[test]
    fn entities_decode() {
        assert_eq!(
            decode_entities("&lt;&#65;&#x42;&copy;&bogus;&"),
            "<AB©&bogus;&"
        );
    }
}
