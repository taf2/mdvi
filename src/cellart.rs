//! Text-mode image rendering for terminals without a pixel graphics protocol.
//!
//! Every terminal cell can show one glyph in two colours. We sample each cell at
//! `SAMPLE_W` x `SAMPLE_H` points, then try every candidate block glyph (sextants,
//! quadrants, half blocks and the 1/8-step bars), pick the two colours that best
//! reproduce the samples for that glyph's shape, and keep the glyph with the lowest
//! squared error. Glyph shapes are unions of rectangles, so each candidate costs a few
//! summed-area-table lookups. This is the approach tools like chafa use, and it gives
//! far more detail than plain half blocks.

use image::{imageops::FilterType, DynamicImage};
use ratatui::{
    buffer::Buffer,
    layout::Rect,
    style::{Color, Modifier},
};
use std::sync::OnceLock;

/// Samples per cell. 8 columns resolve 1/8 bars and halves; 24 rows resolve
/// thirds (sextants), halves (quadrants) and eighths.
const SAMPLE_W: usize = 8;
const SAMPLE_H: usize = 24;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CellArtMode {
    /// `▀`/`▄` only: 1x2 per cell. Works with any font.
    HalfBlocks,
    /// Sextants, quadrants, halves and eighth bars.
    Mosaic,
}

/// Axis-aligned rectangle in sample units: x in 0..=SAMPLE_W, y in 0..=SAMPLE_H.
type SampleRect = (u8, u8, u8, u8);

struct Glyph {
    symbol: char,
    rects: Vec<SampleRect>,
    area: f32,
}

fn glyph(symbol: char, rects: Vec<SampleRect>) -> Glyph {
    let area = rects
        .iter()
        .map(|&(x0, y0, x1, y1)| f32::from(x1 - x0) * f32::from(y1 - y0))
        .sum();
    Glyph {
        symbol,
        rects,
        area,
    }
}

/// Unicode sextant for a 6-bit mask (bit0 top-left, bit1 top-right, bit2 mid-left,
/// bit3 mid-right, bit4 bottom-left, bit5 bottom-right); `None` for masks Unicode
/// assigns to other blocks (empty, full, left half, right half).
fn sextant_char(mask: u8) -> Option<char> {
    if matches!(mask, 0 | 0b01_0101 | 0b10_1010 | 0b11_1111) {
        return None;
    }
    let mut index = u32::from(mask) - 1;
    if mask > 0b01_0101 {
        index -= 1;
    }
    if mask > 0b10_1010 {
        index -= 1;
    }
    char::from_u32(0x1FB00 + index)
}

fn half_glyphs() -> Vec<Glyph> {
    let (w, h) = (SAMPLE_W as u8, SAMPLE_H as u8);
    vec![glyph('▀', vec![(0, 0, w, h / 2)])]
}

fn mosaic_glyphs() -> Vec<Glyph> {
    let (w, h) = (SAMPLE_W as u8, SAMPLE_H as u8);
    let (hw, hh, th) = (w / 2, h / 2, h / 3);
    let mut glyphs = half_glyphs();

    // Halves.
    glyphs.push(glyph('▌', vec![(0, 0, hw, h)]));

    // Sextants: 2 columns x 3 rows.
    for mask in 1u8..63 {
        let Some(symbol) = sextant_char(mask) else {
            continue;
        };
        let rects = (0..6)
            .filter(|bit| mask & (1 << bit) != 0)
            .map(|bit| {
                let (col, row) = (bit % 2, bit / 2);
                (col * hw, row * th, (col + 1) * hw, (row + 1) * th)
            })
            .collect();
        glyphs.push(glyph(symbol, rects));
    }

    // Quadrants (2x2). Complements are equivalent, so one of each pair suffices.
    let tl = (0, 0, hw, hh);
    let tr = (hw, 0, w, hh);
    let bl = (0, hh, hw, h);
    glyphs.push(glyph('▘', vec![tl]));
    glyphs.push(glyph('▝', vec![tr]));
    glyphs.push(glyph('▖', vec![bl]));
    glyphs.push(glyph('▗', vec![(hw, hh, w, h)]));
    glyphs.push(glyph('▚', vec![tl, (hw, hh, w, h)]));

    // Lower eighths ▁..▇ and left eighths ▏..▉ (▄ and ▌ come from the halves above).
    for (k, symbol) in ['▁', '▂', '▃', '▅', '▆', '▇']
        .into_iter()
        .zip([1u8, 2, 3, 5, 6, 7])
        .map(|(c, k)| (k, c))
    {
        glyphs.push(glyph(symbol, vec![(0, h - k * h / 8, w, h)]));
    }
    for (k, symbol) in [1u8, 2, 3, 5, 6, 7]
        .into_iter()
        .zip(['▏', '▎', '▍', '▋', '▊', '▉'])
    {
        glyphs.push(glyph(symbol, vec![(0, 0, k * w / 8, h)]));
    }
    // Top and right single eighths.
    glyphs.push(glyph('▔', vec![(0, 0, w, h / 8)]));
    glyphs.push(glyph('▕', vec![(w - w / 8, 0, w, h)]));
    glyphs
}

fn glyphs_for(mode: CellArtMode) -> &'static [Glyph] {
    static HALF: OnceLock<Vec<Glyph>> = OnceLock::new();
    static MOSAIC: OnceLock<Vec<Glyph>> = OnceLock::new();
    match mode {
        CellArtMode::HalfBlocks => HALF.get_or_init(half_glyphs),
        CellArtMode::Mosaic => MOSAIC.get_or_init(mosaic_glyphs),
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct CellGlyph {
    pub symbol: char,
    pub fg: [u8; 3],
    pub bg: [u8; 3],
    pub bold: bool,
    /// Mostly transparent in the source: leave the terminal's own background showing.
    pub transparent: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LabelAnchor {
    Start,
    Middle,
    End,
}

/// A run of text from a vector image, drawn as real terminal characters instead of
/// pixels so it stays legible at any size. Coordinates are fractions of the image size;
/// `y` is the vertical centre of the text.
#[derive(Debug, Clone, PartialEq)]
pub struct TextLabel {
    pub x: f32,
    pub y: f32,
    pub anchor: LabelAnchor,
    pub text: String,
    pub color: [u8; 3],
    pub bold: bool,
}

#[derive(Debug, Clone)]
pub struct CellArt {
    pub rows: Vec<Vec<CellGlyph>>,
}

impl CellGlyph {
    const TRANSPARENT: CellGlyph = CellGlyph {
        symbol: ' ',
        fg: [0, 0, 0],
        bg: [0, 0, 0],
        bold: false,
        transparent: true,
    };
}

impl CellArt {
    /// Render `image` into exactly `cols` x `rows` cells (callers choose the aspect-correct box).
    pub fn render(
        image: &DynamicImage,
        cols: u16,
        rows: u16,
        mode: CellArtMode,
        labels: &[TextLabel],
    ) -> Self {
        let cols = usize::from(cols.max(1));
        let rows = usize::from(rows.max(1));
        let sampled = image
            .resize_exact(
                (cols * SAMPLE_W) as u32,
                (rows * SAMPLE_H) as u32,
                FilterType::Lanczos3,
            )
            .to_rgba8();
        let stride = cols * SAMPLE_W;
        let rgba: Vec<[f32; 4]> = sampled.pixels().map(|px| px.0.map(f32::from)).collect();

        let glyphs = glyphs_for(mode);
        let mut table = CellTable::default();
        let mut cell = vec![[0f32; 3]; SAMPLE_W * SAMPLE_H];
        let rows = (0..rows)
            .map(|row| {
                (0..cols)
                    .map(|col| {
                        let (x0, y0) = (col * SAMPLE_W, row * SAMPLE_H);
                        // Alpha-weighted mean colour of the cell, used as the matte for
                        // partially transparent samples so edges don't pick up a dark fringe.
                        let (mut weighted, mut alpha) = ([0f32; 3], 0f32);
                        for y in 0..SAMPLE_H {
                            for x in 0..SAMPLE_W {
                                let px = rgba[(y0 + y) * stride + x0 + x];
                                let a = px[3] / 255.0;
                                for c in 0..3 {
                                    weighted[c] += px[c] * a;
                                }
                                alpha += a;
                            }
                        }
                        if alpha < (SAMPLE_W * SAMPLE_H) as f32 * 0.5 {
                            return CellGlyph::TRANSPARENT;
                        }
                        let matte = weighted.map(|v| v / alpha);
                        for y in 0..SAMPLE_H {
                            for x in 0..SAMPLE_W {
                                let px = rgba[(y0 + y) * stride + x0 + x];
                                let a = px[3] / 255.0;
                                cell[y * SAMPLE_W + x] =
                                    [0, 1, 2].map(|c| px[c] * a + matte[c] * (1.0 - a));
                            }
                        }
                        table.load(&cell, SAMPLE_W, 0, 0);
                        table.best(glyphs)
                    })
                    .collect()
            })
            .collect();
        let mut art = CellArt { rows };
        for label in labels {
            art.overlay(label);
        }
        art
    }

    fn overlay(&mut self, label: &TextLabel) {
        let rows = self.rows.len();
        let cols = self.rows.first().map_or(0, Vec::len);
        let chars: Vec<char> = label
            .text
            .chars()
            .map(|c| if c.is_control() { ' ' } else { c })
            .collect();
        if rows == 0 || cols == 0 || chars.is_empty() {
            return;
        }
        let row = ((label.y * rows as f32).floor().max(0.0) as usize).min(rows - 1);
        let anchor_col = (label.x * cols as f32).round();
        let len = chars.len() as f32;
        let start = match label.anchor {
            LabelAnchor::Start => anchor_col,
            LabelAnchor::Middle => anchor_col - (len / 2.0).floor(),
            LabelAnchor::End => anchor_col - len,
        }
        .max(0.0) as usize;
        for (offset, ch) in chars.into_iter().enumerate() {
            let Some(cell) = self.rows[row].get_mut(start + offset) else {
                break;
            };
            // Keep whichever of the cell's colours contrasts least with the text as the
            // backdrop, i.e. the surface the text was sitting on.
            let backdrop = if cell.transparent {
                // Text over a transparent area: keep the terminal background.
                *cell = CellGlyph {
                    symbol: ch,
                    fg: label.color,
                    bg: [0, 0, 0],
                    bold: label.bold,
                    transparent: true,
                };
                continue;
            } else if cell.symbol == ' '
                || distance(cell.bg, label.color) >= distance(cell.fg, label.color)
            {
                cell.bg
            } else {
                cell.fg
            };
            *cell = CellGlyph {
                symbol: ch,
                fg: label.color,
                bg: backdrop,
                bold: label.bold,
                transparent: false,
            };
        }
    }

    /// Draw rows `[first_row, first_row + area.height)` into `area`.
    pub fn draw(&self, first_row: usize, area: Rect, buf: &mut Buffer) {
        for (dy, row) in self
            .rows
            .iter()
            .skip(first_row)
            .take(usize::from(area.height))
            .enumerate()
        {
            for (dx, glyph) in row.iter().take(usize::from(area.width)).enumerate() {
                let x = area.x + dx as u16;
                let y = area.y + dy as u16;
                if let Some(cell) = buf.cell_mut((x, y)) {
                    let mut utf8 = [0u8; 4];
                    let bg = if glyph.transparent {
                        Color::Reset
                    } else {
                        Color::Rgb(glyph.bg[0], glyph.bg[1], glyph.bg[2])
                    };
                    cell.set_symbol(glyph.symbol.encode_utf8(&mut utf8))
                        .set_fg(Color::Rgb(glyph.fg[0], glyph.fg[1], glyph.fg[2]))
                        .set_bg(bg);
                    if glyph.bold {
                        cell.modifier.insert(Modifier::BOLD);
                    } else {
                        cell.modifier.remove(Modifier::BOLD);
                    }
                }
            }
        }
    }
}

/// Summed-area table over one cell's samples (colour sums only; the squared-sum term of
/// the error is identical for every glyph, so it is not needed to compare them).
struct CellTable {
    sums: [[[f32; 3]; SAMPLE_W + 1]; SAMPLE_H + 1],
}

impl Default for CellTable {
    fn default() -> Self {
        Self {
            sums: [[[0.0; 3]; SAMPLE_W + 1]; SAMPLE_H + 1],
        }
    }
}

impl CellTable {
    fn load(&mut self, pixels: &[[f32; 3]], stride: usize, x0: usize, y0: usize) {
        for y in 0..SAMPLE_H {
            let mut row_sum = [0f32; 3];
            for x in 0..SAMPLE_W {
                let px = pixels[(y0 + y) * stride + x0 + x];
                for c in 0..3 {
                    row_sum[c] += px[c];
                    self.sums[y + 1][x + 1][c] = self.sums[y][x + 1][c] + row_sum[c];
                }
            }
        }
    }

    fn rect_sum(&self, (x0, y0, x1, y1): SampleRect) -> [f32; 3] {
        let (x0, y0, x1, y1) = (
            usize::from(x0),
            usize::from(y0),
            usize::from(x1),
            usize::from(y1),
        );
        let s = &self.sums;
        [0, 1, 2].map(|c| s[y1][x1][c] - s[y0][x1][c] - s[y1][x0][c] + s[y0][x0][c])
    }

    fn best(&self, glyphs: &[Glyph]) -> CellGlyph {
        let total = self.sums[SAMPLE_H][SAMPLE_W];
        let total_area = (SAMPLE_W * SAMPLE_H) as f32;
        let norm2 = |v: [f32; 3]| v[0] * v[0] + v[1] * v[1] + v[2] * v[2];

        // Uniform cell: error term is |total|^2 / N; any split must beat it.
        let mean = total.map(|v| v / total_area);
        let mut best_score = norm2(total) / total_area;
        let mut best = CellGlyph {
            symbol: ' ',
            fg: to_u8(mean),
            bg: to_u8(mean),
            bold: false,
            transparent: false,
        };

        for glyph in glyphs {
            let fg_area = glyph.area;
            let bg_area = total_area - fg_area;
            let mut fg_sum = [0f32; 3];
            for &rect in &glyph.rects {
                let s = self.rect_sum(rect);
                for c in 0..3 {
                    fg_sum[c] += s[c];
                }
            }
            let bg_sum = [0, 1, 2].map(|c| total[c] - fg_sum[c]);
            // Minimising squared error == maximising sum |S_i|^2 / n_i over both regions.
            let score = norm2(fg_sum) / fg_area + norm2(bg_sum) / bg_area;
            if score > best_score {
                best_score = score;
                best = CellGlyph {
                    symbol: glyph.symbol,
                    fg: to_u8(fg_sum.map(|v| v / fg_area)),
                    bg: to_u8(bg_sum.map(|v| v / bg_area)),
                    bold: false,
                    transparent: false,
                };
            }
        }
        best
    }
}

fn distance(a: [u8; 3], b: [u8; 3]) -> u32 {
    (0..3)
        .map(|c| (i32::from(a[c]) - i32::from(b[c])).pow(2) as u32)
        .sum()
}

fn to_u8(c: [f32; 3]) -> [u8; 3] {
    c.map(|v| v.round().clamp(0.0, 255.0) as u8)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn sextant_chars_cover_the_unicode_block() {
        assert_eq!(sextant_char(0b00_0001), Some('\u{1FB00}'));
        assert_eq!(sextant_char(0b01_0100), Some('\u{1FB13}'));
        assert_eq!(sextant_char(0b01_0110), Some('\u{1FB14}'));
        assert_eq!(sextant_char(0b11_1110), Some('\u{1FB3B}'));
        assert_eq!(sextant_char(0b01_0101), None);
        assert_eq!(sextant_char(0b10_1010), None);
    }

    fn render_pattern(width: u32, height: u32, paint: impl Fn(u32, u32) -> [u8; 3]) -> CellGlyph {
        let img = image::RgbImage::from_fn(width, height, |x, y| image::Rgb(paint(x, y)));
        let art = CellArt::render(
            &DynamicImage::ImageRgb8(img),
            1,
            1,
            CellArtMode::Mosaic,
            &[],
        );
        art.rows[0][0]
    }

    #[test]
    fn split_cells_pick_the_matching_glyph_and_colours() {
        let glyph = render_pattern(8, 24, |x, _| if x < 4 { [255, 0, 0] } else { [0, 0, 255] });
        assert_eq!(glyph.symbol, '▌');
        assert_eq!((glyph.fg, glyph.bg), ([255, 0, 0], [0, 0, 255]));

        let glyph = render_pattern(
            8,
            24,
            |_, y| if y >= 21 { [0, 0, 0] } else { [255, 255, 255] },
        );
        assert_eq!(glyph.symbol, '▁');
        assert_eq!((glyph.fg, glyph.bg), ([0, 0, 0], [255, 255, 255]));
    }

    #[test]
    fn uniform_cells_are_blank_with_background_colour() {
        let glyph = render_pattern(8, 24, |_, _| [10, 20, 30]);
        assert_eq!(glyph.symbol, ' ');
        assert_eq!(glyph.bg, [10, 20, 30]);
    }

    #[test]
    fn render_produces_requested_grid() {
        let img = DynamicImage::new_rgb8(300, 100);
        let art = CellArt::render(&img, 30, 5, CellArtMode::Mosaic, &[]);
        assert_eq!(art.rows.len(), 5);
        assert!(art.rows.iter().all(|row| row.len() == 30));
    }

    #[test]
    fn labels_are_drawn_as_text_over_the_backdrop() {
        let img = image::RgbImage::from_pixel(80, 48, image::Rgb([240, 240, 240]));
        let label = TextLabel {
            x: 0.5,
            y: 0.5,
            anchor: LabelAnchor::Middle,
            text: "Hi!".to_string(),
            color: [20, 30, 40],
            bold: true,
        };
        let art = CellArt::render(
            &DynamicImage::ImageRgb8(img),
            10,
            2,
            CellArtMode::Mosaic,
            &[label],
        );
        let row: String = art.rows[1].iter().map(|g| g.symbol).collect();
        assert_eq!(row, "    Hi!   ");
        let h = art.rows[1][4];
        assert_eq!((h.fg, h.bg, h.bold), ([20, 30, 40], [240, 240, 240], true));
    }

    #[test]
    fn transparent_areas_leave_terminal_background() {
        let img = image::RgbaImage::from_fn(16, 24, |x, _| {
            if x < 8 {
                image::Rgba([0, 0, 0, 0])
            } else {
                image::Rgba([200, 100, 50, 255])
            }
        });
        let art = CellArt::render(
            &DynamicImage::ImageRgba8(img),
            2,
            1,
            CellArtMode::Mosaic,
            &[],
        );
        assert!(art.rows[0][0].transparent);
        assert!(!art.rows[0][1].transparent);
        assert_eq!(art.rows[0][1].bg, [200, 100, 50]);
    }

    #[test]
    fn mosaic_glyphs_are_valid_shapes() {
        for glyph in glyphs_for(CellArtMode::Mosaic) {
            assert!(glyph.area > 0.0 && glyph.area < (SAMPLE_W * SAMPLE_H) as f32);
        }
    }
}
