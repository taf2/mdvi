mod app;
mod cellart;
mod html;
mod layout;
mod renderer;

use std::path::PathBuf;

use anyhow::Result;
use clap::{Parser, ValueEnum};

#[derive(Debug, Clone, Copy, ValueEnum)]
pub enum ImageProtocol {
    Auto,
    /// Text-mode 2x3 sub-cell rendering (Unicode sextants)
    Symbols,
    Halfblocks,
    Sixel,
    Kitty,
    Iterm2,
}

#[derive(Debug, Parser)]
#[command(name = "mdvi")]
#[command(
    version,
    about = "A high-quality markdown file viewer for the terminal"
)]
struct Cli {
    /// Markdown file to open
    path: PathBuf,

    /// Start at a specific line (1-based)
    #[arg(short, long, default_value_t = 1)]
    line: usize,

    /// Maximum content width in terminal columns (defaults to available width)
    #[arg(long, value_parser = clap::value_parser!(u16).range(1..))]
    width: Option<u16>,

    /// Image rendering protocol: auto, symbols, halfblocks, sixel, kitty, iterm2
    #[arg(long, value_enum, default_value_t = ImageProtocol::Auto)]
    image_protocol: ImageProtocol,
}

fn main() -> Result<()> {
    let cli = Cli::parse();
    app::run(cli.path, cli.line, cli.image_protocol, cli.width)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn width_accepts_positive_columns_only() {
        assert_eq!(
            Cli::try_parse_from(["mdvi", "--width", "72", "a.md"])
                .unwrap()
                .width,
            Some(72)
        );
        assert_eq!(Cli::try_parse_from(["mdvi", "a.md"]).unwrap().width, None);
        for width in ["0", "-1", "65536", "abc"] {
            assert!(Cli::try_parse_from(["mdvi", "--width", width, "a.md"]).is_err());
        }
    }
}
