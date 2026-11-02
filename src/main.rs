mod app;
mod layout;
mod renderer;

use std::path::PathBuf;

use anyhow::Result;
use clap::{Parser, ValueEnum};

#[derive(Debug, Clone, Copy, ValueEnum)]
pub enum ImageProtocol {
    Auto,
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

    /// Image rendering protocol: auto, halfblocks, sixel, kitty, iterm2
    #[arg(long, value_enum, default_value_t = ImageProtocol::Auto)]
    image_protocol: ImageProtocol,
}

fn main() -> Result<()> {
    let cli = Cli::parse();
    app::run(cli.path, cli.line, cli.image_protocol)
}
