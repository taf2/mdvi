mod app;
mod renderer;

use std::path::PathBuf;

use anyhow::Result;
use clap::Parser;

#[derive(Debug, Parser)]
#[command(name = "mdview")]
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
}

fn main() -> Result<()> {
    let cli = Cli::parse();
    app::run(cli.path, cli.line)
}
