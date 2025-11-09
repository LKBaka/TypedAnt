use clap::Parser;

#[derive(Parser, Debug, Clone)]
#[command(
    name = "TypedAnt",
    version = "0.1.0",
    about = "TypedAntScript",
    long_about = None
)]
pub struct Args {
    /// 输入文件路径（可选）
    #[arg(short, long)]
    pub file: Option<String>,
}
