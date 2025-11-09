use clap::Parser;

mod ant_arg;

fn main() {
    let arg = ant_arg::Args::parse();

    if let Some(file) = arg.file {
        unimplemented!()
    }

    repl::repl();
}
