use std::{io::Write, rc::Rc};

use lexer::Lexer;
use parser::{Parser, error::display_err};

pub fn repl() {
    let file: Rc<str> = "*repl".into();

    loop {
        print!(">>> ");

        std::io::stdout().flush().expect("flush failed");

        let mut code = String::new();

        std::io::stdin()
            .read_line(&mut code)
            .expect("read line failed");

        let mut lexer = Lexer::new(code, file.clone());

        let tokens = lexer.get_tokens();

        if lexer.contains_error() {
            lexer.print_errors();
            continue;
        }

        let mut parser = Parser::new(tokens);

        match parser.parse_program() {
            Ok(it) => println!("{it}"),
            Err(err) => display_err(&err),
        }
    }
}
