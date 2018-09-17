extern crate destroy;

use destroy::parse::parse_grammar;
use destroy::string_table::StringTable;
use std::io;
use std::io::Read;
use std::process::exit;

fn main() {
    let mut buf = String::new();
    match io::stdin().read_to_string(&mut buf) {
        Err(e) => {
            eprintln!("error: can't read from file ({})", e);
            exit(2);
        },
        _ => (),
    }
    let mut tab = StringTable::new();
    match parse_grammar(&mut tab, &buf) {
        Err(e) => {
            eprintln!("error: {}", e);
            exit(1);
        },
        Ok(x) => {
            println!("{:?}", x);
        },
    }
}
