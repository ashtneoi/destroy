extern crate destroy;

use destroy::parse::parse_grammar;
use std::io;
use std::io::Read;

fn main() -> io::Result<()> {
    let mut buf = String::new();
    io::stdin().read_to_string(&mut buf)?;
    println!("{:?}", parse_grammar(&buf).unwrap());
    Ok(())
}
