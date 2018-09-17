use parse::{
    parse_grammar,
};
use string_table::StringTable;
use test::Bencher;

#[bench]
fn bench_large_grammar(b: &mut Bencher) {
    let mut large_grammar = "".to_string();
    for i in 0..2000 {
        large_grammar.push_str(
            &format!("a{} = 'a'..'z'\n", i)
        );
    }

    let mut tab = StringTable::new();
    b.iter(|| parse_grammar(&mut tab, &large_grammar).unwrap());
}
