use parse::{
    parse_grammar,
};
use test::Bencher;

#[bench]
fn bench_large_grammar(b: &mut Bencher) {
    let mut large_grammar = "".to_string();
    for i in 0..80 {
        large_grammar.push_str(
            &format!("a{} = 'a'..'z'\n", i)
        );
    }

    b.iter(|| parse_grammar(&large_grammar).unwrap());
}
