use prelude::*;

#[test]
fn minimal() {
    let mut g = e(vec![
        n("start", s(t("a"))),
    ]);

    println!("{:?}", g.parse("start", "aaaaa"));
}
