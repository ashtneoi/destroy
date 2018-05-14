use prelude::*;

#[test]
fn minimal() {
    let mut g = e(vec![
        n("start", s(t("a"))),
    ]);

    let st = g.parse("start", "aaa").unwrap();
    assert_eq!(st.name.as_ref().unwrap(), "start");
    assert_eq!(st.raw, (0, 3));
    assert!(st.children.is_empty());
}
