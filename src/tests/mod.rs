#![cfg(test)]

use parse::Match;
use Pos;

mod grammar_node;
mod group;
mod link_tree;
mod meta;
mod regression;

fn mat(
        (lin0, row0, col0, lin1, row1, col1):
            (usize, usize, usize, usize, usize, usize),
        named: Vec<(&str, Vec<Match>)>,
) -> Match {
    Match::new(
        (
            Pos {
                lin: lin0,
                row: row0,
                col: col0,
            },
            Pos {
                lin: lin1,
                row: row1,
                col: col1,
            },
        ),
        named,
    )
}
