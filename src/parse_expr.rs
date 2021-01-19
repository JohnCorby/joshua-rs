//! handle the painful process that is parsing expressions

#![allow(dead_code)]

use crate::Pair;

pub fn parse_expr(pair: Pair) {
    use crate::Rule::*;
    use std::panic::catch_unwind;

    catch_unwind(|| match pair.as_rule() {
        expr => parse_expr(pair.into_inner().next().unwrap()),
        equality_expr | compare_expr | add_expr | mul_expr => todo!("binary expr"),
        unary_expr => todo!("unary expr"),
        cast_expr => todo!("cast expr"),
        primary_expr => todo!("primary expr"),
        rule => unreachable!("{:?}", rule),
    })
    .unwrap()
}

fn parse_binary_expr(left: Pair, right: Pair, op: &str) {
    todo!()
}

fn parse_unary_expr(thing: Pair, op: &str) {
    todo!()
}

fn parse_cast_expr(expr: Pair, ty: &str) {
    todo!()
}
