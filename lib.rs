#![cfg(test)]

use proc_macro_tool::puncts;
use proc_macro2::{
    Punct,
    Spacing::*, TokenTree,
};

#[test]
fn puncts_test() {
    let tests = [
        Punct::new(':', Joint),
        Punct::new(':', Joint),
    ];
    let puncts = puncts("::", Joint);
    assert_eq!(puncts.clone().into_iter().count(), tests.len());
    for (i, tt) in puncts.into_iter().enumerate() {
        let TokenTree::Punct(p) = tt else {
            panic!("{tt:?}")
        };
        assert_eq!(p.as_char(), tests[i].as_char());
        assert_eq!(p.spacing(), tests[i].spacing());
    }
}

#[test]
fn puncts_test1() {
    let tests = [
        Punct::new(':', Joint),
        Punct::new(':', Alone),
    ];
    let puncts = puncts("::", Alone);
    assert_eq!(puncts.clone().into_iter().count(), tests.len());
    for (i, tt) in puncts.into_iter().enumerate() {
        let TokenTree::Punct(p) = tt else {
            panic!("{tt:?}")
        };
        assert_eq!(p.as_char(), tests[i].as_char());
        assert_eq!(p.spacing(), tests[i].spacing());
    }
}

#[test]
fn puncts_test2() {
    let tests = [
        Punct::new(':', Alone),
        Punct::new(':', Alone),
    ];
    let puncts = puncts(": :", Alone);
    assert_eq!(puncts.clone().into_iter().count(), tests.len());
    for (i, tt) in puncts.into_iter().enumerate() {
        let TokenTree::Punct(p) = tt else {
            panic!("{tt:?}")
        };
        assert_eq!(p.as_char(), tests[i].as_char());
        assert_eq!(p.spacing(), tests[i].spacing());
    }
}

#[test]
fn puncts_test3() {
    let tests = [
        Punct::new(':', Alone),
        Punct::new(':', Alone),
    ];
    let puncts = puncts(":  :", Alone);
    assert_eq!(puncts.clone().into_iter().count(), tests.len());
    for (i, tt) in puncts.into_iter().enumerate() {
        let TokenTree::Punct(p) = tt else {
            panic!("{tt:?}")
        };
        assert_eq!(p.as_char(), tests[i].as_char());
        assert_eq!(p.spacing(), tests[i].spacing());
    }
}

#[test]
fn puncts_test4() {
    let tests = [
        Punct::new(':', Alone),
        Punct::new(':', Joint),
    ];
    let puncts = puncts(":  :", Joint);
    assert_eq!(puncts.clone().into_iter().count(), tests.len());
    for (i, tt) in puncts.into_iter().enumerate() {
        let TokenTree::Punct(p) = tt else {
            panic!("{tt:?}")
        };
        assert_eq!(p.as_char(), tests[i].as_char());
        assert_eq!(p.spacing(), tests[i].spacing());
    }
}
