use std::thread::panicking;

use proc_macro::{*, Spacing::*};
use proc_macro_tool::*;

struct Test(&'static str);

impl Drop for Test {
    fn drop(&mut self) {
        if panicking() {
            println!(" {} failed", self.0)
        }
    }
}

macro_rules! test {
    ($(fn $name:ident () $b:tt)+) => {
        let mut _test;
        $(
            print!("Run test {}", stringify!($name));
            _test = Test(stringify!($name));
            $b;
            println!(" passed");
        )+
    };
}

#[doc(hidden)]
pub fn __test() {
    test! {
        fn puncts_test() {
            let tests = [
                Punct::new(':', Joint),
                Punct::new(':', Joint),
            ];
            let puncts = puncts("::");
            assert_eq!(puncts.clone().into_iter().count(), tests.len());
            for (i, tt) in puncts.into_iter().enumerate() {
                let TokenTree::Punct(p) = tt else {
                    panic!("{tt:?}")
                };
                assert_eq!(p.as_char(), tests[i].as_char());
                assert_eq!(p.spacing(), tests[i].spacing());
            }
        }

        fn puncts_test1() {
            let tests = [
                Punct::new(':', Joint),
                Punct::new(':', Alone),
            ];
            let puncts = puncts(":: ");
            assert_eq!(puncts.clone().into_iter().count(), tests.len());
            for (i, tt) in puncts.into_iter().enumerate() {
                let TokenTree::Punct(p) = tt else {
                    panic!("{tt:?}")
                };
                assert_eq!(p.as_char(), tests[i].as_char());
                assert_eq!(p.spacing(), tests[i].spacing(), "{i}");
            }
        }

        fn puncts_test2() {
            let tests = [
                Punct::new(':', Alone),
                Punct::new(':', Alone),
            ];
            let puncts = puncts(": : ");
            assert_eq!(puncts.clone().into_iter().count(), tests.len());
            for (i, tt) in puncts.into_iter().enumerate() {
                let TokenTree::Punct(p) = tt else {
                    panic!("{tt:?}")
                };
                assert_eq!(p.as_char(), tests[i].as_char());
                assert_eq!(p.spacing(), tests[i].spacing());
            }
        }

        fn puncts_test3() {
            let tests = [
                Punct::new(':', Alone),
                Punct::new(':', Alone),
            ];
            let puncts = puncts(":  : ");
            assert_eq!(puncts.clone().into_iter().count(), tests.len());
            for (i, tt) in puncts.into_iter().enumerate() {
                let TokenTree::Punct(p) = tt else {
                    panic!("{tt:?}")
                };
                assert_eq!(p.as_char(), tests[i].as_char());
                assert_eq!(p.spacing(), tests[i].spacing());
            }
        }

        fn puncts_test4() {
            let tests = [
                Punct::new(':', Alone),
                Punct::new(':', Joint),
            ];
            let puncts = puncts(":  :");
            assert_eq!(puncts.clone().into_iter().count(), tests.len());
            for (i, tt) in puncts.into_iter().enumerate() {
                let TokenTree::Punct(p) = tt else {
                    panic!("{tt:?}")
                };
                assert_eq!(p.as_char(), tests[i].as_char());
                assert_eq!(p.spacing(), tests[i].spacing());
            }
        }

        fn split_test() {
            let puncts = puncts("-+*,/%!");
            let (a, mut b) = puncts.split_puncts(",").unwrap();
            let a = &mut a.into_iter();
            let f = |tt: TokenTree| tt.as_punct_char().unwrap();

            assert_eq!(Some('-'), a.next().map(f));
            assert_eq!(Some('+'), a.next().map(f));
            assert_eq!(Some('*'), a.next().map(f));
            assert_eq!(None,      a.next().map(f));

            assert_eq!(Some('/'), b.next().map(f));
            assert_eq!(Some('%'), b.next().map(f));
            assert_eq!(Some('!'), b.next().map(f));
            assert_eq!(None,      b.next().map(f));
        }

        fn split_test1() {
            let puncts = puncts("-+*,.,+/%!");
            let (a, mut b) = puncts.split_puncts(",+").unwrap();
            let a = &mut a.into_iter();
            let f = |tt: TokenTree| tt.as_punct_char().unwrap();

            assert_eq!(Some('-'), a.next().map(f));
            assert_eq!(Some('+'), a.next().map(f));
            assert_eq!(Some('*'), a.next().map(f));
            assert_eq!(Some(','), a.next().map(f));
            assert_eq!(Some('.'), a.next().map(f));
            assert_eq!(None,      a.next().map(f));

            assert_eq!(Some('/'), b.next().map(f));
            assert_eq!(Some('%'), b.next().map(f));
            assert_eq!(Some('!'), b.next().map(f));
            assert_eq!(None,      b.next().map(f));
        }

        fn split_test2() {
            let puncts = puncts("-+*,.,+");
            let (a, mut b) = puncts.split_puncts(",+").unwrap();
            let a = &mut a.into_iter();
            let f = |tt: TokenTree| tt.as_punct_char().unwrap();

            assert_eq!(Some('-'), a.next().map(f));
            assert_eq!(Some('+'), a.next().map(f));
            assert_eq!(Some('*'), a.next().map(f));
            assert_eq!(Some(','), a.next().map(f));
            assert_eq!(Some('.'), a.next().map(f));
            assert_eq!(None,      a.next().map(f));

            assert_eq!(None,      b.next().map(f));
        }

        fn split_test3() {
            let puncts = puncts("-+*,.,");
            assert!(puncts.split_puncts(",+").is_none());
        }

        fn peek_test1() {
            let puncts = puncts("+-*/ ");
            let mut iter = puncts.parse_iter();
            assert_eq!(Some('-'), iter.peek_i(1).unwrap().as_punct_char());
            assert!(iter.peek_i(1).unwrap().is_joint());
            assert_eq!(Some('+'), iter.peek_i(0).unwrap().as_punct_char());
            assert!(iter.peek_i(0).unwrap().is_joint());
            assert_eq!(Some('+'), iter.next().unwrap().as_punct_char());
            assert_eq!(Some('-'), iter.next().unwrap().as_punct_char());
            assert_eq!(Some('*'), iter.next().unwrap().as_punct_char());
            assert!(! iter.peek_i(0).unwrap().is_joint());
            assert_eq!(Some('/'), iter.next().unwrap().as_punct_char());
            assert!(iter.next().is_none());
        }

        fn peek_test2() {
            let mut puncts = puncts("+-*/");
            puncts.push(Literal::character('m').into());

            let mut iter = puncts.parse_iter();
            assert_eq!(Some('-'), iter.peek_i(1).unwrap().as_punct_char());
            assert!(iter.peek_i(1).unwrap().is_joint());
            assert_eq!(Some('+'), iter.peek_i(0).unwrap().as_punct_char());
            assert!(iter.peek_i(0).unwrap().is_joint());
            assert_eq!(Some('+'), iter.next().unwrap().as_punct_char());
            assert_eq!(Some('-'), iter.next().unwrap().as_punct_char());
            assert_eq!(Some('*'), iter.next().unwrap().as_punct_char());
            assert!(iter.peek_i(0).unwrap().is_joint());
            assert_eq!(Some('/'), iter.next().unwrap().as_punct_char());
            assert!(iter.next().is_some_and(|tt| tt.is_literal()));
            assert!(iter.next().is_none());
        }

        fn peek_test3() {
            let puncts = puncts("+-*/");
            let mut iter = puncts.parse_iter();
            assert_eq!(Some('+'), iter.next().unwrap().as_punct_char());
            assert_eq!(Some('-'), iter.next().unwrap().as_punct_char());
            assert_eq!(Some('/'), iter.peek_i(1).unwrap().as_punct_char());
            assert_eq!(Some('*'), iter.next().unwrap().as_punct_char());
            assert!(iter.peek_i(0).unwrap().is_joint());
            assert_eq!(Some('/'), iter.next().unwrap().as_punct_char());
            assert!(iter.next().is_none());
        }

        fn peek_next_back_test() {
            let puncts = puncts("+-*/").into_iter().collect::<Vec<_>>();
            let mut iter = puncts.into_iter().parse_iter();
            assert_eq!(Some('+'), iter.next().unwrap().as_punct_char());
            assert_eq!(Some('/'), iter.next_back().unwrap().as_punct_char());
            assert_eq!(Some('-'), iter.next().unwrap().as_punct_char());
            assert_eq!(Some('*'), iter.next().unwrap().as_punct_char());
            assert!(iter.next().is_none());
        }

        fn peek_next_back_test1() {
            let puncts = puncts("+-*/").into_iter().collect::<Vec<_>>();
            let mut iter = puncts.into_iter().parse_iter();
            assert_eq!(Some('/'), iter.peek_i(3).unwrap().as_punct_char());
            assert_eq!(Some('+'), iter.next().unwrap().as_punct_char());
            assert_eq!(Some('/'), iter.next_back().unwrap().as_punct_char());
            assert_eq!(Some('-'), iter.next().unwrap().as_punct_char());
            assert_eq!(Some('*'), iter.next().unwrap().as_punct_char());
            assert!(iter.next().is_none());
        }

        fn peek_next_back_test2() {
            let puncts = puncts("+-*/").into_iter().collect::<Vec<_>>();
            let mut iter = puncts.into_iter().parse_iter();
            assert_eq!(Some('*'), iter.peek_i(2).unwrap().as_punct_char());
            assert_eq!(Some('+'), iter.next().unwrap().as_punct_char());
            assert_eq!(Some('/'), iter.next_back().unwrap().as_punct_char());
            assert_eq!(Some('-'), iter.next().unwrap().as_punct_char());
            assert_eq!(Some('*'), iter.next().unwrap().as_punct_char());
            assert!(iter.next().is_none());
        }

        fn peek_next_back_test3() {
            let puncts = puncts("+-*/").into_iter().collect::<Vec<_>>();
            let mut iter = puncts.into_iter().parse_iter();
            assert_eq!(Some('-'), iter.peek_i(1).unwrap().as_punct_char());
            assert_eq!(Some('+'), iter.next().unwrap().as_punct_char());
            assert_eq!(Some('/'), iter.next_back().unwrap().as_punct_char());
            assert_eq!(Some('-'), iter.next().unwrap().as_punct_char());
            assert_eq!(Some('*'), iter.next().unwrap().as_punct_char());
            assert!(iter.next().is_none());
        }

        fn peek_next_back_test4() {
            let puncts = puncts("+-*/").into_iter().collect::<Vec<_>>();
            let mut iter = puncts.into_iter().parse_iter();
            assert_eq!(Some('+'), iter.next().unwrap().as_punct_char());
            assert_eq!(Some('*'), iter.peek_i(1).unwrap().as_punct_char());
            assert_eq!(Some('/'), iter.next_back().unwrap().as_punct_char());
            assert_eq!(Some('-'), iter.next().unwrap().as_punct_char());
            assert_eq!(Some('*'), iter.next().unwrap().as_punct_char());
            assert!(iter.next().is_none());
        }

        fn peek_next_back_test5() {
            let puncts = puncts("+-*/").into_iter().collect::<Vec<_>>();
            let mut iter = puncts.into_iter().parse_iter();
            assert_eq!(Some('+'), iter.next().unwrap().as_punct_char());
            assert_eq!(Some('/'), iter.peek_i(2).unwrap().as_punct_char());
            assert_eq!(Some('/'), iter.next_back().unwrap().as_punct_char());
            assert_eq!(Some('-'), iter.next().unwrap().as_punct_char());
            assert_eq!(Some('*'), iter.next().unwrap().as_punct_char());
            assert!(iter.next().is_none());
        }

        fn err_macro_test() {
            fn inner() -> TokenStream {
                err!("...");
                #[allow(unreachable_code)]
                {
                    unreachable!();
                }
            }
            assert!(inner().into_iter().next().is_some());
        }

        fn err_macro_test1() {
            fn inner() -> TokenStream {
                err!("...", Span::call_site());
                #[allow(unreachable_code)]
                {
                    unreachable!();
                }
            }
            assert!(inner().into_iter().next().is_some());
        }
    }
}
