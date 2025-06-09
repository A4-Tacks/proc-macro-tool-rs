use std::{str::FromStr, thread::panicking};

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

        fn split_all_test() {
            let splitted = puncts("+, -, *")
                .parse_iter()
                .split_puncts_all(",")
                .map(|stream| stream.to_string())
                .collect::<Vec<_>>();
            assert_eq!(splitted, [
                "+",
                "-",
                "*",
            ]);
        }

        fn split_all_test1() {
            let splitted = puncts("+, -, *,")
                .parse_iter()
                .split_puncts_all(",")
                .map(|stream| stream.to_string())
                .collect::<Vec<_>>();
            assert_eq!(splitted, [
                "+",
                "-",
                "*",
            ]);
        }

        fn split_all_test2() {
            let splitted = puncts(", -, *,")
                .parse_iter()
                .split_puncts_all(",")
                .map(|stream| stream.to_string())
                .collect::<Vec<_>>();
            assert_eq!(splitted, [
                "",
                "-",
                "*",
            ]);
        }

        fn split_all_test3() {
            let splitted = puncts("+, -, ,")
                .parse_iter()
                .split_puncts_all(",")
                .map(|stream| stream.to_string())
                .collect::<Vec<_>>();
            assert_eq!(splitted, [
                "+",
                "-",
                "",
            ]);
        }

        fn split_all_test4() {
            let splitted = puncts("+, -, ,,")
                .parse_iter()
                .split_puncts_all(",")
                .map(|stream| stream.to_string())
                .collect::<Vec<_>>();
            assert_eq!(splitted, [
                "+",
                "-",
                "",
                "",
            ]);
        }

        fn split_all_test4() {
            let splitted = puncts("+, , *,")
                .parse_iter()
                .split_puncts_all(",")
                .map(|stream| stream.to_string())
                .collect::<Vec<_>>();
            assert_eq!(splitted, [
                "+",
                "",
                "*",
            ]);
        }

        fn parse_iter_split_state() {
            let mut puncts = puncts("-+*,.,").parse_iter();
            assert!(puncts.split_puncts(",+").is_none());
            assert_eq!(puncts.next().unwrap().as_punct_char(), Some('-'));
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
            fn inner1() -> TokenStream {
                let x = 2;
                err!(@("... {x}"));
                #[allow(unreachable_code)]
                {
                    unreachable!();
                }
            }
            assert!(inner().into_iter().next().is_some());
            assert!(inner1().into_iter().next().is_some());
        }

        fn err_macro_test1() {
            fn inner() -> Result<(), TokenStream> {
                rerr!("...", Span::call_site());
                #[allow(unreachable_code)]
                {
                    unreachable!();
                }
            }
            fn inner1() -> Result<(), TokenStream> {
                let x = 2;
                rerr!(@("... {x}"));
                #[allow(unreachable_code)]
                {
                    unreachable!();
                }
            }
            assert!(inner().is_err());
            assert!(inner1().is_err());
        }

        fn walk_test() {
            let input = TokenStream::from_str("(1+2)*3").unwrap();
            let mut acc = vec![];
            let _ = input.walk(|x| { acc.push(x.clone()); x });
            assert_eq!(acc.len(), 6);
            assert!(acc[0].is_literal());
            assert!(acc[1].is_punct());
            assert!(acc[2].is_literal());
            assert!(acc[3].is_group());
            assert!(acc[4].is_punct());
            assert!(acc[5].is_literal());
        }

        fn take_test() {
            let input = TokenStream::from_str("1+2").unwrap();
            let stream = TokenStream::new()
                .push(Literal::i32_unsuffixed(1).into())
                .push(Punct::new('+', Joint).into())
                .push(Literal::i32_unsuffixed(1).into())
                .take();
            let [input, stream] = [
                input.into_iter().collect(),
                stream.into_iter().collect(),
            ] as [Vec<_>; 2];
            assert_eq!(input.len(), stream.len());
            assert!(input[0].is_literal() && stream[0].is_literal());
            assert!(input[1].is_punct() && stream[1].is_punct());
            assert!(input[2].is_literal() && stream[2].is_literal());
        }

        fn group_functions_test() {
            let input = stream([
                'a'.unsuffixed().tt(),
                '+'.punct(Joint).tt(),
                2.unsuffixed().tt(),
            ]);
            let a = brace(input.clone());
            let b = input.grouped_brace();

            assert_eq!(a.delimiter(), b.delimiter());
            assert_eq!(a.stream().into_iter().size_hint(),
                       b.stream().into_iter().size_hint());
            for (a, b) in a.stream().into_iter().zip(b.stream()) {
                assert_eq!(a.kind(), b.kind());
            }
        }

        fn to_brace_stream_test() {
            let a = stream(['+'.punct(Joint).tt()]).grouped_brace();
            assert!(a.to_brace_stream().is_ok());
            assert!(a.to_none_stream().is_err());
            assert!(a.to_paren_stream().is_err());
            assert!(a.to_bracket_stream().is_err());
            assert!(a.into_bracket_stream().is_err());
        }

        fn next_attributes() {
            let src = r#"
                #[a]
                #[b = 2]
                #[c = 2]
                pub fn foo() {}
            "#.parse::<TokenStream>().unwrap();
            let expected = r#"
                pub fn foo() {}
            "#.parse::<TokenStream>().unwrap();

            let mut iter = src.parse_iter();
            assert_eq!(iter.next_attributes().len(), 3);
            let output = stream(iter);
            assert_eq!(output.to_string(), expected.to_string());
        }

        fn next_outer_attributes() {
            let src = r#"
                #![a]
                #![b = 2]
                #[c = 2]
                pub fn foo() {}
            "#.parse::<TokenStream>().unwrap();
            let expected = r#"
                #[c = 2]
                pub fn foo() {}
            "#.parse::<TokenStream>().unwrap();

            let mut iter = src.parse_iter();
            assert_eq!(iter.next_outer_attributes().len(), 2);
            let output = stream(iter);
            assert_eq!(output.to_string(), expected.to_string());
        }

        fn next_vis() {
            let src = r#"
                pub fn foo() {}
            "#.parse::<TokenStream>().unwrap();
            let expected = r#"
                fn foo() {}
            "#.parse::<TokenStream>().unwrap();

            let mut iter = src.parse_iter();
            assert!(iter.next_vis().is_some());
            let output = stream(iter);
            assert_eq!(output.to_string(), expected.to_string());
        }

        fn next_vis1() {
            let src = r#"
                pub(crate) fn foo() {}
            "#.parse::<TokenStream>().unwrap();
            let expected = r#"
                fn foo() {}
            "#.parse::<TokenStream>().unwrap();

            let mut iter = src.parse_iter();
            assert!(iter.next_vis().is_some());
            let output = stream(iter);
            assert_eq!(output.to_string(), expected.to_string());
        }

        fn next_vis2() {
            let src = r#"
                pub(in super) fn foo() {}
            "#.parse::<TokenStream>().unwrap();
            let expected = r#"
                fn foo() {}
            "#.parse::<TokenStream>().unwrap();

            let mut iter = src.parse_iter();
            assert!(iter.next_vis().is_some());
            let output = stream(iter);
            assert_eq!(output.to_string(), expected.to_string());
        }

        fn next_vis3() {
            let src = r#"
                pub(in super) pub fn foo() {}
            "#.parse::<TokenStream>().unwrap();
            let expected = r#"
                pub fn foo() {}
            "#.parse::<TokenStream>().unwrap();

            let mut iter = src.parse_iter();
            assert!(iter.next_vis().is_some());
            let output = stream(iter);
            assert_eq!(output.to_string(), expected.to_string());
        }

        fn next_vis4() {
            let src = r#"
                (in super) pub fn foo() {}
            "#.parse::<TokenStream>().unwrap();
            let expected = r#"
                (in super) pub fn foo() {}
            "#.parse::<TokenStream>().unwrap();

            let mut iter = src.parse_iter();
            assert!(iter.next_vis().is_none());
            let output = stream(iter);
            assert_eq!(output.to_string(), expected.to_string());
        }

        fn next_vis5() {
            let src = r#"
                fn foo() {}
            "#.parse::<TokenStream>().unwrap();
            let expected = r#"
                fn foo() {}
            "#.parse::<TokenStream>().unwrap();

            let mut iter = src.parse_iter();
            assert!(iter.next_vis().is_none());
            let output = stream(iter);
            assert_eq!(output.to_string(), expected.to_string());
        }
    }
}
