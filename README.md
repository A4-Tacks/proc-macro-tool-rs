Some common simple tool for proc-macro impl

There are many methods available for chain calls

What can do:

- Quickly create processors in the style of `#foo(...)`
- Common methods,
  such as `add`, `split_puncts`, `is_ident`, `as_ident`, `into_punct`, `is_joint`,
  `as_punct_char` and `set_spaned` etc
- `span`, `set_span` traits
- `ParseIter`, can peek n, and peek jointed puncts

# Examples
```rust
extern crate proc_macro;
use proc_macro::*;;
use proc_macro_tool::*;

fn index_tt<I>(mut tt: TokenTree, iter: &mut ParseIter<I>) -> Result<TokenTree, TokenStream>
where I: Iterator<Item = TokenTree>,
{
    while let Some((mut span, mut param)) = iter
        .next_if(|tt| tt.is_delimiter_bracket())
        .map(|tt| tt.into_group().unwrap())
        .map(|g| (g.span_close(), g.stream().into_iter()))
    {
        let i = param.next()
            .ok_or_else(|| err!("unexpected token, expected literal", span))?
            .span_as(&mut span)
            .into_literal()
            .map_err(|_| err!("unexpected token, expected literal", span))?
            .to_string()
            .parse()
            .map_err(|e| err!(@("parse number {e}"), span))?;
        let g = tt.into_group()
            .map_err(|t| err!(@("cannot index {t}, e.g [...]"), span))?;
        tt = g.stream().into_iter().nth(i)
            .ok_or_else(|| err!(@("index {i} out of range, of {}", g), span))?
    };
    Ok(tt)
}
```
