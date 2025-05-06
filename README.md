Some common simple tool for proc-macro impl

What can do:

- Quickly create processors in the style of `#foo(...)`
- Common methods,
  such as `add`, `split_puncts`, `is_ident`, `as_ident`, `into_punct`, `is_joint`,
  `as_punct_char` and `set_spaned` etc
- `span`, `set_span` traits
- `ParseIter`, can peek n, and peek jointed puncts
