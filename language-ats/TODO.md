# Bugs
- [ ] Fix comments/annotations
  - [ ] I don't really have a good plan for this aside from parsing position and
    comments by default (instead of just position)
  - [ ] Ideally, we'd annotate things, but there is also the possibility of
    using a state monad (?)
- [ ] Preserve `and` val declarations?
- [ ] `as` syntax
- [ ] Boxed records
- [ ] Handle `stadef mytkind = $extkind"atslib_linmap_avltree"`
- [ ] Sort annotations for viewtypes etc.
- [ ] `$tup(x, y)` syntax?
- [ ] `stadef x: bool = z`
- [ ] `language-xats` library: https://github.com/githwxi/ATS-Xanadu/tree/master/srcgen/xats/SATS
- [ ] Fix problem with macro blocks being indented wrong
- [ ] handle `absprop someprop` and `absprop some_prop(prop, prop+)`
- [ ] fix for https://github.com/ats-lang/ats-lang.github.io/blob/master/DOCUMENT/INT2PROGINATS/CODE/CHAP_THMPRVING/sqrt2_irrat.dats
# Deficiencies
- [ ] Error messages
  - [ ] Add test suite for messages
- [ ] track `symintr`?
# Performance
- [ ] `ByteString` lexer?
- [ ] Get rid of `identifierSpace`
- [ ] Literals, etc. should only be lexed when in the appropriate state
