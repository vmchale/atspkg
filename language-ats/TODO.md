# Bugs
- [ ] Fix comments/annotations
  - [ ] I don't really have a good plan for this aside from parsing position and
    comments by default (instead of just position)
  - [ ] Ideally, we'd annotate things, but there is also the possibility of
    using a state monad (?)
- [ ] `as` syntax
- [ ] Handle `stadef mytkind = $extkind"atslib_linmap_avltree"`
- [ ] Sort annotations for viewtypes etc.
- [ ] `$tup(x, y)` syntax?
- [ ] `stadef x: bool = z`
- [ ] `language-xats` library: https://github.com/githwxi/ATS-Xanadu/tree/master/srcgen/xats/SATS
- [ ] Fix problem with macro blocks being indented wrong
- [ ] handle `absprop someprop` and `absprop some_prop(prop, prop+)`
- [ ] Parse `fix` keyword correctly.
- [ ] Lambdas in static functions
- [ ] Fix for `llam@` - linear stack allocated functions
- [ ] Handle `extern prval {A:prop}{B:prop} EMPTY_FUNCTOR {n:nat} : BASE_FUNCTOR_PROP(A, FUNCTOR_PROP(A, n))` correctly
# Deficiencies
- [ ] Error messages
  - [ ] Add test suite for messages
# Performance
- [ ] `ByteString` lexer?
- [ ] Get rid of `identifierSpace`
- [ ] Literals, etc. should only be lexed when in the appropriate state
