# Bugs
- [ ] Fix comments/annotations
  - [ ] I don't really have a good plan for this aside from parsing position and
    comments by default (instead of just position)
- [ ] Preserve `and` val declarations?
- [ ] Macros with `#else`
- [ ] Boxed tuples/records
- [ ] Handle `stadef mytkind = $extkind"atslib_linmap_avltree"`
- [ ] Sort annotations for viewtypes etc.
# Deficiencies
- [ ] Error messages
  - [ ] Add test suite for messages
# Performance
- [ ] `ByteString` lexer?
- [ ] Get rid of `identifierSpace`
- [ ] Literals, etc. should only be lexed when in the right state
