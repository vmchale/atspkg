// bad (?) idea: use rawmemchr + append lol
fn memchr { l : addr | l != null }{m:nat}{ n : nat | n <= m }(bytes_v(l,m) | ptr(l), int, size_t(n)) :
  [ l0 : addr | l0 == null || l0 >= l && l0-l <= m ] (bytes_v(l, l0-l), bytes_v(l0, l+m-l0)| ptr(l0)) =
  "mac#"

fn memchr2_rs { l : addr | l != null }{m:nat}{ n : nat | n <= m }(!bytes_v(l, m) | ptr(l), char, char, size_t(n)) :
  [ k : nat | k <= n || k == 18446744073709551615 ] size_t(k) =
  "ext#"

fn memchr2 { l : addr | l != null }{m:nat}{ n : nat | n <= m }(!bytes_v(l, m) | ptr(l), char, char, size_t(n)) :
  Option_vt([ k : nat | k <= n ] size_t(k)) =
  "ext#"

fn memchr3_rs { l : addr | l != null }{m:nat}{ n : nat | n <= m }(!bytes_v(l, m) | ptr(l), char, char, char, size_t(n)) 
  : [ k : nat | k <= n || k == 18446744073709551615 ] size_t(k) =
  "ext#"
fn memchr3 { l : addr | l != null }{m:nat}{ n : nat | n <= m }(!bytes_v(l, m) | ptr(l), char, char, char, size_t(n)) 
  : Option_vt([ k : nat | k <= n ] size_t(k)) =
  "ext#"
