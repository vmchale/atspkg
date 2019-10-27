staload "SATS/wc.sats"
staload "SATS/memchr.sats"
staload "SATS/bytecount.sats"
staload "prelude/SATS/pointer.sats"
staload UN = "prelude/SATS/unsafe.sats"

#include "DATS/bytecount.dats"
#include "DATS/memchr.dats"

#define BUFSZ 32768

%{
size_t sub_ptr1_ptr1_size(atstype_ptr p1, atstype_ptr p2) {
  return ((char *)p1 - (char *)p2);
}
%}

extern
fn sub_ptr1_ptr1_size { l0, l1 : addr | l0 >= l1 }(p1 : ptr(l0), p2 : ptr(l1)) :<> size_t(l0-l1) =
  "ext#"

extern
castfn bounded(size_t) : [n:nat] size_t(n)

fn bptr_succ {l:addr}(p : ptr(l)) :<> ptr(l+1) =
  $UN.cast(ptr1_succ<byte>(p))

fn freadc { l : addr | l != null }{ sz : nat | sz >= 1 }(pf : !bytes_v(l, sz)
                                                        | inp : !FILEptr1, bufsize : size_t(sz), p : ptr(l)) : size_t =
  let
    extern
    castfn as_fileref(x : !FILEptr1) :<> FILEref
    
    var n = $extfcall(size_t, "fread", p, sizeof<byte>, bufsize, as_fileref(inp))
  in
    n
  end

fn freadc_ { l : addr | l != null }{ sz : nat | sz >= 1 }{ n : nat | n <= sz }( pf : !bytes_v(l, sz) | inp : !FILEptr1
                                                                              , bufsize : size_t(sz)
                                                                              , p : ptr(l)
                                                                              ) : size_t(n) =
  $UN.cast(freadc(pf | inp, bufsize, p))

implement free_st (st) =
  case+ st of
    | ~in_string () => ()
    | ~in_block_comment () => ()
    | ~line_comment() => ()
    | ~post_asterisk_in_block_comment() => ()
    | ~post_backslash_in_string() => ()
    | ~post_slash() => ()
    | ~regular() => ()
    | ~post_newline_whitespace() => ()
    | ~post_block_comment() => ()
    | ~post_tick() => ()

implement empty_file =
  @{ lines = 0, blanks = 0, comments = 0, doc_comments = 0 } : file

implement add_file (f0, f1) =
  @{ lines = f0.lines + f1.lines
   , blanks = f0.blanks + f1.blanks
   , comments = f0.comments + f1.comments
   , doc_comments = f0.doc_comments + f1.doc_comments
   } : file

fn byteview_read_as_char {l0:addr}{m:nat}{ l1 : addr | l1 <= l0+m }(pf : !bytes_v(l0, m) | p : ptr(l1)) : char =
  $UN.ptr0_get<char>(p)

fn count_for_loop { l : addr | l != null }{m:nat}{ n : nat | n <= m }( pf : !bytes_v(l, m) | p : ptr(l)
                                                                     , parse_st : &parse_state >> _
                                                                     , bufsz : size_t(n)
                                                                     ) : file =
  let
    fn advance_char(c : char, st : &parse_state >> _, file_st : &file >> _) : void =
      case+ st of
        | regular() => 
          begin
            case+ c of
              | '\n' => (free(st) ; file_st.lines := file_st.lines + 1 ; st := post_newline_whitespace)
              | '\'' => (free(st) ; st := post_tick)
              | '"' => (free(st) ; st := in_string)
              | '/' => (free(st) ; st := post_slash)
              | _ => ()
          end
        | in_string() => 
          begin
            case+ c of
              | '\n' => file_st.lines := file_st.lines + 1
              | '\\' => (free(st) ; st := post_backslash_in_string)
              | '"' => (free(st) ; st := regular)
              | _ => ()
          end
        | post_asterisk_in_block_comment() => 
          begin
            case+ c of
              | '/' => (free(st) ; st := post_block_comment)
              | '\n' => (free(st) ; file_st.comments := file_st.comments + 1 ; st := in_block_comment)
              | '*' => ()
              | _ => (free(st) ; st := in_block_comment)
          end
        | in_block_comment() => 
          begin
            case+ c of
              | '*' => (free(st) ; st := post_asterisk_in_block_comment)
              | '\n' => file_st.comments := file_st.comments + 1
              | _ => ()
          end
        | line_comment() => 
          begin
            case+ c of
              | '\n' => (free(st) ; file_st.comments := file_st.comments + 1 ; st := post_newline_whitespace)
              | _ => ()
          end
        | post_backslash_in_string() => 
          begin
            case+ c of
              | '\n' => (free(st) ; file_st.lines := file_st.lines + 1 ; st := in_string)
              | _ => ()
          end
        | ~post_slash() => 
          begin
            case+ c of
              | '/' => st := line_comment
              | '\n' => (file_st.lines := file_st.lines + 1 ; st := post_newline_whitespace)
              | '*' => st := in_block_comment
              | _ => st := regular
          end
        | post_newline_whitespace() => 
          begin
            case+ c of
              | '\n' => (file_st.blanks := file_st.blanks + 1)
              | '\t' => ()
              | ' ' => ()
              | '/' => (free(st) ; st := post_slash)
              | '\'' => (free(st) ; st := post_tick)
              | '"' => (free(st) ; st := in_string)
              | _ => (free(st) ; st := regular)
          end
        | post_block_comment() => 
          begin
            // TODO: block comments at the end of a line
            case+ c of
              | '\n' => (free(st) ; file_st.comments := file_st.comments + 1 ; st := post_newline_whitespace)
              | '/' => (free(st) ; st := post_slash)
              | '"' => (free(st) ; st := in_string)
              | _ => ()
          end
        | ~post_tick() => st := regular
    
    var res: file = empty_file
    var i: size_t
    val () = for* { i : nat | i <= m } .<i>. (i : size_t(i)) =>
        (i := bufsz ; i != 0 ; i := i - 1)
        (let
          var current_char = byteview_read_as_char(pf | add_ptr_bsz(p, i))
        in
          advance_char(current_char, parse_st, res)
        end)
    var current_char = byteview_read_as_char(pf | p)
    val () = advance_char(current_char, parse_st, res)
  in
    res
  end

fn count_lines_for_loop { l : addr | l != null }{m:nat}{ n : nat | n <= m }(pf : !bytes_v(l, m)
                                                                           | ptr : ptr(l), bufsz : size_t(n)) : int =
  let
    var res: int = 0
    var i: size_t
    val () = for* { i : nat | i <= n } .<n-i>. (i : size_t(i)) =>
        (i := i2sz(0) ; i < bufsz ; i := i + 1)
        (let
          var current_char = byteview_read_as_char(pf | add_ptr_bsz(ptr, i))
        in
          case+ current_char of
            | '\n' => res := res + 1
            | _ => ()
        end)
  in
    res
  end

fn count_file_for_loop(inp : !FILEptr1) : int =
  let
    val (pfat, pfgc | p) = malloc_gc(g1i2u(BUFSZ))
    prval () = pfat := b0ytes2bytes_v(pfat)
    
    fun loop { l : addr | l != null }(pf : !bytes_v(l, BUFSZ) | inp : !FILEptr1, p : ptr(l)) : int =
      let
        var file_bytes = freadc(pf | inp, i2sz(BUFSZ), p)
        
        extern
        praxi lt_bufsz {m:nat} (size_t(m)) : [m <= BUFSZ] void
      in
        if file_bytes = 0 then
          0
        else
          let
            var fb_prf = bounded(file_bytes)
            prval () = lt_bufsz(fb_prf)
            var acc = count_lines_for_loop(pf | p, fb_prf)
          in
            acc + loop(pf | inp, p)
          end
      end
    
    var ret = loop(pfat | inp, p)
    val () = mfree_gc(pfat, pfgc | p)
  in
    ret
  end

fn count_file(inp : !FILEptr1) : file =
  let
    val (pfat, pfgc | p) = malloc_gc(g1i2u(BUFSZ))
    prval () = pfat := b0ytes2bytes_v(pfat)
    var init_st: parse_state = post_newline_whitespace
    
    fun loop { l : addr | l != null }(pf : !bytes_v(l, BUFSZ) | inp : !FILEptr1, st : &parse_state >> _, p : ptr(l)) :
      file =
      let
        var file_bytes = freadc(pf | inp, i2sz(BUFSZ), p)
        
        extern
        praxi lt_bufsz {m:nat} (size_t(m)) : [m <= BUFSZ] void
      in
        if file_bytes = 0 then
          empty_file
        else
          let
            var fb_prf = bounded(file_bytes)
            prval () = lt_bufsz(fb_prf)
            var acc = count_for_loop(pf | p, st, fb_prf)
          in
            acc + loop(pf | inp, st, p)
          end
      end
    
    var ret = loop(pfat | inp, init_st, p)
    val () = free(init_st)
    val () = mfree_gc(pfat, pfgc | p)
  in
    ret
  end
