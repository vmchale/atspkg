#include "share/atspre_staload.hats"
#include "share/HATS/atslib_staload_libats_libc.hats"

staload UN = "prelude/SATS/unsafe.sats"
staload "src/types.sats"
staload "prelude/basics_sta.sats"
staload "libats/libc/SATS/stdio.sats"
staload "prelude/SATS/string.sats"

fun snoc(s : string, c : char) : string =
  let
    val sc = char2string(c)
    val x = string0_append(s, sc)
  in
    strptr2string(x)
  end

fun next {m : nat} (x : string(m)) : Option_vt(char) =
  if length(x) > 0 then
    Some_vt(string_head(x))
  else
    None_vt

fun stop_plain_string(c : char) : bool =
  case+ c of
    | '\n' => true
    | '#' => true
    | _ => false

fun map {a : vtype}{b : vtype} (f : a -<lincloptr1> b, x : parser(a)) : parser(b) =
  let
    val g = x.modify
  in
    @{ modify = llam c =<lincloptr1> 
      begin
        let
          val (y, z): (cstream, a) = g(c)
          val w: b = f(z)
        in
          (cloptr_free($UN.castvwtp0(f)) ; cloptr_free($UN.castvwtp0(g)) ; (y, w))
        end
      end }
  end

extern
fun bind {a : vtype}{b : vtype} (x : parser(a), f : a -<lincloptr1> parser(b)) : parser(b)

fun pure {a : vtype} (x : a) : parser(a) =
  @{ modify = llam c =<lincloptr1> (c, x) }

fun chain {a : vtype}{b : vtype} (x : parser(a), y : parser(b)) : parser(b) =
  @{ modify = llam c =<lincloptr1> let
    val f = x.modify
    val g = y.modify
    val (pre_res, _) = f(c)
    val (res, y) = g(pre_res)
    val _ = cloptr_free($UN.castvwtp0(f))
    val _ = cloptr_free($UN.castvwtp0(g))
  in
    (res, y)
  end }

fun run_parser {a : vtype} (in_stream : cstream, parser : parser(a)) : a =
  let
    val g = parser.modify
    val (s, z) = g(in_stream)
    val _ = stream_vt_free(s)
    val _ = cloptr_free($UN.castvwtp0(g))
  in
    z
  end

fun consume_space() : parser(null) =
  pre_consume_space() where
  { fun pre_consume_space() : parser(null) =
      let
        fnx loop(input : cstream) : (cstream, null) =
          case+ !input of
            | ~stream_vt_cons (' ', xs) => loop(xs)
            | ~stream_vt_cons (_, xs) => (xs, null)
            | ~stream_vt_nil() => ($ldelay(stream_vt_nil), null)
      in
        @{ modify = llam (input) =<lincloptr1> loop(input) }
      end }

fun consume_int() : parser(token) =
  pre_consume_int() where
  { fun pre_consume_int() : parser(token) =
      let
        fun loop(input : cstream, data : int) : (cstream, int) =
          case+ !input of
            | ~stream_vt_cons ('0', xs) => loop(xs, 10 * data)
            | ~stream_vt_cons ('1', xs) => loop(xs, 10 * data + 1)
            | ~stream_vt_cons ('2', xs) => loop(xs, 10 * data + 2)
            | ~stream_vt_cons ('3', xs) => loop(xs, 10 * data + 3)
            | ~stream_vt_cons ('4', xs) => loop(xs, 10 * data + 4)
            | ~stream_vt_cons ('5', xs) => loop(xs, 10 * data + 5)
            | ~stream_vt_cons ('6', xs) => loop(xs, 10 * data + 6)
            | ~stream_vt_cons ('7', xs) => loop(xs, 10 * data + 7)
            | ~stream_vt_cons ('8', xs) => loop(xs, 10 * data + 8)
            | ~stream_vt_cons ('9', xs) => loop(xs, 10 * data + 9)
            | ~stream_vt_cons (_, xs) => (xs, data)
            | ~stream_vt_nil() => ($ldelay(stream_vt_nil), data)
        
        fun data(input : cstream) : (cstream, token) =
          let
            val (x, y) = loop(input, 0)
          in
            (x, int_tok(y))
          end
      in
        @{ modify = llam (input) =<lincloptr1> data(input) }
      end }

fun is_letter(c : char) : bool =
  case+ c of
    | 'a' => true
    | 'b' => true
    | 'c' => true
    | 'd' => true
    | 'e' => true
    | 'f' => true
    | 'g' => true
    | 'h' => true
    | 'i' => true
    | 'j' => true
    | 'k' => true
    | 'l' => true
    | 'm' => true
    | 'n' => true
    | 'o' => true
    | 'p' => true
    | 'q' => true
    | 'r' => true
    | 's' => true
    | 't' => true
    | 'u' => true
    | 'v' => true
    | 'w' => true
    | 'x' => true
    | 'y' => true
    | 'z' => true
    | '-' => true
    | _ => false

fun free_tok(t : token) : void =
  case+ t of
    | ~string_tok (_) => ()
    | ~int_tok (_) => ()
    | ~eq_tok() => ()
    | ~pound_tok() => ()
    | ~float_tok (_) => ()
    | ~bool_tok (_) => ()

fun mk_eq(s : string) : token =
  eq_tok()

// TODO consider list_vt(char)? what would that accomplish/help
// FIXME this is stupid as hell.
fun consume_eq() : parser(token) =
  map(llam x =<lincloptr1> mk_eq(x), pre_consume_identifier()) where
  { fun pre_consume_identifier() : parser(string) =
      let
        fun loop(input : cstream, data : string) : (cstream, string) =
          case+ !input of
            | ~stream_vt_cons ('=', xs) => loop(xs, snoc(data, '='))
            | ~stream_vt_cons (_, xs) => (xs, data)
            | ~stream_vt_nil() => ($ldelay(stream_vt_nil), "")
        
        fun data(input : cstream) : (cstream, string) =
          loop(input, "")
      in
        @{ modify = llam (input) =<lincloptr1> data(input) }
      end }

fun consume_identifier() : parser(token) =
  map(llam x =<lincloptr1> string_tok(x), pre_consume_identifier()) where
  { fun pre_consume_identifier() : parser(string) =
      let
        fun loop(input : cstream, data : string) : (cstream, string) =
          case+ !input of
            | ~stream_vt_cons (c, xs) when is_letter(c) => loop(xs, snoc(data, c))
            | ~stream_vt_cons (_, xs) => (xs, data)
            | ~stream_vt_nil() => ($ldelay(stream_vt_nil), "")
        
        fun data(input : cstream) : (cstream, string) =
          loop(input, "")
      in
        @{ modify = llam (input) =<lincloptr1> data(input) }
      end }

fun consume_quoted() : parser(token) =
  map(llam x =<lincloptr1> string_tok(x), pre_consume_quoted()) where
  { fun pre_consume_quoted() : parser(string) =
      let
        fun loop(input : cstream, is_escaped : bool, data : string) : (cstream, string) =
          case+ !input of
            | ~stream_vt_cons ('\\', xs) => loop(xs, true, data)
            | ~stream_vt_cons (x, xs) => 
              begin
                if not(is_escaped) then
                  if x = '"' then
                    (xs, data)
                  else
                    loop(xs, false, snoc(data, x))
                else
                  loop(xs, false, snoc(data, x))
              end
            | ~stream_vt_nil() => (prerr!("Error: missing \"") ; exit(1) ; ($ldelay(stream_vt_nil), ""))
        
        fun data(input : cstream) : (cstream, string) =
          loop(input, false, "")
      in
        @{ modify = llam (input) =<lincloptr1> data(input) }
      end }

fun tokenize(input : cstream) : tstream =
  case+ !input of
    | ~stream_vt_cons (x, xs) => (stream_vt_free(xs) ; $ldelay(stream_vt_nil()))
    | ~stream_vt_nil() => $ldelay(stream_vt_nil())

fun display_token(t : token) : void =
  case+ t of
    | ~string_tok (s) => println!(s)
    | ~int_tok (i) => println!(tostring_int(i))
    | ~eq_tok() => println!("=")
    | ~pound_tok() => println!("#")
    | ~float_tok (x) => ()
    | ~bool_tok (b) => ()

implement main0 () =
  let
    var fr = fileref_open_exn("junk1", file_mode_r)
    var stream = streamize_fileref_char(fr)
    val t1: token = run_parser(stream, consume_quoted())
    var fr = fileref_open_exn("junk2", file_mode_r)
    var stream = streamize_fileref_char(fr)
    val t2: token = run_parser(stream, consume_int())
    var fr = fileref_open_exn("junk3", file_mode_r)
    var stream = streamize_fileref_char(fr)
    val t3: token = run_parser(stream, chain(consume_identifier(), chain(consume_eq(), consume_int())))
  in
    (display_token(t1) ; display_token(t2) ; display_token(t3))
  end