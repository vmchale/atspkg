#include "share/atspre_staload.hats"

dataprop PAD(int,int,int) =
  | {p,l:nat} Yep(p,l,p-l)
  | {p,l:nat} Nope(p,l,0)

extern fun left_pad
  {p,l:nat | p>0 && l>0}
  (
    pad: ssize_t p,
    c: charNZ,
    s: strnptr l
  ): [cushion:nat] (PAD(p,l,cushion) | strnptr (cushion+l))

extern fun {t:t@ype} fill_list
  {n:nat}
  (
    size:ssize_t n,
    c: t
  ): list_vt(t,n)

implement {t}fill_list{n}(size,c) =
  let
    fun loop
      {i:nat | i <= n}
      .<i>.
      (
        size : ssize_t i,
        c: t,
        res: list_vt(t, n-i)
      ): list_vt(t,n) =
      if (size = i2ssz(0))
      then res
      else loop(pred size, c, list_vt_cons(c,res))
  in
    loop(size,c,list_vt_nil())
  end

implement left_pad{p,l}(pad,c,s) =
  let
    val size = strnptr_length(s)
  in
    if (pad > size)
    then
      let
        val padding = pad-size
        val char_list = fill_list(padding,c)
        val pad_string = string_make_list_vt(char_list)
        val res = strnptr_append(pad_string, s)
      in
        begin
          strnptr_free(pad_string);
          strnptr_free(s);
          (Yep{p,l} | res)
        end
      end
    else
      (Nope{p,l} | s)
  end

implement main0(argc, argv) =
  let
    val args = listize_argc_argv(argc,argv)
    val _ =
      if list_vt_length(args) = 3
      then (
        let
          val c = '0'
          val s = g1ofg0(args[1]) : [n:nat] string n
          val pad = g1ofg0(g0string2int(args[2]))
        in
          if length(s) > 0 && pad > 0
          then (
            let
              prval _ = lemma_not_empty(s) where {
                extern praxi
                  lemma_not_empty{n:int}(x:string(n)):[n > 0] void
              }
              prval _ = lemma_not_zero(pad) where {
                extern praxi
                  lemma_not_zero{n:int}(x:int(n)):[n > 0] void
              }
              val (pf | res) = left_pad(i2ssz(pad),c, string1_copy(s))
            in
              begin
                println! ("padding: ", res);
                strnptr_free(res);
              end
           end
          )
          else
            print "Usage: left-pad <string-to-pad> <pad-length>\n"
        end
      )
      else print "Usage: left-pad <string-to-pad> <pad-length>\n"
  in
    list_vt_free(args)
  end
