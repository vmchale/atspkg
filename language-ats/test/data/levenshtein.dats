staload UN = "prelude/SATS/unsafe.sats"

// Ported over from https://en.wikibooks.org/wiki/Algorithm_Implementation/Strings/Levenshtein_distance#C
fn levenshtein {m:nat}{n:nat}(s1 : string(m), s2 : string(n)) : int =
  let
    val s1_l: size_t(m) = length(s1)
    val s2_l: size_t(n) = length(s2)
    val column: arrayref(int, m+1) = arrayref_make_elt(s1_l + 1, 0)
    var i: int = sz2i(s1_l)
    
    fun loop1 { i : nat | i <= m } .<i>. (i : int(i)) : void =
      case+ i of
        | 0 => ()
        | i =>> {
          val () = column[i] := i
          val () = loop1(i - 1)
        }
    
    val () = loop1(sz2i(s1_l))
    
    val () = while* {i:nat} .<i>. (i: int(i)) => (i > 0)(column[i] := i ; i := i - 1)
    val () = while(i > 0)(column[i] := i ; i := i - 1)
    val () = for* { i : nat | i <= m }  .<i>. (i : int(i)) =>
        (i := sz2i(s1_l) ; i > 0 ; i := i - 1)
        (column[i] := i)
    
    fun loop2 { i : nat | i > 0 && i <= n+1 } .<n-i+1>. (x : int(i)) : void =
      if x <= sz2i(s2_l) then
        {
          val () = column[0] := x
          val () = let
            fun inner_loop { j : nat | j > 0 && j <= m+1 } .<m-j+1>. (y : int(j), last_diag : int) :
              void =
              if y <= sz2i(s1_l) then
                let
                  var old_diag = column[y]
                  
                  fun min_3(x : int, y : int, z : int) : int =
                    min(x, (min(y, z)))
                  
                  fun bool2int(c0 : char, c1 : char) : int =
                    if c0 = c1 then
                      0
                    else
                      1
                  
                  val () = column[y] := min_3( column[y] + 1
                                             , column[y - 1] + 1
                                             , last_diag + bool2int(s1[y - 1], s2[x - 1])
                                             )
                in
                  inner_loop(y + 1, old_diag)
                end
          in
            inner_loop(1, x - 1)
          end
          val () = loop2(x + 1)
        }
    
    val () = loop2(1)
  in
    column[s1_l]
  end

fn levenshtein_vt {m:nat}{n:nat}(s1 : !strnptr(m), s2 : !strnptr(n)) : int =
  let
    var p1 = strnptr2ptr(s1)
    var p2 = strnptr2ptr(s2)
    var s1 = $UN.ptr0_get<string(m)>(p1)
    var s2 = $UN.ptr0_get<string(n)>(p2)
  in
    levenshtein(s1, s2)
  end
