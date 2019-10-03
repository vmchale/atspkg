fun
ifact2
{n:nat} .<>.
(
  n: int (n)
) :<> [r:int] (FACT(n, r) | int r) = let
  fun loop
    {i:nat|i <= n}{r:int} .<n-i>.
  (
    pf: FACT(i, r)
  | n: int n, i: int i, r: int r
  ) :<> [r:int] (FACT(n, r) | int r) =
    if n - i > 0 then let
      val (pfmul | r1) = imul2 (i+1, r) in loop (FACTind(pf, pfmul) | n, i+1, r1)
    end else (pf | r) // end of [if]
  // end of [loop]
in
  loop (FACTbas() | n, 0, 1)
end // end of [ifact2]
