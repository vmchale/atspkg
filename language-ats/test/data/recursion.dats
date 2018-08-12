absprop FUNCTOR_PROP (A : prop, n : int)

absprop BASE_FUNCTOR_PROP (A : prop, B : prop)

dataprop LIST_PROP(A: prop, int) =
  | LIST_PROP_NIL(A, 0) of ()
  | { n : nat | n > 0 } LIST_PROP_CONS(A, n) of (A, LIST_PROP(A, n - 1))

dataprop LISTF_PROP(A: prop, B: prop) =
  | LISTF_PROP_NIL(A, B) of ()
  | LISTF_PROP_CONS(A, B) of (A, B)

extern
prfun MAP {A:prop}{B:prop}{C:prop} (F : B -<prf> C, X : BASE_FUNCTOR_PROP(A, B)) : BASE_FUNCTOR_PROP(A, C)

propdef ALGEBRA (A : prop, B : prop) = BASE_FUNCTOR_PROP(A, B) -<prf> B

extern
prfun {A:prop} PROJECT {n:nat} (FUNCTOR_PROP(A,n)) : BASE_FUNCTOR_PROP(A, FUNCTOR_PROP(A,n-1))

extern
prfn {A:prop}{B:prop} EMPTY_FUNCTOR {n:nat}  : BASE_FUNCTOR_PROP(A, FUNCTOR_PROP(A,n))

assume FUNCTOR_PROP(A, n) = LIST_PROP(A, n)
assume BASE_FUNCTOR_PROP(A, B) = LISTF_PROP(A, B)

prfun {A:prop}{B:prop} CATA {n:nat} .<n>. (F : ALGEBRA(A, B), A : FUNCTOR_PROP(A, n)) : B =
  sif n == 0 then
    F(LISTF_PROP_NIL)
  else
    F(MAP(lam A0 =<prf> CATA(F,A0),PROJECT(A)))

primplmnt MAP (F, XS) =
  case+ XS of
    | LISTF_PROP_NIL() => LISTF_PROP_NIL()
    | LISTF_PROP_CONS (Y, YS) => LISTF_PROP_CONS(Y,F(YS))

primplmnt {A} PROJECT (A) =
  case+ A of
    | LIST_PROP_NIL() => LISTF_PROP_NIL()
    | LIST_PROP_CONS (B, BS) => LISTF_PROP_CONS(B,BS)
