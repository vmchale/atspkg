abst@ype strlen(n: int)
viewdef string_v(n:int, l:addr) = strlen(n) @ l
vtypedef string_vt(n: int, l:addr) = (string_v(n, l) | ptr(l))

vtypedef String_vt = [n:nat][l:addr | l > null] string_vt(n, l)
