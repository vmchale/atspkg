typedef dlist(a: t@ype+) = @{ f = List0(a) -> List0(a) }

fn from_list {a:t@ype} (List0(a)) : dlist(a)

fn to_list {a:t@ype} (dlist(a)) : List0(a)

fn singleton {a:t@ype} (a) : dlist(a)

val empty {a:t@ype} : dlist(a)
