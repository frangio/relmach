let alloc_hash =
  let next = ref Int.min_int in
  fun () ->
    let n = !next in
    incr next;
    Hashtbl.hash n
