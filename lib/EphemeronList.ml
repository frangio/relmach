type ('k, 'd) node =
  | Nil
  | Cons of {
      data : ('k, 'd) Ephemeron.t;
      mutable next : ('k, 'd) node;
    }

type ('k, 'd) t = ('k, 'd) node ref

let create () = ref Nil

let pushe data l = l := Cons { data; next = !l }

let push k d = pushe (Ephemeron.make k d)

let filter_inplace f l =
  let rec set_next l next =
    if l == next then
      ()
    else
      match l with
      | Nil -> ()
      | Cons c ->
          let l' = c.next in
          c.next <- next;
          set_next l' next
  in
  let rec aux root prev = function
    | Nil -> root
    | Cons { data; next } as c ->
        let keep = not (Ephemeron.empty data) && f data in
        if keep then begin
          set_next prev c;
          aux root c next
        end
        else if c == root then
          aux next prev next
        else
          aux root prev next
  in
  let n0 = !l in
  let n0' = aux n0 n0 n0 in
  if n0 != n0' then
    l := n0'
