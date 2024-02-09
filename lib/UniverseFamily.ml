type t = {
  mutable dead : bool;
  children : t WeakVec.t;
  depth : int;
  parent : t;
}

let root () =
  let rec u = {
    dead = false;
    children = WeakVec.create ();
    depth = 0;
    parent = u;
  } in
  u

let branch parent =
  let u = {
    dead = false;
    children = WeakVec.create ();
    depth = parent.depth + 1;
    parent;
  } in
  WeakVec.add u parent.children;
  u

let rec find_common u0 u1 =
  if u0.depth > u1.depth then
    find_common u0.parent u1
  else if u0.depth < u1.depth then
    find_common u0 u1.parent
  else if u0 != u1 then
    find_common u0.parent u1.parent
  else
    u0

let join u0 u1 =
  let p = find_common u0 u1 in
  if p == u0 then
    u1
  else if p == u1 then
    u0
  else begin
    let u = branch p in
    WeakVec.add u u0.children;
    WeakVec.add u u1.children;
    u
  end

let rec kill u =
  if not u.dead then begin
    u.dead <- true;
    WeakVec.iter kill u.children
  end

let is_dead u = u.dead
