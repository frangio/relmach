open Types

module Family = UniverseFamily

let root () = {
  family = Family.root ();
  unifier = Unifier.empty;
  last_merged = Weak.create 2;
}

let branch u =
  let family = Family.branch u.family in
  let last_merged = Weak.create 2 in
  Weak.blit u.last_merged 0 last_merged 0 2;
  { u with family; last_merged }

let kill u = Family.kill u.family
let is_dead u = Family.is_dead u.family

let last_any w x =
  match Weak.(get w 0, get w 1) with
  | Some y, _ when x == y -> true
  | _, Some y when x == y -> true
  | _ -> false

let join u0 u1 =
  let merge_into u =
    match Unifier.merge u0.unifier u1.unifier with
    | None ->
        kill u;
        None
    | Some g ->
        Weak.set u.last_merged 0 (Some u0.unifier);
        Weak.set u.last_merged 1 (Some u1.unifier);
        u.unifier <- g;
        Some u
  in
  let f = Family.join u0.family u1.family in
  if f == u0.family then
    if last_any u0.last_merged u1.unifier then
      Some u0
    else 
      merge_into u0
  else if f == u1.family then
    if last_any u1.last_merged u0.unifier then
      Some u1
    else 
      merge_into u1
  else
    let last_merged = Weak.create 2 in
    let u = { family = f; unifier = Unifier.empty; last_merged } in
    merge_into u

