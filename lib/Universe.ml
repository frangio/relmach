open Types

module Family = UniverseFamily

let root () = {
  family = Family.root ();
  unifier = Unifier.empty;
}

let branch u =
  let family = Family.branch u.family in
  { u with family }

let kill u = Family.kill u.family
let is_dead u = Family.is_dead u.family

let join u0 u1 =
  let merge_into u =
    match Unifier.merge u0.unifier u1.unifier with
    | None ->
        kill u;
        None
    | Some g ->
        u.unifier <- g;
        Some u
  in
  let f = Family.join u0.family u1.family in
  if f == u0.family then
    merge_into u0
  else if f == u1.family then
    merge_into u1
  else
    let u = { family = f; unifier = Unifier.empty } in
    merge_into u
