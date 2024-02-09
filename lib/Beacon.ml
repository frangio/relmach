type status = Alive | Dead | Joined

type alternative = {
  group : group;
  mutable status : status;
  mutable children : alternative Weak.t;
}

and group = {
  mutable alive : int;
  mutable announce : channel;
}

and channel = Root | Parent of alternative | Cousin of group

let make_group parent = { alive = 0; announce = Parent parent }

let make_alternative group =
  group.alive <- group.alive + 1;
  { group; status = Alive; children = Weak.create 0 }

let root () = make_alternative { alive = 1; announce = Root }

let dead a = a.status == Dead

let add_child a c =
  (* todo: resize in power of 2 *)
  let weak_push xs x =
    let open Weak in
    let n = length xs in
    let xs' = create (n + 1) in
    blit xs 0 xs' 0 n;
    set xs' n (Some x);
    xs'
  in
  a.children <- weak_push a.children c

let split parent n =
  let group = make_group parent in
  let make_child _ = 
    let c = make_alternative group in
    add_child parent c;
    c
  in
  Array.init n make_child

let rec kill a =
  let rec kill_down a =
    if a.status = Alive then begin
      a.status <- Dead;
      for i = 0 to Weak.length a.children - 1 do
        Weak.get a.children i |> Option.iter kill_down
      done
    end
  in
  let rec announce_kill g =
    g.alive <- g.alive - 1;
    if g.alive = 0 then
      match g.announce with
      | Root -> ()
      | Parent p -> kill p
      | Cousin g -> announce_kill g
  in
  kill_down a;
  announce_kill a.group

let regroup a g =
  let rec find_parent g =
    match g.announce with
    | Cousin g -> find_parent g
    | p -> p
  in

  (* let rec find_immediate_child pc g = *)
  (*   let c = g.announce in *)
  (*   match c with *)
  (*   | Cousin g' -> assert (g == g'); failwith "todo" *)
  (*   | Parent _ when c == pc -> g *)
  (*   | Root -> assert (c == pc); g *)
  (* in *)

  let p = find_parent g in
  if a.group.announce == p then
    failwith "todo"

let join g (a0 : alternative) (a1 : alternative) =
  regroup a0 g;
  regroup a1 g;
  let jb = make_alternative g in
  kill a0;
  kill a1;
  jb
