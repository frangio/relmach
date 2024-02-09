type cont = Ret | Left of frame | Right of frame

and frame = {
  parent : cont;
  depth : int;
  mutable left : unit list;
  mutable right : unit list;
  mutable expected_left : int;
  mutable expected_right : int;
  mutable last_synced : int;
}

let update_expected delta = function
  | Left fr -> fr.expected_left <- fr.expected_left + delta
  | Right fr -> fr.expected_right <- fr.expected_right + delta
  | Ret -> ()

let sync_expected fr =
  let exp_left = fr.expected_left in
  let exp_right = fr.expected_right in
  let prev_left = List.length fr.left in
  let prev_right = List.length fr.right in
  let expected = exp_left * exp_right + exp_left * prev_right + prev_left * exp_right in
  update_expected (expected - fr.last_synced) fr.parent;
  fr.last_synced <- expected

