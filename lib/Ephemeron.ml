(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Damien Doligez, projet Para, INRIA Rocquencourt            *)
(*                                                                        *)
(*   Copyright 1997 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module ObjEph = Obj.Ephemeron

type ('k, 'd) t = ObjEph.t

exception Empty

let create () : ('k, 'd) t = ObjEph.create 1

let obj_opt : Obj.t option -> 'a option = fun x -> Obj.magic x

let get_key_opt (t : ('k, 'd) t) : 'k option = obj_opt (ObjEph.get_key t 0)
let set_key (t : ('k, 'd) t) (k : 'k) : unit = ObjEph.set_key t 0 (Obj.repr k)
let check_key (t : ('k, 'd) t) : bool = ObjEph.check_key t 0

let get_data_opt (t : ('k, 'd) t) : 'd option = obj_opt (ObjEph.get_data t)
let set_data (t : ('k, 'd) t) (d : 'd) : unit = ObjEph.set_data t (Obj.repr d)

let opt_get = function
  | Some x -> x
  | None -> raise Empty

let get_key eph = opt_get (get_key_opt eph)
let get_data eph = opt_get (get_data_opt eph)

let make key data =
  let eph = create () in
  set_data eph data;
  set_key eph key;
  eph

let query key eph =
  match get_key_opt eph with
  | Some k when k == key -> get_data_opt eph
  | Some _ | None -> None

let has key eph =
  match get_key_opt eph with
  | Some k -> k == key
  | None -> false

let empty eph = not (check_key eph)

let overwrite key data eph =
  set_data eph data;
  set_key eph key
