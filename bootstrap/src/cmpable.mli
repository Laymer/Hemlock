open Cmpable_intf

module Make_eq (T : I) : S_eq with type t := T.t
module Make_rel (T : I) : S_rel with type t := T.t
