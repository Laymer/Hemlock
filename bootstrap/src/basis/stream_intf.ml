open Rudiments

module type I_mono_infin = sig
  type elm
end

module type I_mono_fin = sig
  type elm
end

module type I_error = sig
  type t

  val to_string: t -> string
end

module type S_mono_infin = sig
  include I_mono_infin
  type node =
  | Cons of elm * t
  and t = node lazy_t

  val next: t -> node
  val ( ||> ): (elm * t -> 'elm) -> t -> 't
end

module type S_mono_common = sig
  type elm
  type node
  type t

  val ( ||> ): (node -> 'node) -> t -> 't
  val next: t -> node
  val hd: t -> elm
  val tl: t -> t
  val push: elm -> t -> t
  val pop: t -> elm * t
  val drop: usize -> t -> t
end

module type S_mono_fin_common = sig
  type elm
  type node
  type t

  include S_mono_common
    with type elm := elm
     and type node := node
     and type t := t

  val empty: t
  val is_empty: t -> bool
  val length: t -> usize
  val concat: t -> t -> t
end

module type S_mono_fin = sig
  include I_mono_fin
  type node =
  | Nil
  | Cons of elm * t
  and t = node lazy_t

  include S_mono_fin_common
    with type elm := elm
     and type node := node
     and type t := t

  val init: f:('state -> (elm * 'state) option) -> 'state -> t
  val split: usize -> t -> t * t
  val rev_split: usize -> t -> t * t
  val take: usize -> t -> t
  val rev_take: usize -> t -> t
  val drop: usize -> t -> t
  val rev: t -> t
end

module type S_mono_infin_error = sig
  include I_mono_infin

  module Error : sig
    include I_error
  end

  type node =
  | Error of Error.t
  | Cons of elm * t
  and t = node lazy_t

  val next: t -> node
  val ( ||> ): (node -> 'node) -> t -> 't
end

module type S_mono_fin_error = sig
  include I_mono_fin

  module Error : sig
    include I_error
  end
  type node =
  | Nil
  | Error of Error.t
  | Cons of elm * t
  and t = node lazy_t

  val next: t -> node
  val ( ||> ): (node -> 'node) -> t -> 't
end
