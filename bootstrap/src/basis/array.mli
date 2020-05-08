open Rudiments

type 'a t = 'a array
include Container_common_intf.S_poly_fold with type 'a t := 'a t

include Formattable_intf.S_poly with type 'a t := 'a t

(** Cursor that supports arbitrary array element access.  All operations are
    O(1). *)
module Cursor : sig
  type 'a container = 'a t
  type 'a t

  include Cursor_intf.S_poly with type 'a container := 'a container
                              and type 'a elm := 'a
                              and type 'a t := 'a t
end

val hash_fold: ('a -> Hash.State.t -> Hash.State.t) -> 'a t -> Hash.State.t
  -> Hash.State.t
(** [hash_fold hash_fold_a t state] incorporates the hash of [t] into [state]
    and returns the resulting state.  Array elements are sequentially
    hash-folded into the resulting state via [hash_fold_a]. *)

val cmp: ('a -> 'a -> Cmp.t) -> 'a t -> 'a t -> Cmp.t
(** Compare two arrays given the element comparison function.  The array lengths
    may differ. *)

(*
type 'a slice
module Slice : sig
  type 'a outer = 'a t
  type 'a t = 'a slice

  val of_cursors: base:'a Cursor.t -> past:'a Cursor.t -> 'a t
  (** [of_cursors ~base ~past] creates a slice with contents \[[base .. past)].
  *)

  val to_cursors: 'a t -> 'a Cursor.t * 'a Cursor.t
  (** Return the cursors comprising the slice. *)

  val array: 'a t -> 'a outer
  (** [string t] returns the unsliced string underlying [t]. *)

  val base: 'a t -> 'a Cursor.t
  (** Return the cursor at the base of the slice. *)

  val past: 'a t -> 'a Cursor.t
  (** Return the cursor past the end of the slice. *)

  val of_array: 'a outer -> 'a t
  (** [of_string s] returns a slice enclosing the entirety of [s]. *)

  val to_array: 'a t -> 'a outer
  (** Return a string with contents equivalent to those of the slice. *)

  val base_seek: isize -> 'a t -> 'a t
  (** [base_seek i t] returns a derivative slice with its [base] cursor
      initialized by seeking [t]'s [base] cursor [i] codepoints
      forward/backward. *)

  val base_succ: 'a t -> 'a t
  (** [base_succ t] returns a derivative slice with its [base] cursor
      initialized to the successor of [t]'s [base] cursor. *)

  val base_pred: 'a t -> 'a t
  (** [base_pred t] returns a derivative slice with its [base] cursor
      initialized to the predecessor of [t]'s [base] cursor. *)

  val past_seek: isize -> 'a t -> 'a t
  (** [past_seek i t] returns a derivative slice with its [past] cursor
      initialized by seeking [t]'s [past] cursor [i] codepoints
      forward/backward. *)

  val past_succ: 'a t -> 'a t
  (** [past_succ t] returns a derivative slice with its [past] cursor
      initialized to the successor of [t]'s [past] cursor. *)

  val past_pred: 'a t -> 'a t
  (** [past_pred t] returns a derivative slice with its [past] cursor
      initialized to the predecessor of [t]'s [past] cursor. *)

  val length: 'a t -> usize
  (** Length of the slice. *)

  val get: usize -> 'a t -> byte
  (** [get i t] returns the bytes at offset [i] from the [base] of the slice. *)

  val init: ?length:usize -> usize -> f:(usize -> 'a) -> 'a t
  (** [init ~blength clength ~f] creates a slice of given byte length and
      codepoint length using ~f to determine the values of the codepoints at
      each index.  [blength] must be accurate if specified. *)

  val of_elm: 'a -> 'a t
  (** Create a slice containing a single codepoint. *)

  val of_list: ?length:usize -> 'a list -> 'a t
  (** [of_list ~blength ~clength codepoints] creates a slice of given byte
      length and codepoint length containing the ordered [codepoints].
      [blength]/[clength] must be accurate if specified. *)

  val of_list_rev: ?length:usize -> 'a list -> 'a t
  (** [of_list_rev ~blength ~clength codepoints] creates a slice of given byte
      length and codepoint length containing the reverse-ordered [codepoints].
      [blength]/[clength] must be accurate if specified. *)

  val of_array: ?length:usize -> 'a outer -> 'a t
  (** [of_array ~blength codepoints] creates a slice of given byte length
      containing the ordered [codepoints].  [blength] must be accurate if
      specified. *)

  val map: f:('a -> 'a) -> 'a t -> 'a t
  (** [map ~f t] creates a slice with codepoints mapped from [t]'s codepoints.
  *)

  val mapi: f:(usize -> 'a -> 'a) -> 'a t -> 'a t
  (** [map ~f t] creates a slice with codepoints mapped from [t]'s codepoints.
      Codepoint index within [t] is provided to [f]. *)

  val tr: target:'a -> replacement:'a -> 'a t -> 'a t
  (** [tr ~target ~replacement t] creates a slice with select codepoints
      translated from [t]'s codepoints.  [target] is translated to
      [replacement]; all other codepoints are copied without translation. *)

  val filter: f:('a -> bool) -> 'a t -> 'a t
  (** [filter ~f t] creates a slice from [t]'s codepoints filtered by [f].  Only
      codepoints for which [f] returns [true] are incorporated into the result.
  *)

  val concat: ?sep:'a t -> 'a t list -> 'a t
  (** [concat ~sep slices] creates a slice comprised of the concatenation of the
      [slices] list, with [sep] interposed between the inputs. *)

  val concat_rev: ?sep:'a t -> 'a t list -> 'a t
  (** [concat_rev ~sep slices] creates a slice comprised of the concatenation of
      reversed [slices] list, with [sep] interposed between the inputs. *)

  val concat_map: ?sep:'a t -> f:('a -> 'a t) -> 'a t -> 'a t
  (** [concat_map ~sep ~f t] creates a slice which is the concatenation of
      applying [f] to convert each codepoint to a slice.  This is more general
      (and more expensive) than {!map}, because each input codepoint can be
      mapped to any length of output. *)

  val rev: 'a t -> 'a t
  (** [rev t] creates a slice with the codepoint ordering reversed relative to
      [t]. *)

  val lfind: 'a -> 'a t -> 'a Cursor.t option
  (** [lfind cp t] returns a cursor to the leftmost instance of [cp] in [t], or
      [None] if [cp] is absent. *)

  val lfind_hlt: 'a -> 'a t -> 'a Cursor.t
  (** [lfind_hlt cp t] returns a cursor to the leftmost instance of [cp] in [t],
      or halts if [cp] is absent. *)

  val contains: 'a -> 'a t -> bool
  (** [contains cp t] returns [true] if [t] contains [cp], [false] otherwise. *)

  val rfind: 'a -> 'a t -> 'a Cursor.t option
  (** [rfind cp t] returns a cursor to the rightmost instance of [cp] in [t], or
      [None] if [cp] is absent. *)

  val rfind_hlt: 'a -> 'a t -> 'a Cursor.t
  (** [rfind_hlt cp t] returns a cursor to the rightmost instance of [cp] in
      [t], or halts if [cp] is absent. *)

  (** Simple but efficient pattern matching, based on the {{:
      https://en.wikipedia.org/wiki/Knuth-Morris-Pratt_algorithm}
      Knuth-Morris-Pratt algorithm}.  Patterns are uninterpreted codepoint
      sequences.  Searches require at most a single pass over the input,
      regardless of whether finding one or more (optionally overlapping)
      matches.
  *)
  module Pattern : sig
    type 'a outer = 'a t

    type 'a t
    (** Compiled pattern. *)

    val create: 'a outer -> 'a t
    (** [create s] creates a compiled pattern corresponding to [s]. *)

    val find: in_:'a outer -> 'a t -> 'a Cursor.t option
    (** [find ~in_ t] returns a cursor to the leftmost match in [in_], or [None]
        if no match exists. *)

    val find_hlt: in_:'a outer -> 'a t -> 'a Cursor.t
    (** [find_hlt ~in_ t] returns a cursor to the leftmost match in [in_], or
        halts if no match exists. *)

    val find_all: may_overlap:bool -> in_:'a outer -> 'a t -> 'a Cursor.t list
    (** [find_all ~may_overlap ~in_ t] returns a list of cursors to the matches
        in [in_].  Non-leftmost overlapping matches are excluded if
        [may_overlap] is [false]. *)

    val replace_first: in_:'a outer -> with_:'a outer -> 'a t -> 'a outer
    (** [replace_first ~in_ ~with_ t] returns a slice with the first match in
        [in_], if any, replaced with [with_] in the result. *)

    val replace_all: in_:'a outer -> with_:'a outer -> 'a t -> 'a outer
    (** [replace_all ~in_ ~with_ t] returns a slice with all matches in [in_],
        if any, replaced with [with_] in the result. *)
  end

  val is_prefix: prefix:'a t -> 'a t -> bool
  (** [is_prefix ~prefix t] returns true if [prefix] is a prefix of [t]. *)

  val is_suffix: suffix:'a t -> 'a t -> bool
  (** [is_suffix ~suffix t] returns true if [suffix] is a suffix of [t]. *)

  val prefix: usize -> 'a t -> 'a t
  (** [prefix i t] returns the prefix of [t] comprising [i] codepoints. *)

  val suffix: usize -> 'a t -> 'a t
  (** [suffix i t] returns the suffix of [t] comprising [i] codepoints. *)

  val chop_prefix: prefix:'a t -> 'a t -> 'a t option
  (** [chop_prefix ~prefix t] returns [t] absent [prefix], or [None] if [prefix]
      is not a valid prefix of [t]. *)

  val chop_prefix_hlt: prefix:'a t -> 'a t -> 'a t
  (** [chop_prefix_hlt ~prefix t] returns [t] absent [prefix], or halts if
      [prefix] is not a valid prefix of [t]. *)

  val chop_suffix: suffix:'a t -> 'a t -> 'a t option
  (** [chop_suffix ~suffix t] returns [t] absent [suffix], or [None] if [suffix]
      is not a valid suffix of [t]. *)

  val chop_suffix_hlt: suffix:'a t -> 'a t -> 'a t
  (** [chop_suffix_hlt ~suffix t] returns [t] absent [suffix], or halts if
      [suffix] is not a valid suffix of [t]. *)

  val lstrip: ?drop:('a -> bool) -> 'a t -> 'a t
  (** [lstrip ~drop t] returns [t] absent the prefix codepoints for which [drop]
      returns [true].  The default [drop] strips tab (['\t']), newline (['\n']),
      carriage return (['\r']), and space ([' ']). *)

  val rstrip: ?drop:('a -> bool) -> 'a t -> 'a t
  (** [rstrip ~drop t] returns [t] absent the suffix codepoints for which [drop]
      returns [true].  The default [drop] strips tab (['\t']), newline (['\n']),
      carriage return (['\r']), and space ([' ']). *)

  val strip: ?drop:('a -> bool) -> 'a t -> 'a t
  (** [strip ~drop t] returns [t] absent the prefix and suffix codepoints for
      which [drop] returns [true].  The default [drop] strips tab (['\t']),
      newline (['\n']), carriage return (['\r']), and space ([' ']). *)

  val split_fold_until: init:'accum -> on:('a -> bool)
    -> f:('accum -> 'a slice -> 'accum * bool) -> 'a t -> 'accum
  (** [split_fold_until ~init ~on ~f t] splits [t] on [on] into slices, which
      [f] folds in left to right order based on initial value [init], until [f]
      returns [accum, true], or until all slices have been folded. *)

  val split_fold_right_until: init:'accum -> on:('a -> bool)
    -> f:('a slice -> 'accum -> 'accum * bool) -> 'a t -> 'accum
  (** [split_fold_right_until ~init ~on ~f t] splits [t] on [on] into slices,
      which [f] folds in right to left order based on initial value [init],
      until [f] returns [accum, true], or until all slices have been folded. *)

  val split_fold: init:'accum -> on:('a -> bool)
    -> f:('accum -> 'a slice -> 'accum) -> 'a t -> 'accum
  (** [split_fold ~init ~on ~f t] splits [t] on [on] into slices, which
      [f] folds in left to right order based on initial value [init]. *)

  val split_fold_right: init:'accum -> on:(codepoint -> bool)
    -> f:('a slice -> 'accum -> 'accum) -> 'a t -> 'accum
  (** [split_fold_right ~init ~on ~f t] splits [t] on [on] into slices, which
      [f] folds in right to left order based on initial value [init]. *)

  val lines_fold: init:'accum
    -> f:('accum -> 'a slice -> 'accum) -> 'a t -> 'accum
  (** [lines_fold ~init ~f t] splits [t] into lines separated by ["\r\n"] or
      ["\n"], which [f] folds in left to right order based on initial value
      [init]. *)

  val lines_fold_right: init:'accum
    -> f:('a slice -> 'accum -> 'accum) -> 'a t -> 'accum
  (** [lines_fold_right ~init ~f t] splits [t] into lines separated by ["\r\n"]
      or ["\n"], which [f] folds in right to left order based on initial value
      [init]. *)

  val lsplit2: on:'a -> 'a t -> ('a t * 'a t) option
  (** [lsplit2 ~on t] splits [t] into two slices at the leftmost codepoint for
      which [on] returns [true], or returns [None] if [on] never returns [true].
  *)

  val lsplit2_hlt: on:'a -> 'a t -> 'a t * 'a t
  (** [lsplit2_hlt ~on t] splits [t] into two slices at the leftmost codepoint
      for which [on] returns [true], or halts if [on] never returns [true]. *)

  val rsplit2: on:'a -> 'a t -> ('a t * 'a t) option
  (** [rsplit2 ~on t] splits [t] into two slices at the rightmost codepoint for
      which [on] returns [true], or returns [None] if [on] never returns [true].
  *)

  val rsplit2_hlt: on:'a -> 'a t -> 'a t * 'a t
  (** [rsplit2_hlt ~on t] splits [t] into two slices at the rightmost codepoint
      for which [on] returns [true], or halts if [on] never returns [true]. *)

  (** Slice comparison operators. *)
  module O : sig
    type nonrec 'a t = 'a t

    include Cmpable_intf.S_poly with type 'a t := 'a t
  end
end
*)

module Seq : sig
  type 'a outer = 'a t
  module type S_mono = sig
    type t
    type elm
    val to_array: t -> elm outer
  end
  module type S_poly = sig
    type 'a t
    type 'a elm
    val to_array: 'a t -> 'a elm outer
  end
  module type S_poly2 = sig
    type ('a, 'cmp) t
    type 'a elm
    val to_array: ('a, 'cmp) t -> 'a elm outer
  end
  module type S_poly3 = sig
    type ('k, 'v, 'cmp) t
    type 'k key
    type 'v value
    val to_array: ('k, 'v, 'cmp) t -> ('k key * 'v value) outer
  end

  (** Efficiently convert a sequence of fixed element type with known length to
      an array. *)
  module Make_mono (T : Seq_intf.I_mono_def) : S_mono with type t := T.t
                                                       and type elm := T.elm

  (** Efficiently convert a reversed sequence of fixed element type with known
      length to an array. *)
  module Make_mono_rev (T : Seq_intf.I_mono_def) : S_mono with type t := T.t
                                                           and type elm := T.elm

  (** Efficiently convert a generic sequence with known length to an array. *)
  module Make_poly (T : Seq_intf.I_poly_def) : S_poly
    with type 'a t := 'a T.t
     and type 'a elm := 'a T.elm

  (** Efficiently convert a reversed generic sequence with known length to an
      array. *)
  module Make_poly_rev (T : Seq_intf.I_poly_def) : S_poly
    with type 'a t := 'a T.t
     and type 'a elm := 'a T.elm

  (** Efficiently convert a generic sequence with known length to an array. *)
  module Make_poly2 (T : Seq_intf.I_poly2_def) : S_poly2
    with type ('a, 'cmp) t := ('a, 'cmp) T.t
     and type 'a elm := 'a T.elm

  (** Efficiently convert a generic sequence with known length to an array. *)
  module Make_poly3 (T : Seq_intf.I_poly3_def) : S_poly3
    with type ('k, 'v, 'cmp) t := ('k, 'v, 'cmp) T.t
     and type 'k key := 'k T.key
     and type 'v value := 'v T.value
end

val init: usize -> f:(usize -> 'a) -> 'a t
(** Initialize array.  [init len ~f:(fun i -> ...)] initializes an array of
    given length, where [f] provides the value for each element at given index.
*)

val of_list: ?length:usize -> 'a list -> 'a t
(** Initialize array using contents of list.  If specified, [?length] must equal
    [(List.length list)]. *)

val of_list_rev: ?length:usize -> 'a list -> 'a t
(** Initialize array using reversed contents of list.  If specified, [?length]
    must equal [(List.length list)]. *)

val of_stream: ?length:usize -> 'a Stream.t -> 'a t
(** Initialize array using contents of stream.  If specified, [?length] must equal
    [(Stream.length stream)]. *)

val of_stream_rev: ?length:usize -> 'a Stream.t -> 'a t
(** Initialize array using reversed contents of stream.  If specified, [?length]
    must equal [(Stream.length stream)]. *)

val length: 'a t -> usize
(** Return array length. *)

val is_empty: 'a t -> bool
(** Return [true] if array length is 0; [false] otherwise. *)

val get: usize -> 'a t -> 'a
(** Get array element.  [get i t] returns the element at index [i]. *)

val set_inplace: usize -> 'a -> 'a t -> unit
(** Set array element in place (mutate).  [set_inplace i elm t] sets the element
    at index [i] to [elm]. *)

val set: usize -> 'a -> 'a t -> 'a t
(** Create a new array based on input array, differing in one element.
    [set i elm t] creates an array equal to [t], except that element [i] is
    initialized to [elm]. *)

val copy: 'a t -> 'a t
(** Create a copy of an array. *)

val pare: base:usize -> past:usize -> 'a t -> 'a t
(** Create an array with contents initialized to equal the \[[~base]..[~past])
    input subarray. *)

val join: ?sep:'a t -> 'a t list -> 'a t
(** Concatenate a list of arrays, with optional separator. *)

val concat: 'a t -> 'a t -> 'a t
(** Concatenate two arrays. *)

val append: 'a -> 'a t -> 'a t
(** Create an array that is the concatenation of the input array and the input
    element. *)

val prepend: 'a -> 'a t -> 'a t
(** Create an array that is the concatenation of the input element and the input
    array. *)

val insert: usize -> 'a -> 'a t -> 'a t
(** Create an array that is the concatenation of the bipartition of the input
    array at specified index, with the input element interposed. *)

val remove: usize -> 'a t -> 'a t
(** Create an array that is the concatenation of the first and third components
    of the tripartition about the element at specified index. *)

val reduce: f:('a -> 'a -> 'a) -> 'a t -> 'a option
(** Reduce the array to a single value, or return [None] if the array is empty.
    The reduction function is assumed to be associative; thus reduction order is
    unspecified. *)

val reduce_hlt: f:('a -> 'a -> 'a) -> 'a t -> 'a
(** Reduce the array to a single value, or halt if the array is empty.  The
    reduction function is assumed to be associative; thus reduction order is
    unspecified. *)

val swap_inplace: usize -> usize -> 'a t -> unit
(** Swap elements at given indices in place (mutate). *)

val swap: usize -> usize -> 'a t -> 'a t
(** Create an array based on the input array, but with elements at given indices
    swapped. *)

val rev_inplace: 'a t -> unit
(** Reverse array in place (mutate). *)

val rev: 'a t -> 'a t
(** Create an array with contents reversed relative to the input array. *)

val blit: usize -> usize -> 'a t -> usize -> 'a t -> unit
(** Set a range of elements in place (mutate).  [blit len i0 t0 i1 t1] sets the
    elements of [t1] at [\[i1..i1+len)] to equal the elements of [t0] at
    [\[i0..i0+len)].  Overlapping ranges are supported: [t0] and [t1] may refer
    to the same or different arrays; [i0] and [i1] may have arbitrary
    relationship. *)

val is_sorted: ?strict:bool -> cmp:('a -> 'a -> Cmp.t) -> 'a t -> bool
(** Return true if array is sorted (strictly if [?strict] is [true]) according
    to the comparison function. *)

val sort_inplace: ?stable:bool -> cmp:('a -> 'a -> Cmp.t) -> 'a t -> unit
(** Sort the array in place (mutate) according to the comparison function.
    Preserve order of equivalent elements if [?stable] is [true]. *)

val sort: ?stable:bool -> cmp:('a -> 'a -> Cmp.t) -> 'a t -> 'a t
(** Create an array with sorted contents of the input array according to the
    comparison function.  Preserve order of equivalent elements if [?stable] is
    [true]. *)

val psearch: ?base:usize -> ?past:usize -> 'a -> cmp:('a -> 'a -> Cmp.t) -> 'a t
  -> (Cmp.t * usize) option
(** Binary search for key in array, selecting the leftmost match, and falling
    back to the nearest present predecessor in the case of no match.
    @return {ul
      {- No predecessor: [Some (Cmp.Lt, 0)]}
      {- Leftmost match: [Some (Cmp.Eq, index)]}
      {- Predecessor: [Some (Cmp.Gt, index)]}
      {- Empty array: [None]}
    } *)

val search: ?base:usize -> ?past:usize -> 'a -> cmp:('a -> 'a -> Cmp.t) -> 'a t
  -> usize option
(** Binary search for key in array.  If key is found, return [(Some index)],
    otherwise return [None].  Note that if more than one element matches key, an
    arbitrary match is returned. *)

val nsearch: ?base:usize -> ?past:usize -> 'a -> cmp:('a -> 'a -> Cmp.t) -> 'a t
  -> (Cmp.t * usize) option
(** Binary search for key in array, selecting the rightmost match, and falling
    back to the nearest present successor in the case of no match.
    @return {ul
      {- Successor: [Some (Cmp.Lt, index)]}
      {- Rightmost match: [Some (Cmp.Eq, index)]}
      {- No successor: [Some (Cmp.Gt, (Usize.pred (length t)))]}
      {- Empty array: [None]}
    } *)

val map: f:('a -> 'b) -> 'a t -> 'b t
(** Create an array with elements mapped from the input array, according to the
    element mapping function. *)

val mapi: f:(usize -> 'a -> 'b) -> 'a t -> 'b t
(** Create an array with elements mapped from the input array, according to the
    indexed element mapping function. *)

val fold_map: init:'accum -> f:('accum -> 'a -> 'accum * 'b) -> 'a t
  -> 'accum * 'b t
(** Create an array and accumulated result with elements mapped from the input
    array, according to the folding mapping function. *)

val foldi_map: init:'accum -> f:(usize -> 'accum -> 'a -> 'accum * 'b) -> 'a t
  -> 'accum * 'b t
(** Create an array and accumulated result with elements mapped from the input
    array, according to the indexed folding mapping function. *)

val filter: f:('a -> bool) -> 'a t -> 'a t
(** Create an array with contents filtered by the given function.  Only elements
    for which the filter function returns [true] are incorporated into the
    result. *)

val filteri: f:(usize -> 'a -> bool) -> 'a t -> 'a t
(** Create an array with contents filtered by the given function.  Only elements
    for which the indexed filter function returns [true] are incorporated into
    the result. *)

val fold2_until: init:'accum -> f:('accum -> 'a -> 'b -> 'accum * bool) -> 'a t
  -> 'b t -> 'accum
(** Create an accumulated result for the paired elements of two arrays, calling
    the element folding function in increasing index order, and terminate
    folding early if the folding function returns true. *)

val foldi2_until: init:'accum
  -> f:(usize -> 'accum -> 'a -> 'b -> 'accum * bool) -> 'a t -> 'b t -> 'accum
(** Create an accumulated result for the paired elements of two arrays, calling
    the indexed element folding function in increasing index order, and
    terminate folding early if the folding function returns true. *)

val fold2: init:'accum -> f:('accum -> 'a -> 'b -> 'accum) -> 'a t -> 'b t
  -> 'accum
(** Create an accumulated result for the paired elements of two arrays, calling
    the element folding function in increasing index order. *)

val foldi2: init:'accum -> f:(usize -> 'accum -> 'a -> 'b -> 'accum) -> 'a t
  -> 'b t -> 'accum
(** Create an accumulated result for the paired elements of two arrays, calling
    the indexed element folding function in increasing index order. *)

val iter2: f:('a -> 'b -> unit) -> 'a t -> 'b t -> unit
(** Iterate over the paired elements of two arrays, calling the element visiting
    function in increasing index order. *)

val iteri2: f:(usize -> 'a -> 'b -> unit) -> 'a t -> 'b t -> unit
(** Iterate over the paired elements of two arrays, calling the indexed element
    visiting function in increasing index order. *)

val map2: f:('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
(** Create an array with elements mapped by the element mapping function from
    the paired elements of two input arrays. *)

val mapi2: f:(usize -> 'a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
(** Create an array with elements mapped by the indexed element mapping function
    from the paired elements of two input arrays. *)

val fold2_map: init:'accum -> f:('accum -> 'a -> 'b -> 'accum * 'c) -> 'a t
  -> 'b t -> 'accum * 'c t
(** Create an array and accumulated result based on the paired elements of two
    arrays, calling the element folding/mapping function in increasing index
    order. *)

val foldi2_map: init:'accum -> f:(usize -> 'accum -> 'a -> 'b -> 'accum * 'c)
  -> 'a t -> 'b t -> 'accum * 'c t
(** Create an array and accumulated result based on the paired elements of two
    arrays, calling the indexed element folding/mapping function in increasing
    index order. *)

val zip: 'a t -> 'b t -> ('a * 'b) t
(** Create an array with the paired elements of two arrays. *)

val unzip: ('a * 'b) t -> 'a t * 'b t
(** Create two arrays with the unpaired elements of the input pair array. *)
