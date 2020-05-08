open Rudiments

module Path = Bytes

module Flag: sig
  type t =
  | R_O   (** open existing file for read, failing if it does not exist *)
  | W     (** create new or truncate existing file and open for write *)
  | W_A   (** create new or append to existing file and open for write *)
  | W_AO  (** append to existing file and open for write, failing if it does not
              exist *)
  | W_C   (** create new file and open for write, failing if file exists *)
  | W_O   (** truncate existing file and open for write, failing if file does
              not exist *)
  | RW    (** create new or truncate existing file and open for read and write *)
  | RW_A  (** create new or append to existing file and open for read and write
              *)
  | RW_AO (** append to existing file and open for read and write, failing if
              file does not exist *)
  | RW_C  (** create new file and open for read and write, failing if file
              exists *)
  | RW_O  (** truncate existing file and open for read and write, failing if
              file does not exist *)
end

type t
(** mutable Unix file descriptor *)

module Error : sig
  type t

  val to_string: t -> String.t
end

val of_path: ?flag:Flag.t -> ?mode:usize -> Path.t -> (t, Error.t) result
(** [of_path mode path] opens file in [mode] at [path] and returns the resulting
    file or an error if file could not be opened. *)

val of_path_hlt: ?flag:Flag.t -> ?mode:usize -> Path.t -> t
(** [of_path mode path] opens file in [mode] at [path] and returns the resulting
    file or halts if file could not be opened. *)

val of_stdin: unit -> (t, Error.t) result
val of_stdin_hlt: unit -> t
val of_stdout: unit -> (t, Error.t) result
val of_stdout_hlt: unit -> t
val of_stderr: unit -> (t, Error.t) result
val of_stderr_hlt: unit -> t

val read: ?n:usize -> ?bytes: Bytes.t -> t -> (usize * Bytes.t, Error.t) result * t
(** [read n t] reads up to [n] bytes from [t] and returns the read bytes or an
    error if bytes could not be read. *)

val read_hlt: ?n:usize -> ?bytes: Bytes.t -> t -> usize * Bytes.t * t
(** [read_hlt n t] reads up to [n] bytes from [t] and returns the read bytes or
    halts if bytes could not be read. *)

(*
val write_foo: hd:Byte.t Array.Cursor.t -> tl:Byte.t Array.Cursor.t -> t ->
Error.t option
*)
(* TODO take a Bytes.t Slice.t instead of usize and Bytes.t *)
val write: ?n:usize -> Bytes.t -> t -> Error.t Option.t * usize * t
(** [write bytes t] writes [bytes] to [t] and returns an error if bytes could
    not be written. *)

val write_hlt: ?n:usize -> Bytes.t -> t -> t
(** [write_htl bytes t] writes [bytes] to [t] and halts if bytes could not be
    written. *)

val close: t -> Error.t Option.t
(** [close t] closes file [t] and returns an error if it could not be closed.
    *)
val close_hlt: t -> unit
(** [close_hlt t] closes file [t] and halts if it could not be closed. *)

val seek: ?n:isize -> t -> (usize, Error.t) result * t
(** [seek n t] mutates file descriptor in [t] to point to the [i]th byte
    relative to the current byte position of the file. Returns the new byte
    index relatve to the beginning of the file or an error if it could not be
    changed. *)

val seek_hlt: ?n:isize -> t -> t
(** [seek_hlt n t] mutates file descriptor in [t] to point to the [i]th byte
    relative to the current byte position of the file. Returns the new byte
    index relatve to the beginning of the file or halts if it could not be
    changed. *)

val seek_hd: ?n:isize -> t -> (usize, Error.t) result * t
(** [seek_left n t] mutates file descriptor in [t] to point to the [u]th byte
    after to the beginning of the file. Returns the new byte index relatve to
    the beginning of the file or an error if it could not be changed. *)

val seek_hd_hlt: ?n:isize -> t -> t
(** [seek_left_hlt n t] mutates file descriptor in [t] to point to the [u]th
    byte after to the beginning of the file. Returns the new byte index relatve
    to the beginning of the file or an error if it could not be changed. *)

val seek_tl: ?n:isize -> t -> (usize, Error.t) result * t
(** [seek_right n t] mutates file descriptor in [t] to point to the [u]th byte
    before to the end of the file. Returns the new byte index relatve to the
    beginning of the file or an error if it could not be changed. *)

val seek_tl_hlt: ?n:isize -> t -> t
(** [seek_right_hlt n t] mutates file descriptor in [t] to point to the [u]th
    byte before to the end of the file. Returns the new byte index relatve
    to the beginning of the file or an error if it could not be changed. *)

module Stream : sig

  type outer = t
  type t = (usize * Bytes.t * (unit -> unit), Error.t) result Stream.t

  val of_file: ?buflen:usize -> outer -> t
  (** [of_file file] takes an open [file] with read permissions and returns a
      byte stream of the contents. Reading from the returned byte stream mutates
      the read position of [file]. may cause the returned byte stream to  *)

  val of_path: ?buflen:usize -> Path.t -> (t, Error.t) result
  (** [of_path path] attempts to open the file at given [path] in read-only mode
      and returns a byte stream of the contents or an error if the file could
      not be opened. *)

  val of_path_hlt: ?buflen:usize -> Path.t -> t
  (** [of_path_hlt path] attempts to open the file at given [path] in read-only
    mode and returns a byte stream of the contents or halts if the file could
    not be opened. *)

  val write: outer -> t -> Error.t option * usize * outer
  (** [write file byte_stream] takes an open [file] with write permissions and
    writes contents of [byte_stream] to it. Returns an error if not all bytes
    could be written. *)

  val write_hlt: outer -> t -> outer
  (** [write_hlt file byte_stream] takes an open [file] with write permissions and
    writes contents of [byte_stream] to it. Halts if not all bytes could be
    written. *)
end