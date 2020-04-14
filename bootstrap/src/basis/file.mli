open Rudiments

module Path = String

module Mode : sig
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
  | RW_A  (** create new append to existing file and open for read and write *)
  | RW_AO (** append to existing file and open for read and write, failing if
              file does not exist *)
  | RW_C  (** create new file and open for read and write, failing if file
              exists *)
  | RW_O  (** truncate existing file and open for read and write, failing if
              file does not exist *)
end

type t
(** mutable Unix file descriptor *)

type e

val of_path: ?mode:Mode.t -> Path.t -> (t, e) Result.t
(** [of_path mode path] opens file in [mode] at [path] and returns the resulting
    file or an error if file could not be opened. *)

val of_path_hlt: ?mode:Mode.t -> Path.t -> t
(** [of_path mode path] opens file in [mode] at [path] and returns the resulting
    file or halts if file could not be opened. *)

val read: ?n:usize -> t -> (Bytes.t, e) Result.t
(** [read n t] reads up to [n] bytes from [t] and returns the read bytes or an
    error if bytes could not be read. *)

val read_hlt: ?n:usize -> t -> Bytes.t
(** [read_hlt n t] reads up to [n] bytes from [t] and returns the read bytes or
    halts if bytes could not be read. *)

val write: Bytes.t -> t -> e option
(** [write bytes t] writes [bytes] to [t] and returns an error if bytes could
    not be written. *)

val write_hlt: Bytes.t -> t -> unit
(** [write_htl bytes t] writes [bytes] to [t] and halts if bytes could not be
    written. *)

val close: t -> e option
(** [close t] closes file [t] and returns an error if it could not be closed.
    *)

val close_hlt: t -> unit
(** [close_hlt t] closes file [t] and halts if it could not be closed. *)

val seek: isize -> t -> (usize, e) Result.t
(** [seek i t] mutates file descriptor in [t] to point to the [i]th byte
    relative to the current byte position of the file. Returns the new byte
    index relatve to the beginning of the file or an error if it could not be
    changed. *)

val seek_hlt: isize -> t -> usize
(** [seek_hlt i t] mutates file descriptor in [t] to point to the [i]th byte
    relative to the current byte position of the file. Returns the new byte
    index relatve to the beginning of the file or halts if it could not be
    changed. *)

val seek_left: usize -> t -> (usize, e) Result.t
(** [seek_left u t] mutates file descriptor in [t] to point to the [u]th byte
    after to the beginning of the file. Returns the new byte index relatve to
    the beginning of the file or an error if it could not be changed. *)

val seek_left_hlt: usize -> t -> usize
(** [seek_left_hlt u t] mutates file descriptor in [t] to point to the [u]th
    byte after to the beginning of the file. Returns the new byte index relatve
    to the beginning of the file or an error if it could not be changed. *)

val seek_right: usize -> t -> (usize, e) Result.t
(** [seek_right u t] mutates file descriptor in [t] to point to the [u]th byte
    before to the end of the file. Returns the new byte index relatve to the
    beginning of the file or an error if it could not be changed. *)

val seek_right_hlt: usize -> t -> usize
(** [seek_right_hlt u t] mutates file descriptor in [t] to point to the [u]th
    byte before to the end of the file. Returns the new byte index relatve
    to the beginning of the file or an error if it could not be changed. *)

module Stream : sig

  type outer = t
  type t = byte Stream.t

  val of_file: outer -> t
  (** [of_file file] takes an open [file] with read permissions and returns a
      byte stream of the contents. Reading from the returned byte stream mutates
      the read position of [file]. may cause the returned byte stream to  *)

  val of_path: Path.t -> (t, e) Result.t
  (** [of_path path] attempts to open the file at given [path] in read-only mode
      and returns a byte stream of the contents or an error if the file could
      not be opened. *)

  val of_path_hlt: Path.t -> t
  (** [of_path_hlt path] attempts to open the file at given [path] in read-only
    mode and returns a byte stream of the contents or halts if the file could
    not be opened. *)

  val write: outer -> t -> e option
  (** [write file byte_stream] takes an open [file] with write permissions and
    writes contents of [byte_stream] to it. Returns an error if not all bytes
    could be written. *)

  val write_hlt: outer -> t -> unit
  (** [write_hlt file byte_stream] takes an open [file] with write permissions and
    writes contents of [byte_stream] to it. Halts if not all bytes could be
    written. *)
end
