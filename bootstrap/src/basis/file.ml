open Rudiments

let buflen = 1024

module Error = struct
  type t = usize

  external to_string_get_length: t -> usize = 
    "hemlock_file_error_to_string_get_length"
  external to_string_inner: usize -> Bytes.t -> t -> unit =
    "hemlock_file_error_to_string_inner"
  
  let of_value value =
    Usize.of_isize Isize.(-value)

  let to_string t =
    let n = to_string_get_length t in
    let bytes = Array.init n ~f:(fun _ -> Byte.of_usize 0) in
    let () = to_string_inner n bytes t in
    Bytes.to_string_hlt bytes
end

module Path = Bytes

module Flag = struct
  (* Modifications to Flag
 .t must be reflected in file.c. *)
  type t =
  | R_O
  | W
  | W_A
  | W_AO
  | W_C
  | W_O
  | RW
  | RW_A
  | RW_AO
  | RW_C
  | RW_O
end

type t = usize

external of_path_inner: Flag.t -> usize -> usize -> Bytes.t -> isize =
  "hemlock_file_of_path_inner"

let of_path ?(flag=Flag.RW) ?(mode=0o660) (path : Path.t) =
  let value = of_path_inner flag mode (Array.length path) path in
  match Isize.(value < kv 0) with
  | true -> Error (Usize.of_isize Isize.(-value))
  | false -> Ok (Usize.of_isize value)

let of_path_hlt ?flag ?mode path =
  match of_path ?flag ?mode path with
  | Error error -> halt (Error.to_string error)
  | Ok t -> t

external of_stdin_inner: unit -> isize =
  "hemlock_file_of_stdin_inner"

let of_stdin () =
  let value = of_stdin_inner () in
  match Isize.(value < kv 0) with
  | true -> Error (Usize.of_isize Isize.(-value))
  | false -> Ok (Usize.of_isize value)

let of_stdin_hlt () =
  match of_stdin () with
  | Error error -> halt (Error.to_string error)
  | Ok t -> t

external of_stdout_inner: unit -> isize =
  "hemlock_file_of_stdout_inner"

let of_stdout () =
  let value = of_stdout_inner () in
  match Isize.(value < kv 0) with
  | true -> Error (Usize.of_isize Isize.(-value))
  | false -> Ok (Usize.of_isize value)

let of_stdout_hlt () =
  match of_stdout () with
  | Error error -> halt (Error.to_string error)
  | Ok t -> t

external of_stderr_inner: unit -> isize =
  "hemlock_file_of_stderr_inner"

let of_stderr () =
  let value = of_stderr_inner () in
  match Isize.(value < kv 0) with
  | true -> Error (Usize.of_isize Isize.(-value))
  | false -> Ok (Usize.of_isize value)

let of_stderr_hlt () =
  match of_stderr () with
  | Error error -> halt (Error.to_string error)
  | Ok t -> t

external close_inner: t -> isize =
  "hemlock_file_close_inner"

let close t =
  let value = close_inner t in
  match Isize.(value < kv 0) with
  | true -> Some (Error.of_value value)
  | false -> None

let close_hlt t =
  match close t with
  | Some error -> halt (Error.to_string error)
  | None -> ()

external read_inner: usize -> Bytes.t -> t -> isize =
  "hemlock_file_read_inner"
(** usize -> !Stlib.Bytes.t -> t -> isize  *)

let read ?(n=buflen) ?bytes t =
  let bytes = begin
    match bytes with
    | None -> Array.init n ~f:(fun _ -> Byte.of_usize 0)
    | Some bytes -> bytes
  end in
  let n = Usize.min n (Array.length bytes) in
  let value = read_inner n bytes t in
  let result = begin
    match Isize.(value < kv 0) with
    | true -> Error (Usize.of_isize Isize.(-value))
    | false -> begin
      let i = Usize.of_isize value in
      Ok (i, bytes)
    end
  end in
  result, t

let read_hlt ?n ?bytes t =
  match read ?n ?bytes t with
  | Error error, t -> begin
    let _ = close t in
    halt (Error.to_string error)
  end
  | Ok (i, bytes), t -> i, bytes, t

external write_inner: usize -> usize -> Bytes.t -> t -> isize =
  "hemlock_file_write_inner"

let write ?n bytes t =
  let rec fn i n bytes t = begin
    match i < n with
    | false -> None, n, t
    | true -> begin
      let value = write_inner i n bytes t in
      match Isize.(value < kv 0) with
      | true -> Some (Error.of_value value), i, t
      | false -> begin
        let i' = i + Usize.of_isize value in
        fn i' n bytes t
      end
    end
  end in
  let n = begin
    match n with
    | None -> Array.length bytes
    | Some n -> Usize.min n (Array.length bytes)
  end in
  fn 0 n bytes t

let write_hlt ?n bytes t =
  match write ?n bytes t with
  | Some error, _, t -> begin
    let _ = close t in
    halt (Error.to_string error)
  end
  | None, _, t -> t

let seek_base inner ?n t =
  let n = match n with | None -> isize_of_usize 0 | Some n -> n in
  let value = inner n t in
  let result = begin
    match Isize.(value < kv 0) with
    | true -> Error (Error.of_value value)
    | false -> Ok (Usize.of_isize value)
  end in
  result, t

let seek_hlt_base inner ?n t =
  match seek_base inner ?n t with
  | Error error, t -> begin
    let _ = close t in
    halt (Error.to_string error)
  end
  | Ok _, t' -> t'

external seek_inner: isize -> t -> isize =
  "hemlock_file_seek_inner"

let seek = seek_base seek_inner

let seek_hlt = seek_hlt_base seek_inner

external seek_hd_inner: isize -> t -> isize =
  "hemlock_file_seek_hd_inner"

let seek_hd = seek_base seek_hd_inner

let seek_hd_hlt = seek_hlt_base seek_hd_inner

external seek_tl_inner: isize -> t -> isize =
  "hemlock_file_seek_tl_inner"

let seek_tl = seek_base seek_tl_inner

let seek_tl_hlt = seek_hlt_base seek_tl_inner

module Stream = struct

  type outer = t
  type t = (usize * Bytes.t * (unit -> unit), Error.t) result Stream.t

  type stop = {
    mutable v: bool
  }

  let of_file ?buflen file =
    let rec exhaust t = begin
      match t with
      | lazy Stream.Nil -> ()
      | lazy (Stream.Cons(_, t')) -> exhaust t'
    end in
    let get_interrupt stop t () = begin
      stop.v <- true;
      exhaust t
    end in
    let rec fn stop n file = lazy begin
      match stop.v with
      | true -> begin
        let _ = close file in
        (* EBADFD *)
        Stream.Cons(Error 77, Stream.empty)
      end
      | false -> begin
        match read ?n file with
        | Error error, file' ->begin
          let _ = close file' in
          Stream.Cons(Error error, Stream.empty)
        end
        | Ok (0, _), file' -> begin
          let _ = close file' in
          Stream.Nil
        end
        | Ok (n, bytes'), file' -> begin
          let t = fn stop buflen file' in
          let interrupt = get_interrupt stop t in
          Stream.Cons(Ok (n, bytes', interrupt), t)
        end
      end
    end in
    (* This could be some global condition variable in Hemlock? *)
    let stop = {v=false} in
    fn stop buflen file

  let of_path ?buflen path =
    match of_path ~flag:Flag
   .R_O path with
    | Error error -> Error error
    | Ok file -> Ok (of_file ?buflen file)

  let of_path_hlt ?buflen path =
    match of_path ?buflen path with
    | Error error -> halt (Error.to_string error)
    | Ok(t) -> t
  
  let write (file: outer) (t: t) : (Error.t option * usize * outer) =
    let rec fn (i: usize) (file: outer) (t: t) = begin
      match t with
      | lazy Stream.Nil -> None, i, file
      | lazy (Stream.Cons(Error error, lazy Stream.Nil)) -> Some error, i, file
      | lazy (Stream.Cons(Error _, _)) -> not_reached ()
      | lazy (Stream.Cons(Ok (n, bytes, interrupt), t)) -> begin
        let error_opt, n, file = write ~n:n bytes file in
        let i = i + n in
        match error_opt with
        | Some _ -> begin
          let _ = interrupt () in
          error_opt, i, file
        end
        | None -> fn i file t
      end
    end in
    fn 0 file t

  let write_hlt file t =
    match write file t with
    | Some error, _, _  -> halt (Error.to_string error)
    | None, _, file -> file

end
