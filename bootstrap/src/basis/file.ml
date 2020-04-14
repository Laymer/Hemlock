open Rudiments
open Result

type t = {
  mutable fd: Unix.file_descr option;
}
type e = string

let buflen = 1024

module Path = String

module Mode = struct
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

  let to_unix_flags t =
    let open Unix in
    match t with
    | R_O   -> [O_RDONLY]
    | W     -> [O_WRONLY; O_CREAT]
    | W_A   -> [O_WRONLY; O_APPEND; O_CREAT]
    | W_AO  -> [O_WRONLY; O_APPEND]
    | W_C   -> [O_WRONLY; O_CREAT; O_EXCL]
    | W_O   -> [O_WRONLY]
    | RW    -> [O_RDWR; O_CREAT]
    | RW_A  -> [O_RDWR; O_APPEND; O_CREAT]
    | RW_AO -> [O_RDWR; O_APPEND]
    | RW_C  -> [O_RDWR; O_CREAT; O_EXCL]
    | RW_O  -> [O_RDWR]
end

let of_path ?(mode=Mode.RW) path =
  let open Unix in
  let unix_flags = Mode.to_unix_flags mode in
  try
    Ok ({fd=Some (openfile path unix_flags 0o640)})
  with
  | Unix_error (errno, fn_name, fn_param) -> begin
    Error (String.concat ~sep:" -> " [Unix.error_message errno; fn_name; fn_param])
  end
  | _ -> not_reached ()

let of_path_hlt ?mode path =
  let t_res = of_path ?mode path in
  match t_res with
  | Error e -> halt e
  | Ok t -> t

let read ?(n=buflen) t =
  match t.fd with
  | None -> Error "File is closed"
  | Some fd -> begin
    let chars = Stdlib.Bytes.create n in
    let n' = Unix.read fd chars 0 n in
    Ok (Array.init n' ~f:(fun i -> Byte.of_char(Stdlib.Bytes.get chars i)))
  end

let read_hlt ?n t =
  match read ?n t with
  | Error e -> halt e
  | Ok bytes -> bytes

let write bytes t =
  match t.fd with
  | None -> Some "File is closed"
  | Some fd ->
    let rec fn i n chars fd = begin
      let n' = Unix.write fd chars i n in
      match n' = -1 with
      | true -> Some "Error reading file"
      | false -> begin
        match n' < n with
        | false -> None
        | true -> fn (i + n') (n - n') chars fd
      end
    end in
    let n = Array.length bytes in
    let f = fun i -> Stdlib.char_of_int (Byte.to_usize (Array.get (i) bytes)) in
    let chars = Stdlib.Bytes.init n f in
    fn 0 n chars fd

let write_hlt bytes t =
  match write bytes t with
  | None -> ()
  | Some e -> halt e

let close t =
  match t.fd with
  | None -> Some "File is closed"
  | Some fd -> begin
    Unix.close fd;
    t.fd <- None;
    None
  end

let close_hlt t =
  match close t with
  | None -> ()
  | Some e -> halt e

let _seek strategy i t =
  match t.fd with
  | None -> Error "File is closed"
  | Some fd -> begin
    let i = int_of_isize i in
    Ok(Usize.of_isize(isize_of_int (Unix.lseek fd i strategy)))
  end

let _seek_hlt strategy i t =
  match _seek strategy i t with
  | Error e -> halt e
  | Ok offset -> offset

let seek i t =
  _seek Unix.SEEK_CUR i t

let seek_hlt i t =
  _seek_hlt Unix.SEEK_CUR i t

let seek_left u t =
  let i = isize_of_usize u in
  _seek Unix.SEEK_SET i t

let seek_left_hlt u t =
  let i = isize_of_usize u in
  _seek_hlt Unix.SEEK_SET i t

let seek_right u t =
  let i = isize_of_usize u in
  _seek Unix.SEEK_END i t

let seek_right_hlt u t =
  let i = isize_of_usize u in
  _seek_hlt Unix.SEEK_END i t

module Stream = struct

  type outer = t
  type t = Byte.t Stream.t

  let of_file file =
    let rec fn i bytes file = lazy begin
      match  i < Array.length bytes with
      | false -> begin
        let bytes_res = read file in
        match bytes_res with
        | Error _
        | Ok [||] -> begin
          let _ = close file in
          Stream.Nil
        end
        | Ok bytes' -> Lazy.force (fn 0 bytes' file)
      end
      | true -> begin
        let elm = Array.get i bytes in
        let i' = succ i in
        let byte_stream = fn i' bytes file in
        Stream.Cons(elm, byte_stream)
      end
    end in
    fn 0 [||] file

  let of_path path =
    let file_res = of_path ~mode:Mode.R_O path in
    match file_res with
    | Error error -> Error error
    | Ok file -> Ok (of_file file)

  let of_path_hlt path =
    let t_res = of_path path in
    match t_res with
    | Error e -> halt e
    | Ok(t) -> t

  let write file t =
    let rec fn file t = begin
      match t with
      | lazy Stream.Nil -> None
      | lazy (Stream.Cons(_, _)) -> begin
        let t', t'' = Stream.split buflen t in
        let bytes = Array.of_stream t' in
        let error_opt = write bytes file in
        match error_opt with
        | None -> fn file t''
        | Some error -> Some error
      end
    end in
    fn file t

  let write_hlt file t =
    match write file t with
    | Some e -> halt e
    | None -> ()

end

let%expect_test "of_path_hlt" =
  let open Format in
  printf "@[<h>";
  let t = of_path_hlt ~mode:Mode.R_O "/home/cevans/test.txt" in
  let bytes = read_hlt t in
  let s = Bytes.to_string_hlt bytes in
  let _ = close_hlt t in
  printf "t = %a\n" String.pp s;
  printf "@]";

  [%expect{|
    t = "The quick brown fox jumped over the red fence.\n"
    |}]

let%expect_test "Stream.of_path_hlt" =
  let open Format in
  printf "@[<h>";
  Stream.of_path_hlt "/home/cevans/test.txt"
  |> Array.of_stream
  |> Bytes.to_string_hlt
  |> printf "%a\n" String.pp;
  printf "@]";

  [%expect{|
    "The quick brown fox jumped over the red fence.\n"
    |}]

let%expect_test "Stream.of_path_hlt" =
  let open Format in
  printf "@[<h>";
  let _ = Stream.of_path_hlt "/home/cevans/test.txt"
    |> Stream.write_hlt (of_path_hlt ~mode:Mode.W_A "/home/cevans/test2.txt")
   in
  printf "@]";

  [%expect{| |}]
