open Basis

let () =
  let bytes = Bytes.of_string "these are the file contents" in
  let n, bytes', _ = File.of_path_hlt (Bytes.of_string "foo")
  |> File.write_hlt bytes
  |> File.seek_hd_hlt
  |> File.read_hlt in
  File.write_hlt ~n bytes' (File.of_stdout_hlt ())
  |> File.close_hlt