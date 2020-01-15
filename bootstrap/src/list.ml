open Rudiments

include List0

let reduce t ~f =
  let rec fn accum t = begin
    match t with
    | [] -> Some accum
    | elm :: t' -> fn (f accum elm) t'
  end in
  match t with
  | [] -> None
  | elm :: t' -> fn elm t'

let reduce_hlt t ~f =
  match reduce t ~f with
  | None -> halt "Empty list cannot be reduced"
  | Some result -> result

let is_sorted ?strict t ~cmp =
  let strict = match strict with
    | None -> false
    | Some b -> b
  in
  let rec fn elm t = begin
    match t with
    | [] -> true
    | elm' :: t' -> begin
        match (cmp elm elm'), strict with
        | Cmp.Lt, _
        | Cmp.Eq, false -> fn elm' t'
        | Cmp.Eq, true
        | Cmp.Gt, _ -> false
      end
  end in
  match t with
  | [] -> true
  | elm :: t' -> fn elm t'

let sort ?length ?stable t ~cmp =
  let arr = Array.of_list ?length t in
  Array.sort_inplace ?stable arr ~cmp;
  Array.to_list arr

let dedup ?length t ~cmp =
  let rec fn member t arr i = begin
    let elm = Array.get arr i in
    let member', t' = match cmp elm member with
      | Cmp.Lt -> elm, (member :: t)
      | Cmp.Eq -> elm, t
      | Cmp.Gt -> not_reached ()
    in
    if Uint.(i = zero) then member' :: t'
    else fn member' t' arr (Uint.pred i)
  end in
  match t with
  | []
  | _ :: [] -> t
  | _ :: _ -> begin
      let arr = Array.of_list ?length t in
      Array.sort_inplace ~stable:true arr ~cmp;
      let i = Uint.pred (Array.length arr) in
      fn (Array.get arr i) [] arr (Uint.pred i)
    end

let rev_dedup ?length t ~cmp =
  let rec fn member t arr i = begin
    let elm = Array.get arr i in
    let member', t' = match cmp elm member with
      | Cmp.Lt -> not_reached ()
      | Cmp.Eq -> member, t
      | Cmp.Gt -> elm, (member :: t)
    in
    let i' = Uint.succ i in
    if Uint.(i' = (Array.length arr)) then member' :: t'
    else fn member' t' arr i'
  end in
  match t with
  | []
  | _ :: [] -> t
  | _ :: _ -> begin
      let arr = Array.of_list ?length t in
      Array.sort_inplace ~stable:true arr ~cmp;
      let i = Uint.(kv 0) in
      fn (Array.get arr i) [] arr (Uint.succ i)
    end

let dedup_sorted t ~cmp =
  let rec fn member t = begin
    match t with
    | [] -> [member]
    | elm :: t' -> begin
        match cmp member elm with
        | Cmp.Lt -> member :: (fn elm t')
        | Cmp.Eq -> fn member t'
        | Cmp.Gt -> not_reached ()
      end
  end in
  match t with
  | []
  | _ :: [] -> t
  | elm :: t' -> fn elm t'

let rev_dedup_sorted t ~cmp =
  let rec fn member t accum = begin
    match t with
    | [] -> member :: accum
    | elm :: t' -> begin
        match cmp member elm with
        | Cmp.Lt -> fn elm t' (member :: accum)
        | Cmp.Eq -> fn member t' accum
        | Cmp.Gt -> not_reached ()
      end
  end in
  match t with
  | []
  | _ :: [] -> t
  | elm :: t' -> fn elm t' []

let merge t0 t1 ~cmp =
  let rec fn elm0 t0 elm1 t1 = begin
    match cmp elm0 elm1 with
    | Cmp.Lt
    | Cmp.Eq -> begin
        match t0 with
        | [] -> elm0 :: elm1 :: t1
        | elm0' :: t0' -> elm0 :: (fn elm0' t0' elm1 t1)
      end
    | Cmp.Gt -> begin
        match t1 with
        | [] -> elm1 :: elm0 :: t0
        | elm1' :: t1' -> elm1 :: (fn elm0 t0 elm1' t1')
      end
  end in
  match t0, t1 with
  | [], _ -> t1
  | _, [] -> t0
  | (elm0 :: t0'), (elm1 :: t1') -> fn elm0 t0' elm1 t1'

let rev_merge t0 t1 ~cmp =
  let rec fn elm0 t0 elm1 t1 accum = begin
    match cmp elm0 elm1 with
    | Cmp.Lt
    | Cmp.Eq -> begin
        let accum' = elm0 :: accum in
        match t0 with
        | [] -> rev_concat t1 (elm1 :: accum')
        | elm0' :: t0' -> fn elm0' t0' elm1 t1 accum'
      end
    | Cmp.Gt -> begin
        let accum' = elm1 :: accum in
        match t1 with
        | [] -> rev_concat t0 (elm0 :: accum')
        | elm1' :: t1' -> fn elm0 t0 elm1' t1' accum'
      end
  end in
  match t0, t1 with
  | [], _ -> t1
  | _, [] -> t0
  | (elm0 :: t0'), (elm1 :: t1') -> fn elm0 t0' elm1 t1' []

let sexp_of_t t =
  Sexplib.Std.sexp_of_list t

let t_of_sexp sexp =
  Sexplib.Std.list_of_sexp sexp

(*******************************************************************************
 * Begin tests.
 *)

let%expect_test "reduce[_hlt]" =
  let open Printf in
  let print_uint_list lst = begin
    printf "[";
    let rec fn lst = begin
      match lst with
      | [] -> ()
      | hd :: tl -> begin
          printf "; %u" hd;
          fn tl
        end
    end in
    let () = match lst with
      | [] -> ()
      | hd :: tl -> begin
          printf "%u" hd;
          fn tl
        end
    in
    printf "]"
  end in
  let lists = [
    [];
    [0];
    [0; 1];
    [0; 1; 2];
    [0; 1; 2; 3];
    [0; 1; 2; 3; 4]
  ] in
  iter lists ~f:(fun l ->
    printf "reduce ";
    print_uint_list l;
    match (reduce l ~f:(fun a b -> a + b)) with
    | None -> printf "-> None\n"
    | Some result -> printf " -> %u\n" result
  );

  [%expect{|
    reduce []-> None
    reduce [0] -> 0
    reduce [0; 1] -> 1
    reduce [0; 1; 2] -> 3
    reduce [0; 1; 2; 3] -> 6
    reduce [0; 1; 2; 3; 4] -> 10
  |}]

let%expect_test "is_sorted" =
  let open Printf in
  let print_uint_list lst = begin
    printf "[";
    let rec fn lst = begin
      match lst with
      | [] -> ()
      | hd :: tl -> begin
          printf "; %u" hd;
          fn tl
        end
    end in
    let () = match lst with
      | [] -> ()
      | hd :: tl -> begin
          printf "%u" hd;
          fn tl
        end
    in
    printf "]"
  end in
  let lists = [
    [];
    [0];
    [0; 1];
    [0; 1; 2];

    [0; 0];
    [0; 1; 1];
    [0; 1; 2; 2];

    [1; 0];
    [0; 2; 1]
  ] in
  iter lists ~f:(fun l ->
    printf "is_sorted               ";
    print_uint_list l;
    printf " -> %b\n" (is_sorted l ~cmp:I63.cmp);

    printf "is_sorted ~strict:false ";
    print_uint_list l;
    printf " -> %b\n" (is_sorted ~strict:false l ~cmp:I63.cmp);

    printf "is_sorted ~strict:true  ";
    print_uint_list l;
    printf " -> %b\n" (is_sorted ~strict:true l ~cmp:I63.cmp)
  );

  [%expect{|
    is_sorted               [] -> true
    is_sorted ~strict:false [] -> true
    is_sorted ~strict:true  [] -> true
    is_sorted               [0] -> true
    is_sorted ~strict:false [0] -> true
    is_sorted ~strict:true  [0] -> true
    is_sorted               [0; 1] -> true
    is_sorted ~strict:false [0; 1] -> true
    is_sorted ~strict:true  [0; 1] -> true
    is_sorted               [0; 1; 2] -> true
    is_sorted ~strict:false [0; 1; 2] -> true
    is_sorted ~strict:true  [0; 1; 2] -> true
    is_sorted               [0; 0] -> true
    is_sorted ~strict:false [0; 0] -> true
    is_sorted ~strict:true  [0; 0] -> false
    is_sorted               [0; 1; 1] -> true
    is_sorted ~strict:false [0; 1; 1] -> true
    is_sorted ~strict:true  [0; 1; 1] -> false
    is_sorted               [0; 1; 2; 2] -> true
    is_sorted ~strict:false [0; 1; 2; 2] -> true
    is_sorted ~strict:true  [0; 1; 2; 2] -> false
    is_sorted               [1; 0] -> false
    is_sorted ~strict:false [1; 0] -> false
    is_sorted ~strict:true  [1; 0] -> false
    is_sorted               [0; 2; 1] -> false
    is_sorted ~strict:false [0; 2; 1] -> false
    is_sorted ~strict:true  [0; 2; 1] -> false
  |}]

let%expect_test "[rev_]dedup" =
  let open Printf in
  let print_uint_pair_list lst = begin
    printf "[";
    let rec fn lst = begin
      match lst with
      | [] -> ()
      | (a, b) :: tl -> begin
          printf "; (%u, %u)" a b;
          fn tl
        end
    end in
    let () = match lst with
      | [] -> ()
      | (a, b) :: tl -> begin
          printf "(%u, %u)" a b;
          fn tl
        end
    in
    printf "]"
  end in
  let pair_lists = [
    [];
    [(0, 0)];
    [(0, 0); (1, 1)];

    [(0, 0); (0, 1); (1, 2)];
    [(0, 0); (1, 1); (0, 2)];
    [(1, 0); (0, 1); (0, 2)];
    [(0, 0); (1, 1); (1, 2); (2, 3)];
  ] in
  iter pair_lists ~f:(fun pl ->
    printf "[rev_]dedup ";
    print_uint_pair_list pl;
    printf " -> ";
    let cmp (a, _) (b, _) = I63.cmp a b in
    print_uint_pair_list (dedup pl ~cmp);
    printf " / ";
    print_uint_pair_list (rev_dedup pl ~cmp);
    printf "\n"
  );

  [%expect{|
    [rev_]dedup [] -> [] / []
    [rev_]dedup [(0, 0)] -> [(0, 0)] / [(0, 0)]
    [rev_]dedup [(0, 0); (1, 1)] -> [(0, 0); (1, 1)] / [(1, 1); (0, 0)]
    [rev_]dedup [(0, 0); (0, 1); (1, 2)] -> [(0, 0); (1, 2)] / [(1, 2); (0, 0)]
    [rev_]dedup [(0, 0); (1, 1); (0, 2)] -> [(0, 0); (1, 1)] / [(1, 1); (0, 0)]
    [rev_]dedup [(1, 0); (0, 1); (0, 2)] -> [(0, 1); (1, 0)] / [(1, 0); (0, 1)]
    [rev_]dedup [(0, 0); (1, 1); (1, 2); (2, 3)] -> [(0, 0); (1, 1); (2, 3)] / [(2, 3); (1, 1); (0, 0)]
  |}]

let%expect_test "[rev_]dedup_sorted" =
  let open Printf in
  let print_uint_pair_list lst = begin
    printf "[";
    let rec fn lst = begin
      match lst with
      | [] -> ()
      | (a, b) :: tl -> begin
          printf "; (%u, %u)" a b;
          fn tl
        end
    end in
    let () = match lst with
      | [] -> ()
      | (a, b) :: tl -> begin
          printf "(%u, %u)" a b;
          fn tl
        end
    in
    printf "]"
  end in
  let pair_lists = [
    [];
    [(0, 0)];
    [(0, 0); (1, 1)];
    [(0, 0); (1, 1); (2, 2)];

    [(0, 0); (0, 1); (0, 2)];

    [(0, 0); (0, 1); (1, 2); (1, 3); (2, 4); (2, 5)];
  ] in
  iter pair_lists ~f:(fun pl ->
    printf "[rev_]dedup_sorted ";
    print_uint_pair_list pl;
    printf " -> ";
    let cmp (a, _) (b, _) = I63.cmp a b in
    assert (is_sorted pl ~cmp);
    print_uint_pair_list (dedup_sorted pl ~cmp);
    printf " / ";
    print_uint_pair_list (rev_dedup_sorted pl ~cmp);
    printf "\n"
  );

  [%expect{|
    [rev_]dedup_sorted [] -> [] / []
    [rev_]dedup_sorted [(0, 0)] -> [(0, 0)] / [(0, 0)]
    [rev_]dedup_sorted [(0, 0); (1, 1)] -> [(0, 0); (1, 1)] / [(1, 1); (0, 0)]
    [rev_]dedup_sorted [(0, 0); (1, 1); (2, 2)] -> [(0, 0); (1, 1); (2, 2)] / [(2, 2); (1, 1); (0, 0)]
    [rev_]dedup_sorted [(0, 0); (0, 1); (0, 2)] -> [(0, 0)] / [(0, 0)]
    [rev_]dedup_sorted [(0, 0); (0, 1); (1, 2); (1, 3); (2, 4); (2, 5)] -> [(0, 0); (1, 2); (2, 4)] / [(2, 4); (1, 2); (0, 0)]
  |}]

let%expect_test "[rev_]merge" =
  let open Printf in
  let print_uint_pair_list lst = begin
    printf "[";
    let rec fn lst = begin
      match lst with
      | [] -> ()
      | (a, b) :: tl -> begin
          printf "; (%u, %u)" a b;
          fn tl
        end
    end in
    let () = match lst with
      | [] -> ()
      | (a, b) :: tl -> begin
          printf "(%u, %u)" a b;
          fn tl
        end
    in
    printf "]"
  end in
  let pair_list_pairs = [
    ([], []);
    ([(0, 0)], []);
    ([], [(0, 0)]);

    ([(0, 0)], [(0, 1)]);
    ([(0, 0)], [(1, 1)]);
    ([(1, 0)], [(1, 1)]);

    ([(0, 0); (1, 1)], [(0, 2); (1, 3)]);
    ([(0, 0); (1, 1); (2, 2); (3, 3)], [(1, 4)]);
    ([(0, 0)], [(0, 1); (1, 2); (2, 3)]);
  ] in
  iter pair_list_pairs ~f:(fun (a, b) ->
    printf "[rev_]merge ";
    print_uint_pair_list a;
    printf " ";
    print_uint_pair_list b;
    printf " -> ";
    let cmp (a, _) (b, _) = I63.cmp a b in
    assert (is_sorted a ~cmp);
    assert (is_sorted b ~cmp);
    print_uint_pair_list (merge a b ~cmp);
    printf " / ";
    print_uint_pair_list (rev_merge a b ~cmp);
    printf "\n"
  );

  [%expect{|
    [rev_]merge [] [] -> [] / []
    [rev_]merge [(0, 0)] [] -> [(0, 0)] / [(0, 0)]
    [rev_]merge [] [(0, 0)] -> [(0, 0)] / [(0, 0)]
    [rev_]merge [(0, 0)] [(0, 1)] -> [(0, 0); (0, 1)] / [(0, 1); (0, 0)]
    [rev_]merge [(0, 0)] [(1, 1)] -> [(0, 0); (1, 1)] / [(1, 1); (0, 0)]
    [rev_]merge [(1, 0)] [(1, 1)] -> [(1, 0); (1, 1)] / [(1, 1); (1, 0)]
    [rev_]merge [(0, 0); (1, 1)] [(0, 2); (1, 3)] -> [(0, 0); (0, 2); (1, 1); (1, 3)] / [(1, 3); (1, 1); (0, 2); (0, 0)]
    [rev_]merge [(0, 0); (1, 1); (2, 2); (3, 3)] [(1, 4)] -> [(0, 0); (1, 1); (1, 4); (2, 2); (3, 3)] / [(3, 3); (2, 2); (1, 4); (1, 1); (0, 0)]
    [rev_]merge [(0, 0)] [(0, 1); (1, 2); (2, 3)] -> [(0, 0); (0, 1); (1, 2); (2, 3)] / [(2, 3); (1, 2); (0, 1); (0, 0)]
  |}]