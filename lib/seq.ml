open La_base

include Functor.Make(struct
    type 'a elt = 'a
    type monoid = int
    let null = 0
    let measure _ = 1
    let add = Int.add
  end)

let length t = measure t

let bounds_check i t =
  if i < 0 || i >= length t then
    raise @@ Invalid_argument "index is out of bounds"
  else ()

let partition_unchecked i t = partition ~p:((<) i) t

let partition_exn i t =
  bounds_check i t;
  partition_unchecked i t
  
  
let partition_opt i t =
  match partition_exn i t with
  | exception _ -> None
  | value -> Some value

let split_unchecked i t = split ~p:((<) i) t

let split_exn i t =
  bounds_check i t;
  split_unchecked i t

let split i t =
  match split_exn i t with
  | exception _ -> None
  | v -> Some v

let get_unchecked i t = get ~p:((<) i) t

let get_exn i t =
  bounds_check i t;
  get_unchecked i t

let get i t =
  match get i t with
  | exception _ -> None
  | value -> Some value

let insert_unchecked i el t = insert ~p:((<) i) el t
  
let insert_exn i el t =
  bounds_check i t;
  insert ~p:((<) i) el t

let insert i el t =
  match insert_exn i el t with
  | exception _ -> None
  | value -> Some value

let update_unchecked i el t = update ~p:((<) i) el t
  
let update_exn i el t =
  bounds_check i t;
  update_unchecked i el t

let update i el t =
  match update_exn i el t with
  | exception _ -> None
  | value -> Some value

let pop_unchecked i t = pop ~p:((<) i) t

let pop_exn i t =
  bounds_check i t;
  pop_unchecked i t

let pop i t =
  match pop_exn i t with
  | exception _ -> None
  | value -> Some value

let remove_unchecked i t = remove ~p:((<) i) t

let remove_exn i t =
  bounds_check i t;
  remove_unchecked i t

let remove i t =
  match remove_exn i t with
  | exception _ -> None
  | value -> Some value

let merge ~cmp =
  let rec loop out t1 t2 =
    match lview_lazy t1, lview_lazy t2 with
    | None, None -> out
    | None, Some (h, t) | Some (h, t), None ->
      join_with out h !!t
    | Some (h1, t1'), Some(h2, t2') ->
      match cmp h1 h2 with
      | 0 -> loop (out <@ h1 <@ h2) !!t1' !!t2'
      | n when n < 0 -> loop (out <@ h1) !!t1' t2
      | _ -> loop (out <@ h2) t1 !!t2' in
  loop empty

let rec msort ~cmp t =
  match length t with
  | 0 | 1 -> t
  | n ->
    let left, right = split_unchecked (n / Ints.unsafe 2) t in
    merge ~cmp (msort ~cmp !!left) (msort ~cmp !!right)

let sorta_sort ~cmp t =
  match lview t with
  | None -> assert false
  | Some (p, t') ->
    let rec loop left right t =
      match lview t with
      | None -> left, p, right
      | Some (h, t') ->
        match cmp h p with
        | n when n <= 0 -> loop (left <@ h) right t'
        | _ -> loop left (right <@ h) t' in
    loop empty empty t'

let rec sort ~cmp t =
  match length t with
  | 0 | 1 -> t
  | _ ->
    let left, p, right = sorta_sort ~cmp t in
    join_with (sort ~cmp left) p (sort ~cmp right)

let init ~len ~f =
  let rec loop i t =
    if i >= len then t else
      loop (i+1) (t <@ f i) in
  loop 0 empty

let concat_map2 ~f t =
  let mapped = map ~f t in
  let rec balance t =
    match length t with
    | 0 -> empty
    | 1 -> hd_left_exn t
    | n ->
      let (lazy l, lazy r) = split_unchecked (n / pos_exn 2) t in
      balance l >< balance r in
  balance mapped
