open La_base

include Functor.Make (struct
  type 'a elt = 'a
  type monoid = int

  let null = 0
  let measure _ = 1
  let add = Int.add
end)

module T = struct
  type 'a t = 'a t0
end

let pp_debug = pp
let show_debug = show

let pp pp_el out t =
  let open Format in
  let l = to_list t in
  let pp_list out l =
    pp_print_list ~pp_sep:(fun out () -> fprintf out ";@ ") pp_el out l
  in
  fprintf out "Fseq<@[%a@]>" pp_list l

let show pp_el t = Format.asprintf "%a" (pp pp_el) t
let length t = measure t

let bounds_check i t =
  if i < 0 || i >= length t then
    raise @@ Invalid_argument "index is out of bounds"
  else ()

let partition_lazy_unchecked i t = partition_lazy ~p:(( < ) i) t

let partition_lazy_exn i t =
  bounds_check i t;
  partition_lazy_unchecked i t

let partition_lazy i t =
  match partition_lazy_exn i t with exception _ -> None | value -> Some value

let partition_unchecked i t = partition ~p:(( < ) i) t

let partition_exn i t =
  bounds_check i t;
  partition_unchecked i t

let partition i t =
  match partition_exn i t with exception _ -> None | value -> Some value

let split_lazy i t = split ~p:(( < ) i) t

let split i t =
  let (lazy l), (lazy r) = split_lazy i t in
  (l, r)

let take n t =
  let (lazy l), _ = split_lazy n t in
  l

let drop n t =
  let _, (lazy r) = split_lazy n t in
  r

let rotate i t =
  match pos (length t) with
  | None -> t
  | Some len ->
      let i' =
        match i < 0 with true -> int len + (i mod len) | false -> i mod len
      in
      let l, r = split i' t in
      r >< l

let slice ~start ~stop t =
  let (lazy rest), _ = split_lazy stop t in
  let _, (lazy slice) = split_lazy start rest in
  slice

let get_unchecked i t = get ~p:(( < ) i) t

let get_exn i t =
  bounds_check i t;
  get_unchecked i t

let get i t = match get_exn i t with exception _ -> None | value -> Some value
let insert_unchecked i el t = insert ~p:(( < ) i) el t

let insert_exn i el t =
  if i < 0 || i > length t then invalid_arg "index is out of bounds"
  else insert ~p:(( < ) i) el t

let insert i el t =
  match insert_exn i el t with exception _ -> None | value -> Some value

let set_unchecked i el t = set ~p:(( < ) i) el t

let set_exn i el t =
  bounds_check i t;
  set_unchecked i el t

let set i el t =
  match set_exn i el t with exception _ -> None | value -> Some value

let pop_unchecked i t = pop ~p:(( < ) i) t

let pop_exn i t =
  bounds_check i t;
  pop_unchecked i t

let pop i t = match pop_exn i t with exception _ -> None | value -> Some value
let remove_unchecked i t = remove ~p:(( < ) i) t

let remove_exn i t =
  bounds_check i t;
  remove_unchecked i t

let remove i t = match remove_exn i t with exception _ -> t | t' -> t'

let merge ~cmp =
  let rec loop out t1 t2 =
    match (lview_lazy t1, lview_lazy t2) with
    | None, None -> out
    | None, Some (h, t) | Some (h, t), None -> join_with out h !!t
    | Some (h1, t1'), Some (h2, t2') -> (
        match cmp h1 h2 with
        | 0 -> loop (out >@ h1 >@ h2) !!t1' !!t2'
        | n when n < 0 -> loop (out >@ h1) !!t1' t2
        | _ -> loop (out >@ h2) t1 !!t2')
  in
  loop empty

let rec msort ~cmp t =
  match length t with
  | 0 | 1 -> t
  | n ->
      let left, right = split (n / Ints.unsafe 2) t in
      merge ~cmp (msort ~cmp left) (msort ~cmp right)

let sorta_sort ~cmp t =
  match lview t with
  | None -> assert false
  | Some (p, t') ->
      let rec loop left right t =
        match lview t with
        | None -> (left, p, right)
        | Some (h, t') -> (
            match cmp h p with
            | n when n <= 0 -> loop (left >@ h) right t'
            | _ -> loop left (right >@ h) t')
      in
      loop empty empty t'

let rec sort ~cmp t =
  match length t with
  | 0 | 1 -> t
  | _ ->
      let left, p, right = sorta_sort ~cmp t in
      join_with (sort ~cmp left) p (sort ~cmp right)

let init ~len ~f =
  let rec loop i t = if i >= len then t else loop (i + 1) (t >@ f i) in
  loop 0 empty

let to_array t =
  match length t with
  | 0 -> [||]
  | len ->
      let v = hd_left_exn t in
      let a = Array.make len v in
      ignore
      @@ fold_left ~init:0 t ~f:(fun i el ->
             Array.unsafe_set a i el;
             succ i);
      a

module Monad = Monad.With_functor (struct
  type 'a t = 'a T.t

  let return = singleton
  let bind t f = concat_map ~f t
  let map = map
end)
