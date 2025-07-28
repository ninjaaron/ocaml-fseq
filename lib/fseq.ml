let ( !! ) = Lazy.force
let one _ = 1

module Node = struct
  type 'a t = N2 of int * 'a * 'a | N3 of int * 'a * 'a * 'a

  let pp pp_el out t =
    let open Format in
    match t with
    | N2 (_, a, b) -> fprintf out "(@[%a,@ %a@])" pp_el a pp_el b
    | N3 (_, a, b, c) ->
        fprintf out "(@[%a,@ %a,@ %a@])" pp_el a pp_el b pp_el c

  let measure = function N2 (v, _, _) -> v | N3 (v, _, _, _) -> v
  let mk2 ms a b = N2 (ms a + ms b, a, b)
  let mk3 ms a b c = N3 (ms a + ms b + ms c, a, b, c)

  let get ms i t =
    match t with
    | N2 (_, a, b) -> if i < ms a then a else b
    | N3 (_, a, b, c) ->
        let ms_a = ms a in
        if i < ms_a then a else if i < ms_a + ms b then b else c

  let fold_right f t acc =
    match t with
    | N2 (_, a, b) -> f a (f b acc)
    | N3 (_, a, b, c) -> f a (f b (f c acc))

  let fold_left f acc = function
    | N2 (_, a, b) -> f (f acc a) b
    | N3 (_, a, b, c) -> f (f (f acc a) b) c

  let map ms f = function
    | N2 (_, a, b) -> mk2 ms (f a) (f b)
    | N3 (_, a, b, c) -> mk3 ms (f a) (f b) (f c)

  let to_seq t () =
    let open Seq in
    match t with
    | N2 (_, a, b) -> Cons (a, cons b empty)
    | N3 (_, a, b, c) -> Cons (a, cons b (cons c empty))

  let rev_to_seq t () =
    let open Seq in
    match t with
    | N2 (_, a, b) -> Cons (b, cons a empty)
    | N3 (_, a, b, c) -> Cons (c, cons b (cons a empty))
end

module Digit = struct
  type 'a t =
    | One of 'a
    | Two of 'a * 'a
    | Three of 'a * 'a * 'a
    | Four of 'a * 'a * 'a * 'a

  let pp pp_el out t =
    let open Format in
    match t with
    | One a -> fprintf out "[@[%a@]]" pp_el a
    | Two (a, b) -> fprintf out "[@[%a,@ %a@]]" pp_el a pp_el b
    | Three (a, b, c) ->
        fprintf out "[@[%a,@ %a,@ %a@]]" pp_el a pp_el b pp_el c
    | Four (a, b, c, d) ->
        fprintf out "[@[%a,@ %a,@ %a,@ %a@]]" pp_el a pp_el b pp_el c pp_el d

  let measure ms = function
    | One a -> ms a
    | Two (a, b) -> ms a + ms b
    | Three (a, b, c) -> ms a + ms b + ms c
    | Four (a, b, c, d) -> ms a + ms b + ms c + ms d

  let of_node = function
    | Node.N2 (_, a, b) -> Two (a, b)
    | Node.N3 (_, a, b, c) -> Three (a, b, c)

  let joinl ms t1 t2 =
    match (t1, t2) with
    | One a, One b -> (Two (a, b), None)
    | One a, Two (b, c) | Two (a, b), One c -> (Three (a, b, c), None)
    | One a, Three (b, c, d) | Three (a, b, c), One d | Two (a, b), Two (c, d)
      ->
        (Four (a, b, c, d), None)
    | One a, Four (b, c, d, e)
    | Four (a, b, c, d), One e
    | Two (a, b), Three (c, d, e)
    | Three (a, b, c), Two (d, e) ->
        (Two (a, b), Some (One (Node.mk3 ms c d e)))
    | Two (a, b), Four (c, d, e, f)
    | Four (a, b, c, d), Two (e, f)
    | Three (a, b, c), Three (d, e, f) ->
        (Three (a, b, c), Some (One (Node.mk3 ms d e f)))
    | Three (a, b, c), Four (d, e, f, g) | Four (a, b, c, d), Three (e, f, g) ->
        (One a, Some (Two (Node.mk3 ms b c d, Node.mk3 ms e f g)))
    | Four (a, b, c, d), Four (e, f, g, h) ->
        (Two (a, b), Some (Two (Node.mk3 ms c d e, Node.mk3 ms f g h)))

  let joinr ms t1 t2 =
    match (t1, t2) with
    | One a, Four (b, c, d, e)
    | Four (a, b, c, d), One e
    | Two (a, b), Three (c, d, e)
    | Three (a, b, c), Two (d, e) ->
        (Some (One (Node.mk3 ms a b c)), Two (d, e))
    | Two (a, b), Four (c, d, e, f)
    | Four (a, b, c, d), Two (e, f)
    | Three (a, b, c), Three (d, e, f) ->
        (Some (One (Node.mk3 ms a b c)), Three (d, e, f))
    | Three (a, b, c), Four (d, e, f, g) | Four (a, b, c, d), Three (e, f, g) ->
        (Some (Two (Node.mk3 ms a b c, Node.mk3 ms d e f)), One g)
    | Four (a, b, c, d), Four (e, f, g, h) ->
        (Some (Two (Node.mk3 ms a b c, Node.mk3 ms d e f)), Two (g, h))
    | _, _ -> (
        let d, nodes = joinl ms t1 t2 in
        match nodes with Some _ -> assert false | None -> (None, d))

  let join2 ms t1 t2 =
    match (t1, t2) with
    (* 2 *)
    | One a, One b -> One (Node.mk2 ms a b)
    (* 3 *)
    | One a, Two (b, c) | Two (a, b), One c -> One (Node.mk3 ms a b c)
    (* 4 *)
    | One a, Three (b, c, d) | Two (a, b), Two (c, d) | Three (a, b, c), One d
      ->
        Two (Node.mk2 ms a b, Node.mk2 ms c d)
    (* 5 *)
    | One a, Four (b, c, d, e)
    | Two (a, b), Three (c, d, e)
    | Three (a, b, c), Two (d, e)
    | Four (a, b, c, d), One e ->
        Node.(Two (mk3 ms a b c, mk2 ms d e))
    (* 6 *)
    | Two (a, b), Four (c, d, e, f)
    | Three (a, b, c), Three (d, e, f)
    | Four (a, b, c, d), Two (e, f) ->
        Node.(Two (mk3 ms a b c, mk3 ms d e f))
    (* 7 *)
    | Three (a, b, c), Four (d, e, f, g) | Four (a, b, c, d), Three (e, f, g) ->
        Node.(Three (mk3 ms a b c, mk2 ms d e, mk2 ms f g))
    (* 8 *)
    | Four (a, b, c, d), Four (e, f, g, h) ->
        Node.(Three (mk3 ms a b c, mk3 ms d e f, mk2 ms g h))

  let join3 ms t1 t2 t3 =
    match (t1, t2, t3) with
    (* 3 *)
    | One a, One b, One c -> One (Node.mk3 ms a b c)
    (* 4 *)
    | One a, One b, Two (c, d)
    | One a, Two (b, c), One d
    | Two (a, b), One c, One d ->
        Two (Node.mk2 ms a b, Node.mk2 ms c d)
    (* 5 *)
    | One a, One b, Three (c, d, e)
    | One a, Two (b, c), Two (d, e)
    | One a, Three (b, c, d), One e
    | Two (a, b), One c, Two (d, e)
    | Two (a, b), Two (c, d), One e
    | Three (a, b, c), One d, One e ->
        Node.(Two (mk3 ms a b c, mk2 ms d e))
    (* 6 *)
    | One a, One b, Four (c, d, e, f)
    | One a, Two (b, c), Three (d, e, f)
    | One a, Three (b, c, d), Two (e, f)
    | One a, Four (b, c, d, e), One f
    | Two (a, b), One c, Three (d, e, f)
    | Two (a, b), Two (c, d), Two (e, f)
    | Two (a, b), Three (c, d, e), One f
    | Three (a, b, c), One d, Two (e, f)
    | Three (a, b, c), Two (d, e), One f
    | Four (a, b, c, d), One e, One f ->
        Node.(Two (mk3 ms a b c, mk3 ms d e f))
    (* 7 *)
    | One a, Two (b, c), Four (d, e, f, g)
    | One a, Three (b, c, d), Three (e, f, g)
    | One a, Four (b, c, d, e), Two (f, g)
    | Two (a, b), One c, Four (d, e, f, g)
    | Two (a, b), Two (c, d), Three (e, f, g)
    | Two (a, b), Three (c, d, e), Two (f, g)
    | Two (a, b), Four (c, d, e, f), One g
    | Three (a, b, c), One d, Three (e, f, g)
    | Three (a, b, c), Two (d, e), Two (f, g)
    | Three (a, b, c), Three (d, e, f), One g
    | Four (a, b, c, d), One e, Two (f, g)
    | Four (a, b, c, d), Two (e, f), One g ->
        Node.(Three (mk3 ms a b c, mk2 ms d e, mk2 ms f g))
    (* 8 *)
    | One a, Three (b, c, d), Four (e, f, g, h)
    | One a, Four (b, c, d, e), Three (f, g, h)
    | Two (a, b), Two (c, d), Four (e, f, g, h)
    | Two (a, b), Three (c, d, e), Three (f, g, h)
    | Two (a, b), Four (c, d, e, f), Two (g, h)
    | Three (a, b, c), One d, Four (e, f, g, h)
    | Three (a, b, c), Two (d, e), Three (f, g, h)
    | Three (a, b, c), Three (d, e, f), Two (g, h)
    | Three (a, b, c), Four (d, e, f, g), One h
    | Four (a, b, c, d), One e, Three (f, g, h)
    | Four (a, b, c, d), Two (e, f), Two (g, h)
    | Four (a, b, c, d), Three (e, f, g), One h ->
        Node.(Three (mk3 ms a b c, mk3 ms d e f, mk2 ms g h))
    (* 9 *)
    | One a, Four (b, c, d, e), Four (f, g, h, i)
    | Two (a, b), Three (c, d, e), Four (f, g, h, i)
    | Two (a, b), Four (c, d, e, f), Three (g, h, i)
    | Three (a, b, c), Two (d, e), Four (f, g, h, i)
    | Three (a, b, c), Three (d, e, f), Three (g, h, i)
    | Three (a, b, c), Four (d, e, f, g), Two (h, i)
    | Four (a, b, c, d), One e, Four (f, g, h, i)
    | Four (a, b, c, d), Two (e, f), Three (g, h, i)
    | Four (a, b, c, d), Three (e, f, g), Two (h, i)
    | Four (a, b, c, d), Four (e, f, g, h), One i ->
        Node.(Three (mk3 ms a b c, mk3 ms d e f, mk3 ms g h i))
    (* 10 *)
    | Two (a, b), Four (c, d, e, f), Four (g, h, i, j)
    | Three (a, b, c), Three (d, e, f), Four (g, h, i, j)
    | Three (a, b, c), Four (d, e, f, g), Three (h, i, j)
    | Four (a, b, c, d), Two (e, f), Four (g, h, i, j)
    | Four (a, b, c, d), Three (e, f, g), Three (h, i, j)
    | Four (a, b, c, d), Four (e, f, g, h), Two (i, j) ->
        Node.(Four (mk3 ms a b c, mk3 ms d e f, mk2 ms g h, mk2 ms i j))
    (* 11 *)
    | Three (a, b, c), Four (d, e, f, g), Four (h, i, j, k)
    | Four (a, b, c, d), Three (e, f, g), Four (h, i, j, k)
    | Four (a, b, c, d), Four (e, f, g, h), Three (i, j, k) ->
        Node.(Four (mk3 ms a b c, mk3 ms d e f, mk3 ms g h i, mk2 ms j k))
    (* 12 *)
    | Four (a, b, c, d), Four (e, f, g, h), Four (i, j, k, l) ->
        Node.(Four (mk3 ms a b c, mk3 ms d e f, mk3 ms g h i, mk3 ms j k l))

  let fold_left f acc = function
    | One a -> f acc a
    | Two (a, b) -> f (f acc a) b
    | Three (a, b, c) -> f (f (f acc a) b) c
    | Four (a, b, c, d) -> f (f (f (f acc a) b) c) d

  let map f = function
    | One a -> One (f a)
    | Two (a, b) -> Two (f a, f b)
    | Three (a, b, c) -> Three (f a, f b, f c)
    | Four (a, b, c, d) -> Four (f a, f b, f c, f d)

  let fold_right f t acc =
    match t with
    | One a -> f a acc
    | Two (a, b) -> f a (f b acc)
    | Three (a, b, c) -> f a (f b (f c acc))
    | Four (a, b, c, d) -> f a (f b (f c (f d acc)))

  let ladd a = function
    | One b -> Two (a, b)
    | Two (b, c) -> Three (a, b, c)
    | Three (b, c, d) -> Four (a, b, c, d)
    | _ -> invalid_arg "cannot ladd Four digit"

  let view_l = function
    | One a -> (a, None)
    | Two (a, b) -> (a, Some (One b))
    | Three (a, b, c) -> (a, Some (Two (b, c)))
    | Four (a, b, c, d) -> (a, Some (Three (b, c, d)))

  let hd = function
    | One a -> a
    | Two (a, _) -> a
    | Three (a, _, _) -> a
    | Four (a, _, _, _) -> a

  let radd t z =
    match t with
    | One y -> Two (y, z)
    | Two (x, y) -> Three (x, y, z)
    | Three (w, x, y) -> Four (w, x, y, z)
    | _ -> invalid_arg "cannot radd Four digit"

  let view_r = function
    | One a -> (a, None)
    | Two (a, b) -> (b, Some (One a))
    | Three (a, b, c) -> (c, Some (Two (a, b)))
    | Four (a, b, c, d) -> (d, Some (Three (a, b, c)))

  let hd_r = function
    | One a -> a
    | Two (_, b) -> b
    | Three (_, _, c) -> c
    | Four (_, _, _, d) -> d

  let split ms i t =
    match t with
    | One a -> (None, a, None)
    | Two (a, b) ->
        if i < ms a then (None, a, Some (One b)) else (Some (One a), b, None)
    | Three (a, b, c) ->
        let ms_a = ms a in
        if i < ms_a then (None, a, Some (Two (b, c)))
        else if i < ms_a + ms b then (Some (One a), b, Some (One c))
        else (Some (Two (a, b)), c, None)
    | Four (a, b, c, d) ->
        let ms_a = ms a in
        if i < ms_a then (None, a, Some (Three (b, c, d)))
        else
          let ms_b = ms_a + ms b in
          if i < ms_b then (Some (One a), b, Some (Two (c, d)))
          else if i < ms_b + ms c then (Some (Two (a, b)), c, Some (One d))
          else (Some (Three (a, b, c)), d, None)

  let get ms i t =
    match t with
    | One a -> a
    | Two (a, b) -> if i < ms a then a else b
    | Three (a, b, c) ->
        let ms_a = ms a in
        if i < ms_a then a else if i < ms_a + ms b then b else c
    | Four (a, b, c, d) ->
        let ms_a = ms a in
        if i < ms_a then a
        else
          let ms_b = ms_a + ms b in
          if i < ms_b then b else if i < ms_b + ms c then c else d

  let to_seq t () =
    let open Seq in
    match t with
    | One a -> Cons (a, empty)
    | Two (a, b) -> Cons (a, cons b empty)
    | Three (a, b, c) -> Cons (a, cons b (cons c empty))
    | Four (a, b, c, d) -> Cons (a, cons b (cons c (cons d empty)))

  let rev_to_seq t () =
    let open Seq in
    match t with
    | One a -> Cons (a, empty)
    | Two (a, b) -> Cons (b, cons a empty)
    | Three (a, b, c) -> Cons (c, cons b (cons a empty))
    | Four (a, b, c, d) -> Cons (d, cons c (cons b (cons a empty)))
end

module T = struct
  type +'a t =
    | Empty
    | Single of 'a
    | Deep of int Lazy.t * 'a Digit.t * 'a Node.t t Lazy.t * 'a Digit.t

  let empty = Empty
  let lempty = lazy Empty
  let singleton x = Single x

  let split_node ms i t =
    match t with
    | Node.N2 (_, a, b) ->
        if i < ms a then (None, a, Some (Digit.One b))
        else (Some (Digit.One a), b, None)
    | N3 (_, a, b, c) ->
        let ms_a = ms a in
        if i < ms_a then (None, a, Some (Two (b, c)))
        else if i < ms_a + ms b then (Some (One a), b, Some (One b))
        else (Some (Two (a, b)), c, None)

  let _measure ms = function
    | Empty -> 0
    | Single a -> ms a
    | Deep ((lazy v), _, _, _) -> v

  let length t = _measure one t

  let bounds_check i t =
    if i < 0 || i >= length t then
      raise @@ Invalid_argument "index is out of bounds"
    else ()

  let rec pp_debug :
      'a. (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit =
   fun pp_el out t ->
    let open Format in
    match t with
    | Empty -> fprintf out "{}"
    | Single a -> fprintf out "{@[%a@]}" pp_el a
    | Deep (_, l, (lazy m), r) ->
        fprintf out "{@[%a,@ %a,@ %a@]}" (Digit.pp pp_el) l
          (pp_debug (Node.pp pp_el))
          m (Digit.pp pp_el) r

  let show_debug pp_el t = Format.asprintf "%a" (pp_debug pp_el) t

  let deep ms pr mid sf =
    Deep
      ( lazy
          (Digit.measure ms pr
          + _measure Node.measure !!mid
          + Digit.measure ms sf),
        pr,
        mid,
        sf )

  let rec fold_right : 'a. f:('a -> 'b -> 'b) -> 'a t -> init:'b -> 'b =
   fun ~f t ~init ->
    match t with
    | Empty -> init
    | Single x -> f x init
    | Deep (_, pr, (lazy mid), sf) ->
        let f' = Digit.fold_right f
        and f'' = fold_right ~f:(Node.fold_right f) in
        f' pr (f'' mid ~init:(f' sf init))

  let rec fold_left : 'a. f:('b -> 'a -> 'b) -> init:'b -> 'a t -> 'b =
   fun ~f ~init t ->
    match t with
    | Empty -> init
    | Single x -> f init x
    | Deep (_, pr, (lazy mid), sf) ->
        let f' = Digit.fold_left f and f'' = fold_left ~f:(Node.fold_left f) in
        f' (f'' ~init:(f' init pr) mid) sf

  let fold = fold_left

  let rec _ladd : 'a. ('a -> int) -> 'a -> 'a t -> 'a t =
   fun ms a t ->
    match t with
    | Empty -> Single a
    | Single b -> deep ms (One a) lempty (One b)
    | Deep (_, Four (b, c, d, e), mid, sf) ->
        deep ms
          (Two (a, b))
          (lazy (_ladd Node.measure (Node.mk3 ms c d e) !!mid))
          sf
    | Deep (_, pr, mid, sf) -> deep ms (Digit.ladd a pr) mid sf

  let ladd a t = _ladd one a t
  let ( @< ) = ladd

  let rec _ladd_digit : 'a. ('a -> int) -> 'a Digit.t -> 'a t -> 'a t =
   fun ms d t ->
    match (d, t) with
    | One a, Empty -> Single a
    | One a, Single b | Two (a, b), Empty -> deep ms (One a) lempty (One b)
    | Two (a, b), Single c | Three (a, b, c), Empty ->
        deep ms (Two (a, b)) lempty (One c)
    | Three (a, b, c), Single d | Four (a, b, c, d), Empty ->
        deep ms (Two (a, b)) lempty (Two (c, d))
    | Four (a, b, c, d), Single e ->
        deep ms (Three (a, b, c)) lempty (Two (d, e))
    | digit, Deep (_, pr, mid, sf) -> (
        match Digit.joinl ms digit pr with
        | pr', None -> deep ms pr' mid sf
        | pr', Some nodes ->
            deep ms pr' (lazy (_ladd_digit Node.measure nodes !!mid)) sf)

  let rec _radd : 'a. ('a -> int) -> 'a t -> 'a -> 'a t =
   fun ms t z ->
    match t with
    | Empty -> Single z
    | Single y -> deep ms (One y) lempty (One z)
    | Deep (_, pr, mid, Four (v, w, x, y)) ->
        deep ms pr
          (lazy (_radd Node.measure !!mid (Node.mk3 ms v w x)))
          (Two (y, z))
    | Deep (_, pr, mid, sf) -> deep ms pr mid (Digit.radd sf z)

  let radd t a = _radd one t a
  let ( >@ ) = radd

  let rec _radd_digit : 'a. ('a -> int) -> 'a t -> 'a Digit.t -> 'a t =
   fun ms t d ->
    match (t, d) with
    | Empty, One a -> Single a
    | Single a, One b | Empty, Two (a, b) -> deep ms (One a) lempty (One b)
    | Single a, Two (b, c) | Empty, Three (a, b, c) ->
        deep ms (Two (a, b)) lempty (One c)
    | Single a, Three (b, c, d) | Empty, Four (a, b, c, d) ->
        deep ms (Two (a, b)) lempty (Two (c, d))
    | Single a, Four (b, c, d, e) ->
        deep ms (Three (a, b, c)) lempty (Two (d, e))
    | Deep (_, pr, mid, sf), digit -> (
        match Digit.joinr ms sf digit with
        | None, sf' -> deep ms pr mid sf'
        | Some nodes, sf' ->
            deep ms pr (lazy (_radd_digit Node.measure !!mid nodes)) sf')

  let of_digit ms = function
    | Digit.One a -> Single a
    | Two (a, b) -> Deep (lazy (ms a + ms b), One a, lempty, One b)
    | Three (a, b, c) ->
        Deep (lazy (ms a + ms b + ms c), Two (a, b), lempty, One c)
    | Four (a, b, c, d) ->
        Deep (lazy (ms a + ms b + ms c + ms d), Two (a, b), lempty, Two (c, d))

  let rec view_l : 'a. ('a -> int) -> 'a t -> ('a * 'a t Lazy.t) option =
   fun ms t ->
    match t with
    | Empty -> None
    | Single x -> Some (x, lempty)
    | Deep (_, pr, mid, sf) ->
        let hd, tl = Digit.view_l pr in
        Some (hd, deep_l ms tl mid sf)

  and deep_l :
      'a.
      ('a -> int) ->
      'a Digit.t option ->
      'a Node.t t Lazy.t ->
      'a Digit.t ->
      'a t Lazy.t =
   fun ms pr mid sf ->
    lazy
      (match pr with
      | None -> (
          match view_l Node.measure !!mid with
          | None -> of_digit ms sf
          | Some (a, mid) -> deep ms (Digit.of_node a) mid sf)
      | Some pr -> deep ms pr mid sf)

  let lview_lazy t = view_l one t
  let lview t = Option.map (fun (h, (lazy t)) -> (h, t)) (lview_lazy t)
  let is_empty t = match lview_lazy t with None -> true | Some _ -> false

  let hd_left_exn = function
    | Empty -> invalid_arg "can't find head of empty finger tree"
    | Single x -> x
    | Deep (_, pr, _, _) -> Digit.hd pr

  let tl_left t = match lview t with None -> Empty | Some (_, t) -> t

  let rec view_r : 'a. ('a -> int) -> 'a t -> ('a t Lazy.t * 'a) option =
   fun ms t ->
    match t with
    | Empty -> None
    | Single x -> Some (lempty, x)
    | Deep (_, pr, mid, sf) ->
        let hd, tl = Digit.view_r sf in
        Some (deep_r ms pr mid tl, hd)

  and deep_r :
      'a.
      ('a -> int) ->
      'a Digit.t ->
      'a Node.t t Lazy.t ->
      'a Digit.t option ->
      'a t Lazy.t =
   fun ms pr mid sf ->
    lazy
      (match sf with
      | None -> (
          match view_r Node.measure !!mid with
          | None -> of_digit ms pr
          | Some (mid', a) -> deep ms pr mid' (Digit.of_node a))
      | Some sf -> deep ms pr mid sf)

  let rview_lazy t = view_r one t
  let rview t = Option.map (fun ((lazy t), h) -> (t, h)) (rview_lazy t)

  let hd_right_exn = function
    | Empty -> invalid_arg "can't find head of empty finger tree"
    | Single x -> x
    | Deep (_, _, _, sf) -> Digit.hd_r sf

  let tl_right t = match rview t with None -> Empty | Some (t, _) -> t

  let rec _app3 : 'a. ('a -> int) -> 'a t -> 'a Digit.t -> 'a t -> 'a t =
   fun ms t1 d t2 ->
    match (t1, t2) with
    | Empty, xs -> _ladd_digit ms d xs
    | xs, Empty -> _radd_digit ms xs d
    | Single x, xs -> _ladd ms x (_app3 ms Empty d xs)
    | xs, Single x -> _radd ms (_app3 ms xs d Empty) x
    | Deep (_, pr1, m1, sf1), Deep (_, pr2, m2, sf2) ->
        deep ms pr1
          (lazy (_app3 Node.measure !!m1 (Digit.join3 ms sf1 d pr2) !!m2))
          sf2

  let app3 t1 d t2 = _app3 one t1 d t2

  let _join : 'a. ('a -> int) -> 'a t -> 'a t -> 'a t =
   fun ms t1 t2 ->
    match (t1, t2) with
    | Empty, xs | xs, Empty -> xs
    | Single x, xs -> _ladd ms x xs
    | xs, Single x -> _radd ms xs x
    | Deep (_, pr1, m1, sf1), Deep (_, pr2, m2, sf2) ->
        deep ms pr1
          (lazy (_app3 Node.measure !!m1 (Digit.join2 ms sf1 pr2) !!m2))
          sf2

  let join t1 t2 = _join one t1 t2
  let ( >< ) = join
  let join_with t1 el t2 = app3 t1 (One el) t2

  type 'a split = 'a t Lazy.t * 'a * 'a t Lazy.t

  let rec _split : 'a. ('a -> int) -> int -> 'a t -> 'a split =
   fun ms i -> function
    | Empty -> assert false
    | Single x -> (lempty, x, lempty)
    | Deep (_, pr, (lazy mid), sf) ->
        let vpr = Digit.measure ms pr in
        if i < vpr then
          let l, x, r = Digit.split ms i pr in
          let l =
            match l with None -> lempty | Some d -> lazy (of_digit ms d)
          in
          (l, x, deep_l ms r (lazy mid) sf)
        else
          let vm = length mid in
          let i' = i - vpr in
          if i' < vm then
            let (lazy ml), xs, mr = _split Node.measure i' mid in
            let l, x, r = split_node ms (i' - _measure Node.measure ml) xs in
            (deep_r ms pr (lazy ml) l, x, deep_l ms r mr sf)
          else
            let i'' = i' - vm in
            let l, x, r = Digit.split ms i'' sf in
            let r =
              match r with None -> lempty | Some d -> lazy (of_digit ms d)
            in
            (deep_r ms pr (lazy mid) l, x, r)

  let _get : 'a. ('a -> int) -> int -> 'a t -> 'a =
   fun ms i -> function
    | Empty -> assert false
    | Single x -> x
    | Deep (_, pr, (lazy mid), sf) ->
        let vpr = Digit.measure ms pr in
        if i < vpr then
          let x = Digit.get ms i pr in
          x
        else
          let vm = _measure Node.measure mid in
          let i' = i - vpr in
          if i' < vm then
            let (lazy ml), xs, _ = _split Node.measure i' mid in
            Node.get ms (i' - _measure Node.measure ml) xs
          else Digit.get ms i' sf

  let get_unchecked i t = _get one i t

  let get_exn i t =
    bounds_check i t;
    get_unchecked i t

  let get i t =
    match get_exn i t with exception _ -> None | value -> Some value

  let set_unchecked i x t =
    let l, _, r = _split one i t in
    _app3 one !!l (One x) !!r

  let set_exn i el t =
    bounds_check i t;
    set_unchecked i el t

  let set i el t =
    match set_exn i el t with exception _ -> None | value -> Some value

  let partition_lazy_unchecked i t = _split one i t

  let partition_unchecked i t =
    let (lazy l), x, (lazy r) = partition_lazy_unchecked i t in
    (l, x, r)

  let partition_lazy_exn i t =
    bounds_check i t;
    partition_lazy_unchecked i t

  let partition_lazy i t =
    match partition_lazy_exn i t with
    | exception _ -> None
    | value -> Some value

  let partition_exn i t =
    bounds_check i t;
    partition_unchecked i t

  let partition i t =
    match partition_exn i t with exception _ -> None | value -> Some value

  let split_lazy i = function
    | Empty -> (lempty, lempty)
    | xs ->
        let l, x, r = _split one i xs in
        if i < length xs then (l, lazy (_ladd one x @@ !!r))
        else (lazy xs, lempty)

  let split i t =
    let (lazy r), (lazy l) = split_lazy i t in
    (l, r)

  let take n t =
    let (lazy l), _ = split_lazy n t in
    l

  let drop n t =
    let _, (lazy r) = split_lazy n t in
    r

  let rotate i t =
    match length t with
    | 0 -> t
    | len ->
        let i' =
          match i < 0 with true -> len + (i mod len) | false -> i mod len
        in
        let l, r = split i' t in
        r >< l

  let slice ~start ~stop t =
    let (lazy rest), _ = split_lazy stop t in
    let _, (lazy slice) = split_lazy start rest in
    slice

  let insert_unchecked i x t =
    let l, el, r = _split one i t in
    _app3 one !!l (Two (x, el)) !!r

  let insert_exn i el t =
    if i < 0 || i > length t then invalid_arg "index is out of bounds"
    else insert_unchecked i el t

  let insert i el t =
    match insert_exn i el t with exception _ -> None | value -> Some value

  let pop_unchecked i t =
    let l, el, r = partition_unchecked i t in
    (el, l >< r)

  let pop_exn i t =
    bounds_check i t;
    pop_unchecked i t

  let pop i t =
    match pop_exn i t with exception _ -> None | value -> Some value

  let remove_unchecked i t =
    let _, t = pop_unchecked i t in
    t

  let remove_exn i t =
    bounds_check i t;
    remove_unchecked i t

  let remove i t = match remove_exn i t with exception _ -> t | t' -> t'
  let of_list l = List.fold_left radd empty l
  let to_list t = fold_right ~f:List.cons ~init:[] t
  let of_seq s = Seq.fold_left radd empty s
  let iter ~f t = fold_left ~f:(fun () el -> f el) ~init:() t

  let rec _map : 'a 'b. ('b -> int) -> f:('a -> 'b) -> 'a t -> 'b t =
   fun ms ~f -> function
    | Empty -> Empty
    | Single x -> Single (f x)
    | Deep (_, l, m, r) ->
        let m' = lazy (_map Node.measure ~f:(fun n -> Node.map ms f n) !!m) in
        deep ms (Digit.map f l) m' (Digit.map f r)

  let map ~f t = _map one ~f t

  let filter ~f t =
    fold_left t ~init:empty ~f:(fun acc el -> if f el then radd acc el else acc)

  let filter_map ~f t =
    fold_left t ~init:empty ~f:(fun acc el ->
        match f el with None -> acc | Some el -> radd acc el)

  let concat_map ~f t = fold_left t ~init:empty ~f:(fun acc el -> acc >< f el)

  let rec to_seq : 'a. 'a t -> 'a Seq.t =
   fun t () ->
    let open Seq in
    match t with
    | Empty -> Nil
    | Single a -> Cons (a, empty)
    | Deep (_, l, (lazy m), r) ->
        let end' = Digit.to_seq r
        and mid' = concat_map Node.to_seq (to_seq m)
        and start' = Digit.to_seq l in
        (append start' @@ append mid' end') ()

  let rec rev_to_seq : 'a. 'a t -> 'a Seq.t =
   fun t () ->
    let open Seq in
    match t with
    | Empty -> Nil
    | Single a -> Cons (a, empty)
    | Deep (_, l, (lazy m), r) ->
        let end' = Digit.rev_to_seq r
        and mid' = concat_map Node.rev_to_seq (rev_to_seq m)
        and start' = Digit.rev_to_seq l in
        (append end' @@ append mid' start') ()

  let unfold ~f ~init =
    let rec loop acc state =
      match f state with
      | None -> acc
      | Some (el, state') -> loop (acc >@ el) state'
    in
    loop empty init

  module Operators = struct
    let ( @< ) = ( @< )
    let ( >@ ) = ( >@ )
    let ( >< ) = ( >< )
  end

  let pp pp_el out t =
    let open Format in
    let l = to_list t in
    let pp_list out l =
      pp_print_list ~pp_sep:(fun out () -> fprintf out ";@ ") pp_el out l
    in
    fprintf out "Fseq<@[%a@]>" pp_list l

  let show pp_el t = Format.asprintf "%a" (pp pp_el) t

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
        let left, right = split (n / 2) t in
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
end

include T

module NonEmpty = struct
  module Ne = struct
    type +'a t =
      | Single of 'a
      | Deep of int Lazy.t * 'a Digit.t * 'a Node.t T.t Lazy.t * 'a Digit.t
  end

  include Ne

  let singleton x = Single x

  let of_t : 'a T.t -> 'a t option = function
    | Empty -> None
    | Single a -> Some (Single a)
    | Deep (v, l, m, r) -> Some (Deep (v, l, m, r))

  let of_t_exn : 'a T.t -> 'a t = function
    | Empty -> invalid_arg "cannot construct non-empty from empty sequence"
    | Single a -> Single a
    | Deep (v, l, m, r) -> Deep (v, l, m, r)

  let to_t : 'a t -> 'a T.t = function
    | Single a -> Single a
    | Deep (v, l, m, r) -> Deep (v, l, m, r)

  let _measure ms = function Single a -> ms a | Deep ((lazy v), _, _, _) -> v
  let length t = _measure one t

  let pp :
      'a. (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit =
   fun pp_el out t ->
    let open Format in
    match t with
    | Single a -> fprintf out "{@[%a@]}" pp_el a
    | Deep (_, l, (lazy m), r) ->
        fprintf out "{@[%a,@ %a,@ %a@]}" (Digit.pp pp_el) l
          (T.pp (Node.pp pp_el))
          m (Digit.pp pp_el) r

  let show pp_el t = Format.asprintf "%a" (pp pp_el) t

  let deep pr mid sf =
    Deep
      ( lazy
          (Digit.measure one pr
          + T._measure Node.measure !!mid
          + Digit.measure one sf),
        pr,
        mid,
        sf )

  let fold_right ~f t ~init =
    match t with
    | Single x -> f x init
    | Deep (_, pr, (lazy mid), sf) ->
        let f' = Digit.fold_right f
        and f'' = T.fold_right ~f:(Node.fold_right f) in
        f' pr (f'' mid ~init:(f' sf init))

  let fold_left ~f ~init t =
    match t with
    | Single x -> f init x
    | Deep (_, pr, (lazy mid), sf) ->
        let f' = Digit.fold_left f
        and f'' = T.fold_left ~f:(Node.fold_left f) in
        f' (f'' ~init:(f' init pr) mid) sf

  let fold = fold_left

  let ladd a t =
    match t with
    | Single b -> deep (One a) lempty (One b)
    | Deep (_, Four (b, c, d, e), mid, sf) ->
        deep
          (Two (a, b))
          (lazy (T._ladd Node.measure (Node.mk3 one c d e) !!mid))
          sf
    | Deep (_, pr, mid, sf) -> deep (Digit.ladd a pr) mid sf

  let ( @< ) = ladd

  let _ladd_digit d t =
    match (d, t) with
    | Digit.One a, Single b -> deep (One a) lempty (One b)
    | Two (a, b), Single c -> deep (Two (a, b)) lempty (One c)
    | Three (a, b, c), Single d -> deep (Two (a, b)) lempty (Two (c, d))
    | Four (a, b, c, d), Single e -> deep (Three (a, b, c)) lempty (Two (d, e))
    | digit, Deep (_, pr, mid, sf) -> (
        match Digit.joinl one digit pr with
        | pr', None -> deep pr' mid sf
        | pr', Some nodes ->
            deep pr' (lazy (T._ladd_digit Node.measure nodes !!mid)) sf)

  let radd t z =
    match t with
    | Single y -> deep (One y) lempty (One z)
    | Deep (_, pr, mid, Four (v, w, x, y)) ->
        deep pr
          (lazy (T._radd Node.measure !!mid (Node.mk3 one v w x)))
          (Two (y, z))
    | Deep (_, pr, mid, sf) -> deep pr mid (Digit.radd sf z)

  let ( >@ ) = radd

  let _radd_digit t d =
    match (t, d) with
    | Single a, Digit.One b -> deep (One a) lempty (One b)
    | Single a, Two (b, c) -> deep (Two (a, b)) lempty (One c)
    | Single a, Three (b, c, d) -> deep (Two (a, b)) lempty (Two (c, d))
    | Single a, Four (b, c, d, e) -> deep (Three (a, b, c)) lempty (Two (d, e))
    | Deep (_, pr, mid, sf), digit -> (
        match Digit.joinr one sf digit with
        | None, sf' -> deep pr mid sf'
        | Some nodes, sf' ->
            deep pr (lazy (T._radd_digit Node.measure !!mid nodes)) sf')

  let of_digit = function
    | Digit.One a -> Single a
    | Two (a, b) -> Deep (lazy 2, One a, lempty, One b)
    | Three (a, b, c) -> Deep (lazy 3, Two (a, b), lempty, One c)
    | Four (a, b, c, d) -> Deep (lazy 4, Two (a, b), lempty, Two (c, d))

  let rec lview_lazy t =
    match t with
    | Single x -> (x, None)
    | Deep (_, pr, mid, sf) ->
        let hd, tl = Digit.view_l pr in
        (hd, Some (deep_l tl mid sf))

  and deep_l pr mid sf =
    lazy
      (match pr with
      | None -> (
          match T.view_l Node.measure !!mid with
          | None -> of_digit sf
          | Some (a, mid) -> deep (Digit.of_node a) mid sf)
      | Some pr -> deep pr mid sf)

  let lview t =
    match lview_lazy t with
    | h, Some (lazy t) -> (h, Some t)
    | h, None -> (h, None)

  let hd_left = function Single x -> x | Deep (_, pr, _, _) -> Digit.hd pr

  let tl_left_exn t =
    match lview t with
    | _, None ->
        invalid_arg "can't derive non-empty tail of single-element sequence"
    | _, Some t -> t

  let rec rview_lazy t =
    match t with
    | Single x -> (None, x)
    | Deep (_, pr, mid, sf) ->
        let hd, tl = Digit.view_r sf in
        (Some (deep_r pr mid tl), hd)

  and deep_r pr mid sf =
    lazy
      (match sf with
      | None -> (
          match T.view_r Node.measure !!mid with
          | None -> of_digit pr
          | Some (mid', a) -> deep pr mid' (Digit.of_node a))
      | Some sf -> deep pr mid sf)

  let rview t =
    match rview_lazy t with
    | None, h -> (None, h)
    | Some (lazy t), h -> (Some t, h)

  let hd_right = function Single x -> x | Deep (_, _, _, sf) -> Digit.hd_r sf

  let tl_right_exn t =
    match rview t with
    | None, _ ->
        invalid_arg "can't derive non-empty tail of single-element sequence"
    | Some t, _ -> t

  let app3 t1 d t2 =
    match (t1, t2) with
    | Single x, xs -> ladd x (_ladd_digit d xs)
    | xs, Single x -> radd (_radd_digit xs d) x
    | Deep (_, pr1, m1, sf1), Deep (_, pr2, m2, sf2) ->
        deep pr1
          (lazy (T._app3 Node.measure !!m1 (Digit.join3 one sf1 d pr2) !!m2))
          sf2

  let join t1 t2 =
    match (t1, t2) with
    | Single x, xs -> ladd x xs
    | xs, Single x -> radd xs x
    | Deep (_, pr1, m1, sf1), Deep (_, pr2, m2, sf2) ->
        deep pr1
          (lazy (_app3 Node.measure !!m1 (Digit.join2 one sf1 pr2) !!m2))
          sf2

  let ( >< ) = join
  let join_with t1 el t2 = app3 t1 (One el) t2

  let get_unchecked : 'a. int -> 'a t -> 'a =
   fun i -> function
    | Single x -> x
    | Deep (_, pr, (lazy mid), sf) ->
        let vpr = Digit.measure one pr in
        if i < vpr then
          let x = Digit.get one i pr in
          x
        else
          let vm = T._measure Node.measure mid in
          let i' = i - vpr in
          if i' < vm then
            let (lazy ml), xs, _ = _split Node.measure i' mid in
            Node.get one (i' - T._measure Node.measure ml) xs
          else Digit.get one i' sf

  let get i t =
    if i < 0 || i >= length t then None else Some (get_unchecked i t)

  let insert_unchecked i x t = of_t_exn @@ insert_unchecked i x @@ to_t t

  let insert i x t =
    match insert i x @@ to_t t with None -> None | Some t' -> of_t t'

  let set_unchecked i x t = of_t_exn @@ set_unchecked i x @@ to_t t

  let set i x t =
    match set i x @@ to_t t with None -> None | Some t2 -> of_t t2

  let rotate n t = of_t_exn @@ rotate n @@ to_t t
  let msort ~cmp t = of_t_exn @@ msort ~cmp @@ to_t t
  let sort ~cmp t = of_t_exn @@ sort ~cmp @@ to_t t

  let of_list = function
    | [] -> None
    | h :: t -> Some (List.fold_left radd (singleton h) t)

  let to_list t = fold_right ~f:List.cons ~init:[] t

  let of_seq s =
    let open Seq in
    match s () with
    | Nil -> None
    | Cons (h, t) -> Some (Seq.fold_left radd (singleton h) t)

  let iter ~f t = fold_left ~f:(fun () el -> f el) ~init:() t

  let map ~f = function
    | Single x -> Single (f x)
    | Deep (_, l, m, r) ->
        let m' =
          lazy (T._map Node.measure ~f:(fun n -> Node.map one f n) !!m)
        in
        deep (Digit.map f l) m' (Digit.map f r)

  let concat_map ~f t =
    match lview t with
    | h, None -> f h
    | h, Some t -> fold_left t ~init:(f h) ~f:(fun acc el -> acc >< f el)

  let to_seq : 'a. 'a t -> 'a Seq.t =
   fun t () ->
    let open Seq in
    match t with
    | Single a -> Cons (a, empty)
    | Deep (_, l, (lazy m), r) ->
        let end' = Digit.to_seq r
        and mid' = concat_map Node.to_seq (T.to_seq m)
        and start' = Digit.to_seq l in
        (append start' @@ append mid' end') ()

  let rev_to_seq : 'a. 'a t -> 'a Seq.t =
   fun t () ->
    let open Seq in
    match t with
    | Single a -> Cons (a, empty)
    | Deep (_, l, (lazy m), r) ->
        let end' = Digit.rev_to_seq r
        and mid' = concat_map Node.rev_to_seq (T.rev_to_seq m)
        and start' = Digit.rev_to_seq l in
        (append end' @@ append mid' start') ()

  let unfold ~f ~init =
    match f init with
    | None -> None
    | Some (el, state) ->
        let rec loop acc state =
          match f state with
          | None -> acc
          | Some (el, state') -> loop (acc >@ el) state'
        in
        Some (loop (singleton el) state)

  let init ~len ~f =
    if len < 1 then invalid_arg "nonempty length cannot be less than 1";
    let rec loop i t = if i >= len then t else loop (i + 1) (t >@ f i) in
    loop 1 (singleton (f 0))

  let to_array t =
    let len = length t in
    let v = hd_left t in
    let a = Array.make len v in
    ignore
    @@ fold_left ~init:0 t ~f:(fun i el ->
           Array.unsafe_set a i el;
           succ i);
    a

  module Operators = struct
    let ( @< ) = ( @< )
    let ( >@ ) = ( >@ )
    let ( >< ) = ( >< )
  end
end

let to_nonempty = NonEmpty.of_t
let of_nonempty = NonEmpty.to_t

module Functor = Functor
module Pqueue = Pqueue
