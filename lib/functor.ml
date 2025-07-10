module type Container = sig type 'a t end

type 'a thunk = unit -> 'a

module Split (C: Container) : sig
  type 'a split = Split of  'a C.t Lazy.t * 'a * 'a C.t Lazy.t
end = struct
  type 'a split = Split of 'a C.t Lazy.t * 'a * 'a C.t Lazy.t
end

module type Measurable = sig
  type +'a elt
  type monoid
  val null : monoid
  val measure : 'a elt -> monoid
  val add : monoid -> monoid -> monoid
end

module type S = sig

  type monoid

  module Node : sig
    type 'a t =
      | N2 of monoid * 'a * 'a
      | N3 of monoid * 'a * 'a * 'a
    val measure : 'a t -> monoid
    val pp : (Format.formatter -> 'a -> unit)
      -> Format.formatter -> 'a t -> unit
    val show : (Format.formatter -> 'a -> unit) -> 'a t -> string
  end

  module Digit : sig
    type 'a t =
      | One of 'a
      | Two of 'a * 'a
      | Three of 'a * 'a * 'a
      | Four of 'a * 'a * 'a * 'a
    val pp : (Format.formatter -> 'a -> unit)
      -> Format.formatter -> 'a t -> unit
    val show : (Format.formatter -> 'a -> unit) -> 'a t -> string
  end

  type +'a t0 =
    | Empty
    | Single of 'a
    | Deep of monoid * 'a Digit.t * 'a Node.t t0 Lazy.t * 'a Digit.t

  type +'a elt
  type +'a t

  val pp : (Format.formatter -> 'a elt -> unit)
    -> Format.formatter -> 'a t -> unit
    [@@ocaml.toplevel_printer]
  val show : (Format.formatter -> 'a elt -> unit) -> 'a t -> string
  val empty : 'a t
  val measure : 'a t -> monoid
  val fold_right : f:('a elt -> 'b -> 'b) -> 'a t -> init:'b -> 'b
  val fold_left : f:('b -> 'a elt -> 'b) -> init:'b -> 'a t -> 'b
  val map : f:('a elt -> 'b elt) -> 'a t -> 'b t
  val filter : f:('a elt -> bool) -> 'a t -> 'a t
  val filter_map : f:('a elt -> 'b elt option) -> 'a t -> 'b t
  val concat_map : f:('a elt -> 'b t) -> 'a t -> 'b t
  val iter : f:('a elt -> unit) -> 'a t -> unit
  val cons_left : 'a elt -> 'a t -> 'a t
  val cons_right : 'a t -> 'a elt -> 'a t
  val (<$) : 'a elt -> 'a t -> 'a t
  val ($>) : 'a t -> 'a elt -> 'a t
  val view_left : 'a t -> ('a elt * 'a t Lazy.t) option
  val view_right : 'a t -> ('a elt * 'a t Lazy.t) option
  val is_empty: 'a t -> bool
  val hd_left_exn : 'a t -> 'a elt
  val tl_left : 'a t -> 'a t
  val hd_right_exn : 'a t -> 'a elt
  val tl_right : 'a t -> 'a t
  val join : 'a t -> 'a t -> 'a t
  val (><) : 'a t -> 'a t -> 'a t
  val join_with : 'a t -> 'a elt -> 'a t -> 'a t
  val concat : 'a t list -> 'a t
  val partition
    : p:(monoid -> bool) -> 'a t ->
    ('a t Lazy.t * 'a elt * 'a t Lazy.t, string) result
  val split : p:(monoid -> bool) -> 'a t -> 'a t Lazy.t * 'a t Lazy.t
  val get : p:(monoid -> bool) -> 'a t -> 'a elt
  val insert : p:(monoid -> bool) -> 'a elt -> 'a t -> 'a t
  val update : p:(monoid -> bool) -> 'a elt -> 'a t -> 'a t
  val of_list : 'a elt list -> 'a t
  val to_list : 'a t -> 'a elt list
  val of_seq : 'a elt Seq.t -> 'a t
  val to_seq : 'a t -> 'a elt Seq.t
  val rev_to_seq : 'a t -> 'a elt Seq.t
end

module Make (M: Measurable)
  : (S with type monoid := M.monoid
        and type 'a elt := 'a M.elt)
= struct
  type +'a elt = 'a M.elt
  type 'a measure = 'a -> M.monoid
  let (+) = M.add

  module Node = struct
    type 'a t = N2 of M.monoid * 'a * 'a | N3 of M.monoid * 'a * 'a * 'a

    let pp pp_el out t =
      let open Format in
      match t with
      | N2 (_,a,b) -> fprintf out "(@[%a,@ %a@])" pp_el a pp_el b
      | N3 (_,a,b,c) ->
        fprintf out "(@[%a,@ %a@, %a@])" pp_el a pp_el b pp_el c

    let show pp_el t = Format.asprintf "%a" (pp pp_el) t

    let measure : 'a measure = function
      | N2 (v,_,_) -> v
      | N3 (v,_,_,_) -> v

    let mk2 (ms:'a measure) a b = N2 (ms a + ms b, a, b)
    let mk3 (ms:'a measure) a b c = N3 (ms a + ms b + ms c, a, b, c)

    let fold_right f t acc =
      match t with
      | N2 (_,a,b) ->  f a (f b acc)
      | N3 (_,a,b,c) ->  f a (f b (f c acc))

    let fold_left f acc = function
      | N2 (_,a,b) -> f (f acc a) b
      | N3 (_,a,b,c) -> f (f (f acc a) b) c
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
      | Two(a,b) -> fprintf out "[@[%a,@ %a@]]" pp_el a pp_el b
      | Three(a,b,c) ->
        fprintf out "[@[%a,@ %a,@ %a@]]" pp_el a pp_el b pp_el c
      | Four(a,b,c,d) ->
        fprintf out "[@[%a,@ %a,@ %a,@ %a@]]" pp_el a pp_el b pp_el c pp_el d

    let show pp_el t = Format.asprintf "%a" (pp pp_el) t

    let measure (ms:'a measure) : 'a t measure = function
      | One a -> ms a
      | Two (a,b) -> ms a + ms b
      | Three (a,b,c) -> ms a + ms b + ms c
      | Four (a,b,c,d) -> ms a + ms b + ms c + ms d

    let of_node = function
      | Node.N2 (_,a,b) -> Two (a,b)
      | Node.N3 (_,a,b,c) -> Three (a,b,c)

    let joinl ms t1 t2 =
      match t1, t2 with
      | One a, One b -> Two(a,b), None
      | One a, Two(b,c)
      | Two(a,b), One c -> Three(a,b,c), None
      | One a, Three(b,c,d)
      | Three(a,b,c), One d
      | Two(a,b), Two(c,d) ->
        Four(a,b,c,d), None
      | One a, Four(b,c,d,e)
      | Four(a,b,c,d), One e
      | Two(a,b), Three(c,d,e)
      | Three(a,b,c), Two(d,e) ->
        Two(a,b), Some (One (Node.mk3 ms c d e))
      | Two(a,b), Four(c,d,e,f)
      | Four(a,b,c,d), Two(e,f)
      | Three(a,b,c), Three(d,e,f) ->
        Three(a,b,c), Some (One (Node.mk3 ms d e f))
      | Three(a,b,c), Four(d,e,f,g)
      | Four(a,b,c,d), Three(e,f,g) ->
        One a, Some (Two (Node.mk3 ms b c d, Node.mk3 ms e f g))
      | Four(a,b,c,d), Four(e,f,g,h) ->
        Two(a,b), Some (Two (Node.mk3 ms c d e, Node.mk3 ms f g h))

    let joinr ms t1 t2 =
      match t1, t2 with
      | One a, Four(b,c,d,e)
      | Four(a,b,c,d), One e
      | Two(a,b), Three(c,d,e)
      | Three(a,b,c), Two(d,e) ->
        Some (One (Node.mk3 ms a b c)), Two(d,e)
      | Two(a,b), Four(c,d,e,f)
      | Four(a,b,c,d), Two(e,f)
      | Three(a,b,c), Three(d,e,f) ->
        Some (One (Node.mk3 ms a b c)), Three(d,e,f)
      | Three(a,b,c), Four(d,e,f,g)
      | Four(a,b,c,d), Three(e,f,g) ->
        Some (Two (Node.mk3 ms a b c, Node.mk3 ms d e f)), One g
      | Four(a,b,c,d), Four(e,f,g,h) ->
        Some (Two (Node.mk3 ms a b c, Node.mk3 ms d e f)), Two(g,h)
      | _, _ -> let d, nodes = joinl ms t1 t2 in
        match nodes with Some _ -> assert false | None -> None, d

    let join2 ms t1 t2 =
      match t1, t2 with
      (* 2 *)
      | One a, One b ->
        One(Node.mk2 ms a b)
      (* 3 *)
      | One a, Two(b,c)
      | Two(a,b), One c ->
        One(Node.mk3 ms a b c)
      (* 4 *)
      | One a, Three(b,c,d)
      | Two(a,b), Two(c,d)
      | Three(a,b,c), One d ->
        Two(Node.mk2 ms a b, Node.mk2 ms c d) 
      (* 5 *)
      | One a, Four(b,c,d,e)
      | Two(a,b), Three(c,d,e)
      | Three(a,b,c), Two(d,e)
      | Four(a,b,c,d), One e ->
        Node.(Two(mk3 ms a b c, mk2 ms d e))
      (* 6 *)
      | Two(a,b), Four(c,d,e,f)
      | Three(a,b,c), Three(d,e,f)
      | Four(a,b,c,d), Two(e,f) ->
        Node.(Two(mk3 ms a b c, mk3 ms d e f))
      (* 7 *)
      | Three(a,b,c), Four(d,e,f,g)
      | Four(a,b,c,d), Three(e,f,g) ->
        Node.(Three(mk3 ms a b c, mk2 ms d e, mk2 ms f g))
      (* 8 *)
      | Four(a,b,c,d), Four(e,f,g,h) ->
        Node.(Three(mk3 ms a b c, mk3 ms d e f, mk2 ms g h))


    let join3 ms t1 t2 t3 =
      match t1, t2, t3 with
      (* 3 *)
      | One a, One b, One c ->
        One(Node.mk3 ms a b c)
       (* 4 *) 
      | One a, One b, Two(c,d)
      | One a, Two(b,c), One d
      | Two(a,b), One c, One d ->
        Two(Node.mk2 ms a b, Node.mk2 ms c d) 
      (* 5 *)
      | One a, One b, Three(c,d,e)
      | One a, Two(b,c), Two(d,e)
      | One a, Three(b,c,d), One e
      | Two(a,b), One c, Two(d,e)
      | Two(a,b), Two(c,d), One e
      | Three(a,b,c), One d, One e ->
        Node.(Two(mk3 ms a b c, mk2 ms d e))
      (* 6 *)
      | One a, One b, Four(c,d,e,f)
      | One a, Two(b,c), Three(d,e,f)
      | One a, Three(b,c,d), Two(e,f)
      | One a, Four(b,c,d,e), One f
      | Two(a,b), One c, Three(d,e,f)
      | Two(a,b), Two(c,d), Two(e,f)
      | Two(a,b), Three(c,d,e), One f
      | Three(a,b,c), One d, Two(e,f)
      | Three(a,b,c), Two(d,e), One f
      | Four(a,b,c,d), One e, One f ->
        Node.(Two(mk3 ms a b c, mk3 ms d e f))
      (* 7 *)
      | One a, Two(b,c), Four(d,e,f,g)
      | One a, Three(b,c,d), Three(e,f,g)
      | One a, Four(b,c,d,e), Two(f,g)
      | Two(a,b), One c, Four(d,e,f,g)
      | Two(a,b), Two(c,d), Three(e,f,g)
      | Two(a,b), Three(c,d,e), Two(f,g)
      | Two(a,b), Four(c,d,e,f), One g
      | Three(a,b,c), One d, Three(e,f,g)
      | Three(a,b,c), Two(d,e), Two(f,g)
      | Three(a,b,c), Three(d,e,f), One g
      | Four(a,b,c,d), One e, Two(f,g)
      | Four(a,b,c,d), Two(e,f), One g ->
        Node.(Three(mk3 ms a b c, mk2 ms d e, mk2 ms f g))
      (* 8 *)
      | One a, Three(b,c,d), Four(e,f,g,h)
      | One a, Four(b,c,d,e), Three(f,g,h)
      | Two(a,b), Two(c,d), Four(e,f,g,h)
      | Two(a,b), Three(c,d,e), Three(f,g,h)
      | Two(a,b), Four(c,d,e,f), Two(g,h)
      | Three(a,b,c), One d, Four(e,f,g,h)
      | Three(a,b,c), Two(d,e), Three(f,g,h)
      | Three(a,b,c), Three(d,e,f), Two(g,h)
      | Three(a,b,c), Four(d,e,f,g), One h
      | Four(a,b,c,d), One e, Three(f,g,h)
      | Four(a,b,c,d), Two(e,f), Two(g,h)
      | Four(a,b,c,d), Three(e,f,g), One h ->
        Node.(Three(mk3 ms a b c, mk3 ms d e f, mk2 ms g h))
      (* 9 *)
      | One a, Four(b,c,d,e), Four(f,g,h,i)
      | Two(a,b), Three(c,d,e), Four(f,g,h,i)
      | Two(a,b), Four(c,d,e,f), Three(g,h,i)
      | Three(a,b,c), Two(d,e), Four(f,g,h,i)
      | Three(a,b,c), Three(d,e,f), Three(g,h,i)
      | Three(a,b,c), Four(d,e,f,g), Two(h,i)
      | Four(a,b,c,d), One e, Four(f,g,h,i)
      | Four(a,b,c,d), Two(e,f), Three(g,h,i)
      | Four(a,b,c,d), Three(e,f,g), Two(h,i)
      | Four(a,b,c,d), Four(e,f,g,h), One i ->
        Node.(Three(mk3 ms a b c, mk3 ms d e f, mk3 ms g h i))
      (* 10 *)
      | Two(a,b), Four(c,d,e,f), Four(g,h,i,j)
      | Three(a,b,c), Three(d,e,f), Four(g,h,i,j)
      | Three(a,b,c), Four(d,e,f,g), Three(h,i,j)
      | Four(a,b,c,d), Two(e,f), Four(g,h,i,j)
      | Four(a,b,c,d), Three(e,f,g), Three(h,i,j)
      | Four(a,b,c,d), Four(e,f,g,h), Two(i,j) ->
        Node.(Four(mk3 ms a b c, mk3 ms d e f, mk2 ms g h, mk2 ms i j))
      (* 11 *)
      | Three(a,b,c), Four(d,e,f,g), Four(h,i,j,k)
      | Four(a,b,c,d), Three(e,f,g), Four(h,i,j,k)
      | Four(a,b,c,d), Four(e,f,g,h), Three(i,j,k) ->
        Node.(Four(mk3 ms a b c, mk3 ms d e f, mk3 ms g h i, mk2 ms j k))
      (* 12 *)
      | Four(a,b,c,d), Four(e,f,g,h), Four(i,j,k,l) ->
        Node.(Four(mk3 ms a b c, mk3 ms d e f, mk3 ms g h i, mk3 ms j k l))


    let fold_left f acc = function
      | One a -> f acc a
      | Two (a,b) -> f (f acc a) b
      | Three (a,b,c) -> f (f (f acc a) b) c
      | Four (a,b,c,d) -> f (f (f (f acc a) b) c) d

    let fold_right f t acc = match t with
      | One a -> f a acc
      | Two (a,b) -> f a (f b acc)
      | Three (a,b,c) -> f a (f b (f c acc))
      | Four (a,b,c,d) -> f a (f b (f c (f d acc)))

    let ladd a = function
      | One b -> Two (a,b)
      | Two (b,c) -> Three (a,b,c)
      | Three (b,c,d) -> Four (a,b,c,d)
      | _ -> invalid_arg "cannot ladd Four digit"

    let view_l = function
      | One a -> a, None
      | Two (a,b) -> a, Some (One b)
      | Three (a,b,c) -> a, Some(Two (b,c))
      | Four (a,b,c,d) -> a, Some(Three (b,c,d))

    let hd = function
      | One a -> a
      | Two (a,_) -> a
      | Three (a,_,_) -> a
      | Four (a,_,_,_) -> a

    let radd t z = match t with
      | One y -> Two (y,z)
      | Two (x,y) -> Three (x,y,z)
      | Three (w,x,y) -> Four (w,x,y,z)
      | _ -> invalid_arg "cannot radd Four digit"

    let view_r = function
      | One a -> a, None
      | Two (a,b) -> b, Some(One a)
      | Three (a,b,c) -> c, Some(Two (a,b))
      | Four (a,b,c,d) -> d, Some(Three (a,b,c))

    let hd_r = function
      | One a -> a
      | Two (_,b) -> b
      | Three (_,_,c) -> c
      | Four (_,_,_,d) -> d

    type 'a split = Split of 'a t option * 'a * 'a t option

    let rec split (ms: 'a measure) p i t =
      match view_l t with
      | x, None -> Split (None, x, None)
      | x, Some xs ->
        let i' = i + ms x in
        if p i' then Split (None, x, Some xs)
        else let Split (l,x',r) = split ms p i' xs in
          match l with
          | None -> Split(Some(One(x)), x', r)
          | Some l ->
          Split (Some(ladd x l), x', r)
            
    let rec get (ms: 'a measure) p i t =
      match view_l t with
      | x, None -> x
      | x, Some xs ->
        let i' = i + ms x in
        if p i' then x
        else let x' = get ms p i' xs in x'
  end

  type +'a t0 =
    | Empty
    | Single of 'a
    | Deep of M.monoid * 'a Digit.t * 'a Node.t t0 Lazy.t * 'a Digit.t

  type +'a t = 'a elt t0
  
  let empty = Empty
  let lempty = lazy Empty

  let _measure ms = function
    | Empty -> M.null
    | Single a -> ms a
    | Deep (v,_,_,_) -> v 

  let measure t = _measure M.measure t

  let force = Lazy.force

  let rec pp :
    'a. (Format.formatter -> 'a -> unit) ->
    Format.formatter -> 'a t0 -> unit =
    fun pp_el out t ->
    let open Format in
    match t with
    | Empty -> fprintf out "{}"
    | Single a -> fprintf out "{@[%a@]}" pp_el a
    | Deep(_,l,lazy m,r) ->
      fprintf out "{@[%a,@ %a,@ %a@]}"
        (Digit.pp pp_el) l
        (pp (Node.pp pp_el)) m
        (Digit.pp pp_el) r

  let show pp_el t = Format.asprintf "%a" (pp pp_el) t

  let deep ms pr mid sf =
    Deep (
      Digit.measure ms pr + _measure Node.measure mid + Digit.measure ms sf,
      pr, lazy mid, sf
    )
    

  let rec fold_right : 'a. f:('a -> 'b -> 'b) -> 'a t0 -> init:'b -> 'b =
    fun ~f t ~init -> match t with
      | Empty -> init
      | Single x -> f x init
      | Deep (_,pr,lazy mid,sf) ->
        let f' = Digit.fold_right f
        and f'' = fold_right ~f:(Node.fold_right f) in
        f' pr (f'' mid ~init:(f' sf init))

  let rec fold_left : 'a. f:('b -> 'a -> 'b) -> init:'b -> 'a t0 -> 'b =
    fun ~f ~init t -> match t with
      | Empty -> init
      | Single x -> f init x
      | Deep (_,pr,lazy mid,sf) ->
        let f' = Digit.fold_left f
        and f'' = fold_left ~f:(Node.fold_left f) in
        f' (f'' ~init:(f' init pr) mid) sf

  let rec _ladd : 'a. 'a measure -> 'a -> 'a t0 -> 'a t0 = fun ms a t ->
    match t with
    | Empty -> Single a
    | Single b -> deep ms (One a) Empty (One b)
    | Deep (_, Four (b,c,d,e), lazy mid, sf) ->
      deep ms
        (Two (a,b)) (_ladd Node.measure (Node.mk3 ms c d e) mid) sf
    | Deep (_,pr,lazy mid, sf) -> deep ms (Digit.ladd a pr) mid sf
  let cons_left a t = _ladd M.measure a t

  let (<$) = cons_left

  let rec _ladd_digit : 'a. 'a measure -> 'a Digit.t -> 'a t0 -> 'a t0 =
    fun ms d t ->
    match d, t with
    | One a, Empty -> Single a
    | One a, Single b
    | Two(a,b), Empty -> deep ms (One a) Empty (One b)
    | Two(a,b), Single c
    | Three(a,b,c), Empty -> deep ms (Two(a,b)) Empty (One c)
    | Three(a,b,c), Single d
    | Four(a,b,c,d), Empty -> deep ms (Two(a,b)) Empty (Two(c,d))
    | Four(a,b,c,d), Single e -> deep ms (Three(a,b,c)) Empty (Two(d,e))
    | digit, Deep (_, pr, lazy mid, sf) ->
      match Digit.joinl ms digit pr with
      | pr', None -> deep ms pr' mid sf
      | pr', Some nodes ->
        deep ms pr' (_ladd_digit Node.measure nodes mid) sf

  let rec _radd : 'a. 'a measure -> 'a t0 -> 'a -> 'a t0 = fun ms t z ->
    match t with
    | Empty -> Single z
    | Single y -> deep ms (One y) Empty (One z)
    | Deep (_, pr, lazy mid, Four (v,w,x,y)) ->
      deep ms pr
        (_radd Node.measure mid (Node.mk3 ms v w x))
        (Two (y,z))
    | Deep (_, pr, lazy mid, sf) -> deep ms pr mid (Digit.radd sf z)
  let cons_right t a = _radd M.measure t a

  let ($>) = cons_right

  let rec _radd_digit : 'a. 'a measure -> 'a t0 -> 'a Digit.t -> 'a t0 =
    fun ms t d ->
    match t, d with
    | Empty, One a -> Single a
    | Single a, One b
    | Empty, Two(a,b) -> deep ms (One a) Empty (One b)
    | Single a, Two(b,c)
    | Empty, Three(a,b,c) -> deep ms (Two(a,b)) Empty (One c)
    | Single a, Three(b,c,d)
    | Empty, Four(a,b,c,d) -> deep ms (Two(a,b)) Empty (Two(c,d))
    | Single a, Four(b,c,d,e) -> deep ms (Three(a,b,c)) Empty (Two(d,e))
    | Deep (_, pr, lazy mid, sf), digit ->
      match Digit.joinr ms sf digit with
      | None, sf' -> deep ms pr mid sf'
      | Some nodes, sf' ->
        deep ms pr (_radd_digit Node.measure mid nodes) sf'

  let of_digit ms d = Digit.fold_left (_radd ms) Empty d

  let rec view_l : 'a. 'a measure -> 'a t0 ->  ('a * 'a t0 Lazy.t) option =
    fun ms t ->
    match t with
    | Empty -> None
    | Single x -> Some (x, lempty)  
    | Deep (_,pr,mid,sf) ->
      let hd, tl = Digit.view_l pr in
      Some (hd, deep_l ms tl mid sf)
  and deep_l : 'a. 'a measure -> 'a Digit.t option ->
    'a Node.t t0 Lazy.t -> 'a Digit.t -> 'a t0 Lazy.t =
    fun ms pr mid sf -> lazy (
      let mid = force mid in
      match pr with
      | None -> begin match view_l Node.measure mid with
          | None -> of_digit ms sf
          | Some (a,lazy mid') -> deep ms (Digit.of_node a) mid' sf
        end
      | Some pr -> deep ms pr mid sf)
  let view_left t = view_l M.measure t

  let is_empty t =
    match view_left t with
    | None -> true
    | Some _ -> false

  let hd_left_exn = function
    | Empty -> invalid_arg "can't find head of empty finger tree"
    | Single x -> x
    | Deep (_,pr,_,_) -> Digit.hd pr

  let tl_left t = match view_left t with
    | None -> Empty
    | Some (_, lazy t) -> t

  let rec view_r : 'a. 'a measure -> 'a t0 -> ('a * 'a t0 Lazy.t) option =
    fun ms t ->
    match t with
    | Empty -> None
    | Single x -> Some (x, lempty)
    | Deep (_,pr,mid,sf) ->
      let hd, tl = Digit.view_r sf in
      Some (hd, deep_r ms pr mid tl)
  and deep_r :
    'a. 'a measure -> 'a Digit.t -> 'a Node.t t0 Lazy.t -> 'a Digit.t option -> 'a t0 Lazy.t =
    fun ms pr mid sf -> lazy (
      let mid = force mid in
      match sf with
      | None -> begin match view_r Node.measure mid with
          | None -> of_digit ms pr
          | Some (a, lazy mid') ->
            deep ms pr mid' (Digit.of_node a)
        end
      | Some sf -> deep ms pr mid sf)
  let view_right t = view_r M.measure t

  let hd_right_exn = function
    | Empty -> invalid_arg "can't find head of empty finger tree"
    | Single x -> x
    | Deep (_,_,_,sf) -> Digit.hd_r sf

  let tl_right t =
    match view_right t with
    | None -> Empty
    | Some(_, lazy t) -> t

  let rec _app3 : 'a. 'a measure -> 'a t0 -> 'a Digit.t -> 'a t0 -> 'a t0 =
    fun ms t1 d t2 ->
    match t1, t2 with
    | Empty, xs -> _ladd_digit ms d xs
    | xs, Empty -> _radd_digit ms xs d
    | (Single x), xs -> _ladd ms x (_app3 ms Empty d xs)
    | xs, (Single x) -> _radd ms (_app3 ms xs d Empty) x
    | Deep (_,pr1,lazy m1,sf1), Deep (_,pr2,lazy m2,sf2) ->
      deep ms pr1
        (_app3 Node.measure m1 (Digit.join3 ms sf1 d pr2) m2)
        sf2

  let app3 t1 d t2 = _app3 M.measure t1 d t2

  let _join : 'a. 'a measure -> 'a t0 -> 'a t0 -> 'a t0 =
    fun ms t1 t2 ->
    match t1, t2 with
    | Empty, xs | xs, Empty -> xs
    | (Single x), xs -> _ladd ms x xs
    | xs, (Single x) -> _radd ms xs x
    | Deep (_,pr1,lazy m1,sf1), Deep (_,pr2,lazy m2,sf2) -> 
      deep ms pr1
         (_app3 Node.measure m1 (Digit.join2 ms sf1 pr2) m2)
        sf2

  let join t1 t2 = _join M.measure t1 t2
  let (><) = join
  let join_with t1 el t2 = app3 t1 (One el) t2
  let concat = function
    | [] -> Empty
    | t :: ts -> List.fold_left join t ts

  include Split(struct type 'a t = 'a t0 end)

  let rec _split :
    'a. 'a measure -> (M.monoid -> bool) -> M.monoid -> 'a t0 -> 'a split =
    fun ms p i -> function
      | Empty -> assert false
      | Single x -> Split (lempty, x, lempty)
      | Deep (_,pr,lazy mid,sf) ->
        let vpr = i + Digit.measure ms pr in
        let vm = vpr + _measure Node.measure mid in
        if p vpr then
          let Split(l,x,r) = Digit.split ms p i pr in
          let l = match l with
            | None -> lempty
            | Some d -> lazy (of_digit ms d)  in
          Split (l, x, deep_l ms r (lazy mid) sf)
        else if p vm then
          let Split (lazy ml, xs, mr) = _split Node.measure p vpr mid in
          let Split (l, x, r) =
            Digit.split ms p
              (vpr + _measure Node.measure ml)
              (Digit.of_node xs) in
          Split (deep_r ms pr (lazy ml) l,
                 x,
                 deep_l ms r mr sf)
        else
          let Split (l,x,r) = Digit.split ms p vm sf in
          let r = match r with
            | None -> lempty
            | Some d -> lazy (of_digit ms d)  in
          Split (deep_r ms pr (lazy mid) l, x, r)

  let get :
    'a. 'a measure -> (M.monoid -> bool) -> M.monoid -> 'a t0 -> 'a =
    fun ms p i -> function
      | Empty -> assert false
      | Single x -> x
      | Deep (_,pr,lazy mid,sf) ->
        let vpr = i + Digit.measure ms pr in
        let vm = vpr + _measure Node.measure mid in
        if p vpr then
          let x = Digit.get ms p i pr in x
        else if p vm then
          let Split (lazy ml, xs, _) = _split Node.measure p vpr mid in
          let x = Digit.get ms p
              (vpr + _measure Node.measure ml)
              (Digit.of_node xs) in x
        else
          let x = Digit.get ms p vm sf in
          x
  let get ~p t = get M.measure p M.null t

  let partition ~p = function
    | Empty -> Error "cannot partition empty tree"
    | xs ->
      let Split (l,x,r) = _split M.measure p M.null xs in
      if p (measure xs) then
        Ok (l, x, r)
      else Error "predicate p was not satisfied by tree"

  let split ~p = function
    | Empty -> (lempty, lempty)
    | xs ->
      let Split (l,x,r) = _split M.measure p M.null xs in
      if p (measure xs) then
        (l, lazy (_ladd M.measure x @@ force r))
      else (lazy xs, lempty)

  let insert ~p x t =
    let Split(l, el, r) = _split M.measure p M.null t in
    _app3 M.measure (force l) (Two(x,el)) (force r)

  let update ~p x t =
    let Split(l, _, r) = _split M.measure p M.null t in
    _app3 M.measure (force l) (One x) (force r)

  let of_list l = List.fold_left cons_right empty l
  let to_list t = fold_right ~f:List.cons ~init:[] t
  let of_seq s = Seq.fold_left cons_right empty s
  let iter ~f t = fold_left ~f:(fun () el -> f el) ~init:() t
  let map ~f t = fold_left t ~init:empty
      ~f:(fun acc el -> cons_right acc (f el))
  let filter ~f t = fold_left t ~init:empty
      ~f:(fun acc el -> if f el then cons_right acc el else acc)
  let filter_map ~f t = fold_left t ~init:empty
      ~f:(fun acc el -> match f el with
            None -> acc | Some el -> cons_right acc el)
  let concat_map ~f t = fold_left t ~init:empty
      ~f:(fun acc el -> join acc (f el))

  let rec _to_seq_node view t = match view t with
    | None -> Seq.Nil
    | Some (hd, tl) ->
      Seq.Cons(hd, fun () -> _to_seq_node view (force tl))

  let to_seq t () = _to_seq_node view_left t
  let rev_to_seq t () = _to_seq_node view_right t
end
