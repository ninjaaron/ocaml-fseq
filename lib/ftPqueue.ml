
module type Orderable = sig
  type t
  val compare : t -> t -> int
  type +'a elt
  val priority : 'a elt -> t
end

module Make(M : Orderable) = struct
  type prio = MInfty | Prio of M.t

  include Functor.Make(struct
    type 'a elt = 'a M.elt
    type monoid = prio
    let null = MInfty 
    let measure el = Prio (M.priority el)
    let add p1 p2 =
      match p1, p2 with
      | MInfty, p | p, MInfty -> p
      | Prio a, Prio b ->
        Prio (if M.compare a b >= 0 then a else b)
  end)


  let get_max t =
    let p prio =
      match measure t, prio with
      | MInfty, _ -> true
      | _, MInfty -> false
      | Prio t, Prio el ->
        M.compare t el <= 0 in
    let open La_base in
    let+! l, el, r = partition ~p t in
    el, l >< r
end

include Make(struct
    include Int
    type 'a elt = int * 'a
    let priority (i, _) = i
  end)

let pp pp_el out t =
  let open Format in
  let pp_el' out (n, value) =
    fprintf out "(%d@ %a)" n pp_el value in
  Format.fprintf out "%a" (pp pp_el') t
