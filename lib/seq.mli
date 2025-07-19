type 'a t
val empty : 'a t
val is_empty : 'a t -> bool
val singleton : 'a -> 'a t
val length : 'a t -> int
val ladd : 'a -> 'a t -> 'a t
val ( @> ) : 'a -> 'a t -> 'a t
val radd : 'a t -> 'a -> 'a t
val ( <@ ) : 'a t -> 'a -> 'a t
val init : len:int -> f:(int -> 'a) -> 'a t
val unfold : f:('a -> ('b * 'a) option) -> init:'a -> 'b t
val join : 'a t -> 'a t -> 'a t
val ( >< ) : 'a t -> 'a t -> 'a t
val join_with : 'a t -> 'a -> 'a t -> 'a t
val lview : 'a t -> ('a * 'a t) option
val rview : 'a t -> ('a t * 'a) option
val fold_right : f:('a -> 'b -> 'b) -> 'a t -> init:'b -> 'b
val fold_left : f:('b -> 'a -> 'b) -> init:'b -> 'a t -> 'b
val fold : f:('b -> 'a -> 'b) -> init:'b -> 'a t -> 'b
val iter : f:('a -> unit) -> 'a t -> unit
val map : f:('a -> 'b) -> 'a t -> 'b t
val filter : f:('a -> bool) -> 'a t -> 'a t
val concat_map : f:('a -> 'b t) -> 'a t -> 'b t
val lview_lazy : 'a t -> ('a * 'a t lazy_t) option
val rview_lazy : 'a t -> ('a t lazy_t * 'a) option
val hd_left_exn : 'a t -> 'a
val tl_left : 'a t -> 'a t
val hd_right_exn : 'a t -> 'a
val tl_right : 'a t -> 'a t
val partition_lazy :
  p:(int -> bool) -> 'a t -> 'a t lazy_t * 'a * 'a t lazy_t
val partition : p:(int -> bool) -> 'a t -> 'a t * 'a * 'a t
val of_list : 'a list -> 'a t
val to_list : 'a t -> 'a list
val of_seq : 'a Stdlib.Seq.t -> 'a t
val to_seq : 'a t -> 'a Stdlib.Seq.t
val rev_to_seq : 'a t -> 'a Stdlib.Seq.t
val bounds_check : int -> 'a t -> unit
val partition_unchecked : int -> 'a t -> 'a t * 'a * 'a t
val partition_exn : int -> 'a t -> 'a t * 'a * 'a t
val partition_opt : int -> 'a t -> ('a t * 'a * 'a t) option
val split_unchecked : int -> 'a t -> 'a t lazy_t * 'a t lazy_t
val split_exn : int -> 'a t -> 'a t lazy_t * 'a t lazy_t
val split : int -> 'a t -> ('a t lazy_t * 'a t lazy_t) option
val get_unchecked : int -> 'a t -> 'a
val get_exn : int -> 'a t -> 'a
val get : ('a -> 'b) t -> 'a -> (p:(int -> bool) -> 'b) option
val insert_unchecked : int -> 'a -> 'a t -> 'a t
val insert_exn : int -> 'a -> 'a t -> 'a t
val insert : int -> 'a -> 'a t -> 'a t option
val update_unchecked : int -> 'a -> 'a t -> 'a t
val update_exn : int -> 'a -> 'a t -> 'a t
val update : int -> 'a -> 'a t -> 'a t option
val pop_unchecked : int -> 'a t -> 'a * 'a t
val pop_exn : int -> 'a t -> 'a * 'a t
val pop : int -> 'a t -> ('a * 'a t) option
val remove_unchecked : int -> 'a t -> 'a t
val remove_exn : int -> 'a t -> 'a t
val remove : int -> 'a t -> 'a t option
val msort : cmp:('a -> 'a -> int) -> 'a t -> 'a t
val sort : cmp:('a -> 'a -> int) -> 'a t -> 'a t
val balance : 'a t -> 'a t
val pp : (Format.formatter -> 'a -> unit)
  -> Format.formatter -> 'a t -> unit
    [@@ocaml.toplevel_printer]
val show : (Format.formatter -> 'a -> unit) -> 'a t -> string
val pp_debug : (Format.formatter -> 'a -> unit)
  -> Format.formatter -> 'a t -> unit
val show_debug : (Format.formatter -> 'a -> unit) -> 'a t -> string



module Operators : sig
  val (@>) : 'a -> 'a t -> 'a t
  val (<@) : 'a t -> 'a -> 'a t
  val (><) : 'a t -> 'a t -> 'a t
end

module Monad : sig
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( and* ) : 'a t -> 'b t -> ('a * 'b) t
  val map : f:('a -> 'b) -> 'a t -> 'b t
  val liftA2 : f:('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  val liftA3 : f:('a -> 'b -> 'c -> 'd) -> 'a t -> 'b t -> 'c t -> 'd t
  val apply : ('a -> 'b) t -> 'a t -> 'b t
  val ( <$> ) : ('a -> 'b) -> 'a t -> 'b t
  val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t
  val ( *> ) : unit t -> 'a t -> 'a t
  val ( <* ) : 'a t -> unit t -> 'a t
  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
end
