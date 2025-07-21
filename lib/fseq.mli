(** {!Fseq.t} is a functional finite sequence.

    It is general purpose sequence and excels for several tasks:

    - As a deque, it has amortized O(1) access time to both ends of
      the sequence.
    - As a functional alternative to arrays, it provides O(log(n))
      random access to elements, and provides better cache locality
      than a list (though this really only matters with unboxed
      values). Obviously an array is faster for these things, but the
      sequence is purely functional.
    - Additionally, the sequence provides O(log(n)) time for split and
      join operations, a characteristic non-tree containers cannot
      duplicate. This makes it a great option for highly concatenative
      operations like {!concat_map} and {!Monad} operations.
    - Because of the greater structural complexity, the sequence is
      not as good of a stack as a basic linked list. However, it might
      might be better in some scenarios because getting the length is
      O(1), so, for example, it might be better for incrementally
      building up and converting to an array---though you may want to
      benchmark to see if the standard library [Dynarray] is more
      suitable in your usecase.

    As always, when performance matters, profile your code and seek an
    alternative if [Fseq] proves to be too slow for your usecase. If
    your usecase involves some combination of, deque, random access,
    join/split or stack operations, you'll be hard-pressed to find a
    better data structure than the sequence.

    As a functional data structure, operations which modify the
    content of a sequence create a new sequence as output, leaving the
    input sequence unchanged.

    {!Fseq.t} is based on the random access sequence presented in
    {{:https://www.staff.city.ac.uk/~ross/papers/FingerTree.html}
    Finger Trees: A Simple General-purpose Data Structure} (2006 Hinze
    and Paterson, {i Journal of Functional Programming}). It
    implements most of the optimizations suggested in the paper,
    including laziness in the computation of the spine to preserve
    amortized O(1) bounds for accessing the ends of the sequence and a
    sepcialized digit implementation which is faster than the default
    list-based approach (and keeps the implementation from manually
    maintaining invariants). It does not, however, implement index
    operations by subtraction as suggested in the paper but instead
    uses the default additive approach because it is based on a module
    functor which can be used to implement some of the other data
    structures in the paper.

    Laziness is less efficient in OCaml than Haskell, but the finger
    tree is still a good case for it because the number of suspensions
    has a logrithmic relationship to the size of the tree, so the
    presence of laziness adds relatively little overhead.

    While laziness is part of the implementation of the finger tree,
    it is not non-strict in the same way as [Seq.t] in the OCaml
    standard library or a Haskell list. {!Fseq.t} should be treated as
    a strict. Far from being useful for saving memory, the sequence
    requires more space than arrays or linked lists to contain the
    same number of elements due to the internal tree structure.

    It is the same data structure as Haskell's
    {{:https://hackage-content.haskell.org/package/containers-0.8/docs/Data-Sequence.html}
    Data.Sequence}.

    {2 Usage as deque}

    You can add values to the left (or front) of the sequence with
    {!ladd} and to the right (or back) with {!radd}.

    {ocaml[
    # module Fseq = La_fingertree.Fseq;;
    
    # let one_value = Fseq.ladd "foo" Fseq.empty;;
    val one_value : string Fseq.t = Fseq.of_list ["foo"]

    # let two_values = ladd "bar" one_value;;
    val two_values : string Fseq.t = Fseq.of_list ["bar"; "foo"]

    # Fseq.radd two_values "baz";;
    - : string Fseq.t = Fseq.of_list ["bar"; "foo"; "baz"]
    ]}

    Alternatively, [@<] and [>@] are provided for the same purpose.
    
    {ocaml[
    # open Fseq.Operators;;

    # let fs = "foo" @< Fseq.empty;;
    val fs : string Fseq.t = Fseq.of_list ["foo"]

    # fs >@ "bar";;
    - : string Fseq.t = Fseq.of_list ["foo"; "bar"]

    # "foo" @< "bar" @< Fseq.empty >@ "baz" >@ "qux";;
    - : string Fseq.t = Fseq.of_list ["foo"; "bar"; "baz"; "qux"]
    ]}

    You will notice that the included pretty printer shows how an
    equivalent sequence would be constructed from a list. This is to
    keep it as concise as possible. The actual structure of the
    sequence can be show with additional included pretty printer,
    {!pp_debug} and the related string function {!show_debug}.

    To "pop" elements from the left and right sides of the list, use
    {!lview} and {!rview} respectively. If the sequence is empty, they
    return [None] and if not, they return [Some] of a pair consiting
    of the end element and the remaining sequence.

    {ocaml[
    # let fs = Fseq.init ~len:5 ~f:Fun.id;;
    val fs : int Fseq.t = Fseq.of_list [0; 1; 2; 3; 4]

    # Fseq.lview fs;;
    - : (int * int Fseq.t) option =
    Some (0, Fseq.of_list [1; 2; 3; 4])

    # Fseq.rview fs;;
    - : (int Fseq.t * int) option =
    Some (Fseq.of_list [0; 1; 2; 3], 4)
    ]}

    If you only care about the end and do not wish to compute the
    remaining sequence (not expensive, but not free either), there are
    two ways to avoid this:

    {ocaml[
    # Fseq.lview_lazy fs;;
    - : (int * int Fseq.t lazy_t) option = Some (0, <lazy>)

    # Fseq.hd_left_exn fs;;
    - : int = 0
    ]}

    {!lview_lazy} and {!rview_lazy} correspond to [lview] and [rview],
    but they are lazy in the computation of the tail (i.e. the
    remaining sequence). {!hd_left_exn} and {!hd_right_exn} retrieve
    only the end element and throw [Invalid_argument] if the sequence
    is empty.

    Rotation is also provided:

    {ocaml[
    # Fseq.rotate 2 fs;;
    - : int Fseq.t = Fseq.of_list [2; 3; 4; 0; 1]

    # Fseq.rotate (-1) fs;;
    - : int Fseq.t = Fseq.of_list [4; 0; 1; 2; 3]
    ]}

    Unlike the rotation of a doubly-linked list, where performance is
    proportional to the size of the rotation, rotating a sequence
    always preforms exactly one {!split} and one {!join}.

    {2 Usage as a random-access sequence}
*)

type 'a t

val empty : 'a t
val is_empty : 'a t -> bool
val length : 'a t -> int
val singleton : 'a -> 'a t
val ladd : 'a -> 'a t -> 'a t
val ( @< ) : 'a -> 'a t -> 'a t
val radd : 'a t -> 'a -> 'a t
val ( >@ ) : 'a t -> 'a -> 'a t
val init : len:int -> f:(int -> 'a) -> 'a t
val unfold : f:('a -> ('b * 'a) option) -> init:'a -> 'b t
val join : 'a t -> 'a t -> 'a t
val ( >< ) : 'a t -> 'a t -> 'a t
val join_with : 'a t -> 'a -> 'a t -> 'a t
val insert : int -> 'a -> 'a t -> 'a t option
val set : int -> 'a -> 'a t -> 'a t option
val balance : 'a t -> 'a t
val msort : cmp:('a -> 'a -> int) -> 'a t -> 'a t
val sort : cmp:('a -> 'a -> int) -> 'a t -> 'a t
val lview : 'a t -> ('a * 'a t) option
val lview_lazy : 'a t -> ('a * 'a t Lazy.t) option
val rview : 'a t -> ('a t * 'a) option
val rview_lazy : 'a t -> ('a t Lazy.t * 'a) option
val tl_left : 'a t -> 'a t
val tl_right : 'a t -> 'a t
val get : ('a -> 'b) t -> 'a -> (p:(int -> bool) -> 'b) option
val partition : int -> 'a t -> ('a t * 'a * 'a t) option
val partition_lazy : int -> 'a t -> ('a t Lazy.t * 'a * 'a t Lazy.t) option
val split : int -> 'a t -> 'a t * 'a t
val split_lazy : int -> 'a t -> 'a t Lazy.t * 'a t Lazy.t
val rotate : int -> 'a t -> 'a t
val slice : start:int -> stop:int -> 'a t -> 'a t
val pop : int -> 'a t -> ('a * 'a t) option
val remove : int -> 'a t -> 'a t option
val fold_right : f:('a -> 'b -> 'b) -> 'a t -> init:'b -> 'b
val fold_left : f:('b -> 'a -> 'b) -> init:'b -> 'a t -> 'b
val fold : f:('b -> 'a -> 'b) -> init:'b -> 'a t -> 'b
val iter : f:('a -> unit) -> 'a t -> unit
val map : f:('a -> 'b) -> 'a t -> 'b t
val filter : f:('a -> bool) -> 'a t -> 'a t
val concat_map : f:('a -> 'b t) -> 'a t -> 'b t
val of_list : 'a list -> 'a t
val to_list : 'a t -> 'a list
val of_seq : 'a Seq.t -> 'a t
val to_seq : 'a t -> 'a Seq.t
val rev_to_seq : 'a t -> 'a Seq.t
val to_array : 'a t -> 'a array

val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
[@@ocaml.toplevel_printer]

val show : (Format.formatter -> 'a -> unit) -> 'a t -> string

val pp_debug :
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit

val show_debug : (Format.formatter -> 'a -> unit) -> 'a t -> string
val set_exn : int -> 'a -> 'a t -> 'a t
val insert_exn : int -> 'a -> 'a t -> 'a t
val hd_left_exn : 'a t -> 'a
val hd_right_exn : 'a t -> 'a
val get_exn : int -> 'a t -> 'a
val partition_exn : int -> 'a t -> 'a t * 'a * 'a t
val partition_lazy_exn : int -> 'a t -> 'a t Lazy.t * 'a * 'a t Lazy.t
val pop_exn : int -> 'a t -> 'a * 'a t
val remove_exn : int -> 'a t -> 'a t
val set_unchecked : int -> 'a -> 'a t -> 'a t
val insert_unchecked : int -> 'a -> 'a t -> 'a t
val get_unchecked : int -> 'a t -> 'a
val partition_unchecked : int -> 'a t -> 'a t * 'a * 'a t
val partition_lazy_unchecked : int -> 'a t -> 'a t Lazy.t * 'a * 'a t Lazy.t
val pop_unchecked : int -> 'a t -> 'a * 'a t
val remove_unchecked : int -> 'a t -> 'a t

module Operators : sig
  val ( @< ) : 'a -> 'a t -> 'a t
  val ( >@ ) : 'a t -> 'a -> 'a t
  val ( >< ) : 'a t -> 'a t -> 'a t
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
