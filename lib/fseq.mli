(** {!Fseq.t} is a functional finite sequence.

    It is general purpose sequence and excels for several tasks:

    - As a deque, it has amortized O(1) access time to both ends of the
      sequence.
    - As a functional alternative to arrays, it provides O(log(n)) random access
      to elements, and provides better cache locality than a list (though this
      really only matters with unboxed values). Obviously an array is faster for
      these things, but the sequence is purely functional.
    - Additionally, the sequence provides O(log(n)) time for split and join
      operations, a characteristic non-tree containers cannot duplicate. This
      makes it a great option for highly concatenative operations like
      {!concat_map} and {!Monad} operations.
    - Because of the greater structural complexity, the sequence is not as good
      of a stack as a basic linked list.

    As always, when performance matters, profile your code and seek an
    alternative if [Fseq] proves to be too slow for your usecase. To me the
    appeal of this container is less that it is optimal for any specific task
    (though it is very good as a functional deque and for concatenation), and
    more that it is a functional data structure that is useful for many things
    and doesn't have catastrophic worst-case characteristics for any of them.

    As a functional data structure, operations which modify the content of a
    sequence create a new sequence as output, leaving the input sequence
    unchanged.

    {!Fseq.t} is based on the random access sequence presented in
    {{:https://www.staff.city.ac.uk/~ross/papers/FingerTree.html} Finger Trees:
     A Simple General-purpose Data Structure} (2006, Hinze and Paterson,
    {i Journal of Functional Programming}). It implements most of the
    optimizations suggested in the paper, including laziness in the computation
    of the spine to preserve amortized O(1) bounds for accessing the ends of the
    sequence and a sepcialized digit implementation which is faster than the
    default list-based approach (and keeps the implementation from manually
    maintaining invariants). It does not, however, implement index operations by
    subtraction as suggested in the paper but instead uses the default additive
    approach because it is based on a module functor which can be used to
    implement some of the other data structures in the paper.

    Laziness is less efficient in OCaml than Haskell, but the finger tree is
    still a good case for it because the number of suspensions has a logrithmic
    relationship to the size of the tree, so the presence of laziness adds
    relatively little overhead.

    While laziness is part of the implementation of the finger tree, it is not
    non-strict in the same way as [Seq.t] in the OCaml standard library or a
    Haskell list. {!Fseq.t} should be treated as a strict. Far from being useful
    for saving memory, the sequence requires more space than arrays or linked
    lists to contain the same number of elements due to the internal tree
    structure.

    It is the same data structure as Haskell's
    {{:https://hackage-content.haskell.org/package/containers-0.8/docs/Data-Sequence.html}
     Data.Sequence}.

    {2 Usage as deque}

    You can add values to the left (or front) of the sequence with {!ladd} and
    to the right (or back) with {!radd}.

    {@ocaml[
      # module Fseq = La_fingertree.Fseq;;

      # let one_value = Fseq.ladd "foo" Fseq.empty;;
      val one_value : string Fseq.t = Fseq<"foo">

      # let two_values = ladd "bar" one_value;;
      val two_values : string Fseq.t = Fseq<"bar"; "foo">

      # Fseq.radd two_values "baz";;
      - : string Fseq.t = Fseq<"bar"; "foo"; "baz">
    ]}

    Alternatively, [@<] and [>@] are provided for the same purpose.

    {@ocaml[
      # open Fseq.Operators;;

      # let fs = "foo" @< Fseq.empty;;
      val fs : string Fseq.t = Fseq<"foo">

      # fs >@ "bar";;
      - : string Fseq.t = Fseq<"foo"; "bar">

      # "foo" @< "bar" @< Fseq.empty >@ "baz" >@ "qux";;
      - : string Fseq.t = Fseq<"foo"; "bar"; "baz"; "qux">
    ]}

    You will notice that the included pretty printer represents the sequence as
    a flat data structure similar to a list or array. This is to keep it as
    concise as possible. The actual tree structure of the sequence can be show
    with additional included pretty printer, {!pp_debug} and the related string
    function {!show_debug}.

    To "pop" elements from the left and right sides of the list, use {!lview}
    and {!rview} respectively. If the sequence is empty, they return [None] and
    if not, they return [Some] of a pair consiting of the end element and the
    remaining sequence.

    {@ocaml[
      # let fs = Fseq.init ~len:5 ~f:Fun.id;;
      val fs : int Fseq.t = Fseq<0; 1; 2; 3; 4>

      # Fseq.lview fs;;
      - : (int * int Fseq.t) option =
      Some (0, Fseq<1; 2; 3; 4>)

      # Fseq.rview fs;;
      - : (int Fseq.t * int) option =
      Some (Fseq<0; 1; 2; 3>, 4)
    ]}

    If you only care about the end and do not wish to compute the remaining
    sequence (not expensive, but not free either), there are two ways to avoid
    this:

    {@ocaml[
      # Fseq.lview_lazy fs;;
      - : (int * int Fseq.t lazy_t) option = Some (0, <lazy>)

      # Fseq.hd_left_exn fs;;
      - : int = 0
    ]}

    {!lview_lazy} and {!rview_lazy} correspond to [lview] and [rview], but they
    are lazy in the computation of the tail (i.e. the remaining sequence).
    {!hd_left_exn} and {!hd_right_exn} retrieve only the end element and throw
    [Invalid_argument] if the sequence is empty.

    Deque rotation (not matrix rotation) is also provided:

    {@ocaml[
      # let fs = Fseq.init ~len:5 ~f:Fun.id;;
      val fs : int Fseq.t = Fseq<0; 1; 2; 3; 4>

      # Fseq.rotate 2 fs;;
      - : int Fseq.t = Fseq<2; 3; 4; 0; 1>

      # Fseq.rotate (-1) fs;;
      - : int Fseq.t = Fseq<4; 0; 1; 2; 3>
    ]}

    Unlike the rotation of a doubly-linked list, where performance is
    proportional to the size of the rotation, rotating a sequence always
    preforms exactly one {!split} and one {!join}.

    {2 Usage as a random-access sequence (i.e. a functional alternative to
    arrays)}

    For basic usage {!Fseq.t} provides {!get} and {!set} operations. These
    operations return [None] if the index is out of bounds.

    {@ocaml[
      # let fs = Fseq.of_list ["OCaml"; "Haskell"; "Clojure"; "F#"];;
      val fs : string Fseq.t = Fseq<"OCaml"; "Haskell"; "Clojure"; "F#">

      # Fseq.get 2 fs;;
      - : string option = Some "Clojure"

      # Fseq.set 2 "Scheme" fs;;
      - : string Fseq.t option =
      Some Fseq<"OCaml"; "Haskell"; "Scheme"; "F#">
    ]}

    Note that {!set} is not like [Array.set] in that it does not do in-place
    modification, but produces a new sequence with the updated value.

    If you prefer an [Invalid_argument] exception instead of an option,
    {!get_exn} and {!set_exn} are available.

    Likewise, if you know your index is in bounds, checks may be omitted with
    {!get_unchecked} and {!set_unchecked}. The bounds check quite cheap relative
    to the cost of the actual lookup, so there is generally little reason to
    leave it out. Unchecked operations in this library are not unsafe in the
    same way as unsafe array indexing and will never access other regions of
    memory, but they probably also don't do what you want. Indexing beyond the
    end of a sequence will return its last element, while giving a negative
    index will access the first element of the sequence. Therefore, bounds
    checks in this case are not for safety, but to alert you that you probably
    did something you did not intend to do.

    Some random-access operations which do not exist for arrays (at least in
    OCaml) are also provided.

    {@ocaml[
      # Fseq.pop 2 fs;;
      - : (string * string Fseq.t) option =
      Some ("Clojure", Fseq<"OCaml"; "Haskell"; "F#">)

      # Fseq.insert 3 "ReScript" fs;;
      - : string Fseq.t option =
      Some Fseq<"OCaml"; "Haskell"; "Clojure"; "ReScript"; "F#">

      # Fseq.remove 1 fs;;
      - : string Fseq.t = Fseq<"OCaml"; "Clojure"; "F#">
    ]}

    {!pop}, {!insert} and {!remove} also have their accompanying [_exn] and
    [_unchecked] flavors. Recall once again that these operations do not modify
    the sequence in place, but always produce a new sequence. Note that
    {!remove} with an out-of-bounds index returns the same sequence.
    {!remove_exn} raises if the index is out of bounds. {!remove_unchecked} will
    remove {i something}, but not at the specified index. (the first or last
    element, depending whether the index is below or above the bounds,
    respectively)

    Slicing is also implemented.

    Slices are more of a split operation and properly belong to the section on
    splits and joins, but I include it here because it is an operation
    frequently associated with arrays.

    {@ocaml[
      # let fs = Fseq.init ~len:10 ~f:Fun.id;;
      val fs : int Fseq.t = Fseq<0; 1; 2; 3; 4; 5; 6; 7; 8; 9>

      # Fseq.slice ~start:2 ~stop:7 fs;;
      - : int Fseq.t = Fseq<2; 3; 4; 5; 6>
    ]}

    Slicing does not involve a bounds check. Slicing out of bounds creates an
    empty sequence. Slicing where only one part of the slice is out of bounds
    will include as many elements as available.

    {@ocaml[
      # Fseq.slice ~start:20 ~stop:30 fs;;
      - : int Fseq.t = Fseq<>
      # Fseq.slice ~start:5 ~stop:30 fs;;
      - : int Fseq.t = Fseq<5; 6; 7; 8; 9>
    ]}

    {2 Splits and Joins}

    {!Fseq.t} is cheap to split and join, with the operations costing O(log(n)),
    similarly to random-access operations.

    {@ocmal[
      # let fs = Fseq.init ~len:10 ~f:Fun.id;;
      val fs : int Fseq.t = Fseq<0; 1; 2; 3; 4; 5; 6; 7; 8; 9>

      # let l, r = Fseq.split 5 fs;;
      val l : int Fseq.t = Fseq<0; 1; 2; 3; 4>
      val r : int Fseq.t = Fseq<5; 6; 7; 8; 9>

      # Fseq.join r l;;
      - : int Fseq.t = Fseq<5; 6; 7; 8; 9; 0; 1; 2; 3; 4>

      # (* there is also a join operator *)
      # r >< l;;
      - : int Fseq.t = Fseq<5; 6; 7; 8; 9; 0; 1; 2; 3; 4>
    ]}

    That's about all there is to join and split. As with slices, split will
    return one of the sequences empty if the index is out of bounds--because
    slicing is implemented in terms of splits.

    If you only want one half of the sequence, there are take and drop.

    {@ocaml[
      # Fseq.take 2 fs;;
      - : int Fseq.t = Fseq<0; 1>

      # Fseq.drop 2 fs;;
      - : int Fseq.t = Fseq<2; 3; 4; 5; 6; 7; 8; 9>
    ]}

    There are also {!partition} and {!join_with} functions. {!partition} gives
    the element at the index in addition to splitting. {!join_with} does the
    opposite.

    {@ocaml[
      # let Some (l, n, r) =Fseq.partition 7 fs;;
      val l : int Fseq.t = Fseq<0; 1; 2; 3; 4; 5; 6>
      val n : int = 7
      val r : int Fseq.t = Fseq<8; 9>

      # Fseq.join_with r n l;;
      - : int Fseq.t = Fseq<8; 9; 7; 0; 1; 2; 3; 4; 5; 6>
    ]}

    As with other indexing operations, {!partition} returns an option, and like
    other index operations, there are [_exn] and [_unchecked] variants. There is
    also a {!partition_lazy} where both sides of the sequence are suspended, and
    this function also has [_exn] and [_unchecked].

    {2 General iteration operations}

    The {!Fseq.t} also has a modest complement of general sequence functions.

    This includes {!fold_right}, {!fold_left}, {!iter}, {!map}, {!filter},
    {!concat_map}, {!sort}, {!msort}, {!of_list}, {!to_list}, {!of_seq},
    {!to_seq}, {!rev_to_seq} and {!to_array}.

    There is also a {!Monad} module with both monadic and applicative
    operations. It works like a list monad, if you're familar witht that (i.e.
    it's computationally eqivalent to list comprehensions).

    Specifics are detailed in the API documentation. Speaking of...

    {2 API Reference} *)

type 'a t

val empty : 'a t
(** The empty sequence *)

val is_empty : 'a t -> bool

val length : 'a t -> int
(** {!Fseq.t} keeps track of length, so this operation is O(1) *)

(** {3 Construction operations} *)

val singleton : 'a -> 'a t
(** Take a value as input and return a one-element sequence containing that
    value *)

val ladd : 'a -> 'a t -> 'a t
(** Add an element to the left side of a sequence. Like [cons] with a list. *)

val ( @< ) : 'a -> 'a t -> 'a t
(** operator version of {!ladd}. Right associative. *)

val radd : 'a t -> 'a -> 'a t
(** Add an element to the right side of a sequence. Like [snoc] in that "other"
    functional language. Performance is O(1). *)

val ( >@ ) : 'a t -> 'a -> 'a t
(** operator version of {!radd}. Left associative. *)

val init : len:int -> f:(int -> 'a) -> 'a t
(** Initialize a new sequence of length [len] with the result of [f] for each
    index. *)

val unfold : f:('a -> ('b * 'a) option) -> init:'a -> 'b t
(** Unfold a sequence from an initial state. The initial state is passed to [f].
    [f] produces either [None], in which case the the sequence is finished, or
    [Some] of a pair of an element and the next state to passed to [f]. *)

val join : 'a t -> 'a t -> 'a t
(** join two sequences. Probably should be called "append". *)

val ( >< ) : 'a t -> 'a t -> 'a t
(** Operator version of join. Left associative. *)

val join_with : 'a t -> 'a -> 'a t -> 'a t
(** Join two sequences with an element in the middle. *)

val insert : int -> 'a -> 'a t -> 'a t option
(** Insert an element at the given index. *)

(** {3 Restructuring operations} *)

val set : int -> 'a -> 'a t -> 'a t option
(** Change the element at the given index. *)

val msort : cmp:('a -> 'a -> int) -> 'a t -> 'a t
(** Merge sort with stable sort time. *)

val sort : cmp:('a -> 'a -> int) -> 'a t -> 'a t
(** Sorting algorithm loosely based on quicksort, but it's not in-place. Don't
    expect miracles. Generally faster than merge sort, but worse in the worst
    case. *)

val rotate : int -> 'a t -> 'a t
(** Deque rotation (not matrix rotation). A positive integer rotates towards the
    left. A negative integer rotates towards the right. Over-rotation is
    permitted. *)

(** {3 Destructuring operations} *)

val lview : 'a t -> ('a * 'a t) option
(** "Uncons" on the left side of the sequence. Returns [None] on an empty
    seqnece and [Some] of a pair of an element and the remaining sequence. *)

val lview_lazy : 'a t -> ('a * 'a t Lazy.t) option
(** Same as {!lview}, but lazy in the tail *)

val rview : 'a t -> ('a t * 'a) option
(** Uncons from the right side of the sequence (or "unsnoc", if you like).
    Returns [None] on an empty sequence and [Some] of a pair of an element and
    the remaining sequence. *)

val rview_lazy : 'a t -> ('a t Lazy.t * 'a) option
(** Same as {!rview}, but lazy in the tail *)

val tl_left : 'a t -> 'a t
(** Remomve the left-most element and return the rest of the sequence. An empty
    input returns empty. *)

val tl_right : 'a t -> 'a t
(** Remomve the right-most element and return the rest of the sequence. An empty
    input returns empty. *)

val get : int -> 'a t -> 'a option
(** Retrieve the element at the given index. Returns [None] if the index is out
    of bounds and [Some] of the element otherwise. *)

val partition : int -> 'a t -> ('a t * 'a * 'a t) option
(** Split the sequence on the index and give the element at that index. Returns
    [None] if the index is out of bounds or the sequence is empty and [Some] of
    a triple of left side, element, right side otherwise. *)

val partition_lazy : int -> 'a t -> ('a t Lazy.t * 'a * 'a t Lazy.t) option
(** Same as {!partition}, but lazy in the left and right sides. *)

val split : int -> 'a t -> 'a t * 'a t
(** Split the sequence at the given index. If the index is out of bounds, one of
    the sides will be empty. Empty input will produce both sides empty. *)

val take : int -> 'a t -> 'a t
(** Take elements up to the given index. *)

val drop : int -> 'a t -> 'a t
(** Drop elements up to the given index. *)

val slice : start:int -> stop:int -> 'a t -> 'a t
(** Return elements between the start and stop indices. *)

val pop : int -> 'a t -> ('a * 'a t) option
(** If the given index is out of range, return [None]. Otherwise, remove the
    element from the sequence and return [Some] of the element and the remaining
    sequence. *)

val remove : int -> 'a t -> 'a t
(** Remove the element at the given index. If the index is out of range, the
    sequence is unchanged. *)

(** {3 Iterative operations}

    I won't take much time to explain these, since I assume most OCaml
    programmers are familiar with them. If not, check out standard library
    documentation for [List] or [Array] or [Seq] which all have all of these.

    There are not many of these functions because we have [Seq] in the standard
    library, and you can simply wrap {!Fseq.t} with {!to_seq} to use a huge
    number of them. *)

val fold_right : f:('a -> 'b -> 'b) -> 'a t -> init:'b -> 'b
(** Note that, while right fold is somewhat inadvisable with regular OCaml lists
    because it is not tail recursive, [Fseq.fold_right] doesn't have this
    problem. That is, it's not tail recursive (nor are any of these iteration
    functions), but it's tree traversal so the stack depth is much smaller. You
    would likely exhaust your RAM before the stack limit is reached. *)

val fold_left : f:('b -> 'a -> 'b) -> init:'b -> 'a t -> 'b

val fold : f:('b -> 'a -> 'b) -> init:'b -> 'a t -> 'b
(** An alias for fold_left because I am lazy about typing. *)

val iter : f:('a -> unit) -> 'a t -> unit

val map : f:('a -> 'b) -> 'a t -> 'b t
(** Note well that {b [map] does not operated over elements in order}. The
    output is in order, but mapping over the spine is suspended. If you have
    side effects in [f], they {i will} happen out of order, starting at the ends
    of the sequence and working inward.

    The reason for this is that spine of the finger tree is always lazy. It
    would be simple to write [map] in a way that all applications of [f] occur
    in the order of elements in the sequence, but I found it cool to allow this
    computation to be suspended. If there is "public outcry" about this
    behavior, I'll change it. *)

val filter : f:('a -> bool) -> 'a t -> 'a t
val filter_map : f:('a -> 'b option) -> 'a t -> 'b t
val concat_map : f:('a -> 'b t) -> 'a t -> 'b t

val of_list : 'a list -> 'a t
(** {3 Converstion operations} *)

val to_list : 'a t -> 'a list
val of_seq : 'a Seq.t -> 'a t
val to_seq : 'a t -> 'a Seq.t

val rev_to_seq : 'a t -> 'a Seq.t
(** Creates an instance of [Seq.t] which iterates over sequence elements from
    right to left *)

val to_array : 'a t -> 'a array
(** I included {!to_array} because because the implementation is not a
    one-liner, and a sequence is a nice for coversion to an array because
    getting the length is O(1).

    There is no [of_array]. It's a one-liner if you need it:

    {@ocaml[
      let of_array a = Array.fold_left Fseq.radd Fseq.empty a
    ]}

    Conversion from most other types is as simple as this. *)

(** {3 Pretty printing} *)

val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
[@@ocaml.toplevel_printer]
(** Displays {!Fseq.t} as a flat sequence. For use with the print functions in
    the [Format] library. *)

val show : (Format.formatter -> 'a -> unit) -> 'a t -> string
(** Same as {!pp} but returns a string instead of printing *)

val pp_debug :
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
(** Displays the structure of the finger tree. Mostly for my own use while
    developing the library, but I made it public in case others are interested
    in the structure.

    For those familiar with the implementation of a finger tree, trees are
    represented between curly braces [{}], fingers are between square brackets
    [[]] and 2-3 nodes are between parentheses [()]. *)

val show_debug : (Format.formatter -> 'a -> unit) -> 'a t -> string
(** Same as {!pp_debug} but returns a string instead of printing *)

(** {3 Operations which throw exceptions for out-of-bounds lookups} *)

val set_exn : int -> 'a -> 'a t -> 'a t
val insert_exn : int -> 'a -> 'a t -> 'a t
val hd_left_exn : 'a t -> 'a
val hd_right_exn : 'a t -> 'a
val get_exn : int -> 'a t -> 'a
val partition_exn : int -> 'a t -> 'a t * 'a * 'a t
val partition_lazy_exn : int -> 'a t -> 'a t Lazy.t * 'a * 'a t Lazy.t
val pop_exn : int -> 'a t -> 'a * 'a t
val remove_exn : int -> 'a t -> 'a t

(** {3 Operations which don't do bounds checks} *)

val set_unchecked : int -> 'a -> 'a t -> 'a t
val insert_unchecked : int -> 'a -> 'a t -> 'a t
val get_unchecked : int -> 'a t -> 'a
val partition_unchecked : int -> 'a t -> 'a t * 'a * 'a t
val partition_lazy_unchecked : int -> 'a t -> 'a t Lazy.t * 'a * 'a t Lazy.t
val pop_unchecked : int -> 'a t -> 'a * 'a t
val remove_unchecked : int -> 'a t -> 'a t

(** {3 Modules} *)

(** A module with only the operators so you can open it without poluting your
    namespace *)
module Operators : sig
  val ( @< ) : 'a -> 'a t -> 'a t
  val ( >@ ) : 'a t -> 'a -> 'a t
  val ( >< ) : 'a t -> 'a t -> 'a t
end

(** A module with monad and applicative operations based on concat_map. It is
    computationally equivalent to list comprehensions in other languages. *)
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
