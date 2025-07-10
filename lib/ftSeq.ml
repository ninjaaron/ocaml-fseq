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

let partition_unchecked i t =
  partition ~p:((<) i) t

let partition i t =
  bounds_check i t;
  partition ~p:((<) i) t
  
let partition_opt i t =
  match partition i t with
  | exception _ -> None
  | value -> Some value

let split i t =
  split ~p:((<) i) t

let get_unchecked i t =
  get ~p:((<) i) t

let get i t =
  bounds_check i t;
  get ~p:((<) i) t

let get_opt i t =
  match get i t with
  | exception _ -> None
  | value -> Some value

let insert_unchecked i el t =
  insert ~p:((<) i) el t
  
let insert i el t =
  bounds_check i t;
  insert ~p:((<) i) el t

let insert_opt i el t =
  match insert i el t with
  | exception _ -> None
  | value -> Some value

let update_unchecked i el t =
  update ~p:((<) i) el t
  
let update i el t =
  bounds_check i t;
  update ~p:((<) i) el t

let update_opt i el t =
  match update i el t with
  | exception _ -> None
  | value -> Some value
