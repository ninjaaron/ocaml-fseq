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

let partition_exn i t =
  bounds_check i t;
  partition ~p:((<) i) t
  
let partition_opt i t =
  match partition_exn i t with
  | exception _ -> None
  | value -> Some value

let split_exn i t =
  bounds_check i t;
  Some (split ~p:((<) i) t)

let split i t =
  match split_exn i t with
  | exception _ -> None
  | v -> Some v

let get_exn i t =
  bounds_check i t;
  get ~p:((<) i) t

let get i t =
  match get i t with
  | exception _ -> None
  | value -> Some value

let insert_unchecked i el t =
  insert ~p:((<) i) el t
  
let insert_exn i el t =
  bounds_check i t;
  insert ~p:((<) i) el t

let insert i el t =
  match insert_exn i el t with
  | exception _ -> None
  | value -> Some value
  
let update_exn i el t =
  bounds_check i t;
  update ~p:((<) i) el t

let update i el t =
  match update_exn i el t with
  | exception _ -> None
  | value -> Some value

let pop_exn i t =
  bounds_check i t;
  pop ~p:((<) i) t

let pop i t =
  match pop_exn i t with
  | exception _ -> None
  | value -> Some value

let remove_exn i t =
  bounds_check i t;
  remove ~p:((<) i) t

let remove i t =
  match remove_exn i t with
  | exception _ -> None
  | value -> Some value
