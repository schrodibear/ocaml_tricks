type ('p, 'f) app = App of 'p * 'f
type nil

module Newtype1 (T : sig type 'a t end) =
struct
  type 'a t = Mk : 'a T.t -> (('a, nil) app) t
  let inj v = Mk v
  let prj (Mk v) = v
end

module Newtype2 (T : sig type ('a, 'b) t end) =
struct
  type 'a t = Mk : ('a, 'b) T.t -> (('a, ('b, nil) app) app) t
  let inj v = Mk v
  let prj (Mk v) = v
end

module type S =
sig
  type 'a t
end

module Performer (M : S) =
struct
  open M
  type task = Task : 'a t * ('a t -> 'b t) * ('b t -> 'c t) * ('c t -> 'a t) * ('a t -> string) -> task
  let perform =
    function
    | Task  (v, m1, m2, m3, s) ->
      let s = v |> m1 |> m2 |> m3 |> s in
      prerr_endline s
end

module Pair = Newtype2 (struct type ('a, 'b) t = 'a * 'b end)
module Pair_performer = Performer (Pair)

let mappers =
  (fun (x, y) -> int_of_string x, int_of_string y),
  (fun (x, y) -> float_of_int x, float_of_int y),
  (fun (x, y) -> string_of_float x, string_of_float y)

let pair_task =
  let open Pair in
  let open Pair_performer in
  let m1, m2, m3 = mappers in
  let wrap m x = x |> prj |> m |> inj in
  let m1, m2, m3 = wrap m1, wrap m2, wrap m3 in
  [|Task (inj ("1", "2"), m1, m2, m3, fun x -> let a, b = prj x in a ^ ", " ^ b);
    Task (inj (1, 2), m2, m3, m1, fun x -> let a, b = prj x in string_of_int a ^ ", " ^ string_of_int b)|]

module Singleton = Newtype1 (struct type 'a t = 'a end)
module Single_performer = Performer (Singleton)

let mappers = int_of_string, float_of_int, string_of_float

let singleton_task =
  let open Singleton in
  let open Single_performer in
  let m1, m2, m3 = mappers in
  let wrap m x = x |> prj |> m |> inj in
  let m1, m2, m3 = wrap m1, wrap m2, wrap m3 in
  [|Task (inj (1), m2, m3, m1, fun x -> "Just " ^ string_of_int @@ prj x);
    Task (inj ("1"), m1, m2, m3, fun x -> "Just " ^ prj x)|]

let () = Pair_performer.perform pair_task.(0)
let () = Single_performer.perform singleton_task.(1)

let () = Pair_performer.perform pair_task.(1)
