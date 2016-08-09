module rec F : sig
  type 'a c'
  class ['a] c : 'a -> object
      method get : 'a
      method map : 'b. ('a -> 'b) -> 'b F.c'
    end
  val new_c : 'a -> 'a F.c'
  val self_cast : 'a c' -> 'a c
end = struct
  class ['a] c a = object
    method get = a
    method map : 'b. ('a -> 'b) -> 'b F.c' = fun f -> F.new_c (f a)
  end
  type 'a c' = 'a c
  let new_c a = new F.c a
  let self_cast (x : 'a c') = x
end

let () =
  let c1 = new F.c 1 in
  Printf.eprintf "%d\n" c1#get;
  let c2 = F.self_cast @@ c1#map (string_of_int) in
  Printf.eprintf "%s\n" c2#get
