type 'a wit = ..

module type T = sig type u end

module type Witness = sig type u  type 'a wit += Wit : u wit end

let make_witness : type a. (module T with type u = a) -> (module Witness with type u = a) =
  let module Make_witness (T : T) : Witness with type u = T.u = struct
    type u = T.u
    type x = u
    type 'a wit +=
      | Wit : T.u wit
  end in
  fun (module T) ->
    (module Make_witness (T))

module Container : sig
  type 'a obj = < hello : string; .. > as 'a
  type 'a wit = (module Witness with type u = 'a)
  type some_obj = Obj : ('a wit * 'a obj) -> some_obj [@@unboxed]
  val register_obj : string -> 'a wit -> 'a obj -> unit
  val get_obj : string -> some_obj
  val get_and_downcast_exn : string -> 'a wit -> 'a obj
end = struct
  type 'a obj = < hello : string; .. > as 'a
  type 'a wit = (module Witness with type u = 'a)
  type some_obj = Obj : ('a wit * 'a obj) -> some_obj [@@unboxed]
  let objs = ref []
  let register_obj name wit obj = objs := (name, Obj (wit, obj)) :: !objs
  let get_obj name = List.assoc name !objs
  let get_and_downcast_exn (type a) :  string -> a wit -> a =
    fun name (module W) ->
    let Obj ((module W'), o) = get_obj name in
    match W'.Wit with
    | W.Wit -> o
    | _ -> failwith "Dynamic typing error"
end


class o = object(self) method hello = "Hello! I'm " ^ self#name ^ "!" method private name = "O" end
class a = object inherit o method! private name = "A" method a = 5 end
class b = object inherit a  method! private name = "B" method! a = 55 method b = 0.5 end

let do_it =
  let o = new o in
  let a = new a in
  let b = new b in
  let (module O) as o_wit = make_witness (module (struct type u = o end)) in
  let (module A) as a_wit = make_witness (module (struct type u = a end)) in
  let (module B) as b_wit = make_witness (module (struct type u = b end)) in
  let open Container in
  register_obj "o" o_wit o;
  register_obj "a" a_wit a;
  register_obj "b" b_wit b;
  register_obj "b_as_a" a_wit (b :> a);
  let open Printf in
  printf "Retreiving `o' as o: hello? -- %s\n" (get_and_downcast_exn "o" o_wit)#hello;
  let a' = get_and_downcast_exn "a" a_wit in
  printf "Retreiving `a' as a: hello? -- %s a? -- a=%d\n" a'#hello a'#a;
  let b' = get_and_downcast_exn "b" b_wit in
  printf "Retreiving `b' as b: hello? -- %s a? -- a=%d b? -- b=%f\n" b'#hello b'#a b'#b;
  let a'' = get_and_downcast_exn "b_as_a" a_wit in
  printf "Retreiving `b_as_a' as a: hello? -- %s a? -- a=%d Can't get b, the precise type is lost!\n" a''#hello a''#a;
  printf "Now we don't have a witness!\n";
  let Obj ((module W), x) = get_obj "b_as_a" in
  let o : o =
    match W.Wit with
    | O.Wit -> x
    | A.Wit -> (x :> o)
    | B.Wit -> (x :> o)
    | _ -> failwith "Unrecognized type of object"
  in
  printf "Still can extract at least `hello' by explicit enumeration of possible witnesses! -- %s\n" o#hello;
  printf "Will try to retrieve `a' as o (subtyping is not supported directly!)!\n";
  let _ = get_and_downcast_exn "a" o_wit in
  ()
