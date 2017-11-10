
type _ wit = ..

type (_, _, 'e) app = ..

let (%) f g x = f (g x)

module Freer : sig
  type ('a, 'e, 'h) t = private
    | Pure of 'a
    | Impure : 't wit * ('b, 't, 'e) app * ('b -> ('a, 'e, 'h) t) -> ('a, 'e, 'h) t

  val return : 'a -> ('a, [>], [>]) t

  val lift : 't wit -> ('a, 't, 'e) app -> ('a , 'e, [>]) t

  val bind : ('a, [>] as 'e, [>] as 'h) t -> ('a -> ('b, 'e, 'h) t) -> ('b, 'e, 'h) t

  val (>>=) : ('a, [>] as 'e, [>] as 'h) t -> ('a -> ('b, 'e, 'h) t) -> ('b, 'e, 'h) t

  module type Wit = sig type w type _ wit += W : w wit end

  type 't wit = (module Wit with type w = 't)

  type ('t, 'e, 'h) inj = { f : 'b. ('b, 't, 'e) app -> ('b, 't, 'h) app}

  type ('t, 'h, 'r) hdl = { f : 'b. ('b, 't, 'h) app -> ('b -> 'r) -> 'r }

  val handle : wit: 't wit
    -> inj: ('t, 'e, 'h) inj
    -> hdl: ('t, 'h, 'r) hdl
    -> dlg: (('r, 'e, 'h) t -> 'r)
    -> cont: ('a -> 'r)
    -> ('a, [>] as 'e, [>] as 'h) t -> 'r

  val handle0 : wit: 't wit
    -> inj: ('t, 'e, 'h) inj
    -> hdl: ('t, 'h, ('a, 'e, 'h) t) hdl
    -> ('a, [>] as 'e, [>] as 'h) t -> ('a, 'e, 'h) t
end = struct

  type ('a, 'e, 'h) t =
    | Pure of 'a
    | Impure : 't wit * ('b, 't, 'e) app * ('b -> ('a, 'e, 'h) t) -> ('a, 'e, 'h) t

  let return x = Pure x

  let lift w m = Impure (w, m, return)

  let rec bind m f =
    match m with
    | Pure x            -> f x
    | Impure (w, m, f') -> Impure (w, m, fun x -> bind (f' x) f)

  let (>>=) = bind

  module type Wit = sig type w type _ wit += W : w wit end

  type 't wit = (module Wit with type w = 't)

  type ('t, 'e, 'h) inj = { f : 'b. ('b, 't, 'e) app -> ('b, 't, 'h) app}

  type ('t, 'h, 'r) hdl = { f : 'b. ('b, 't, 'h) app -> ('b -> 'r) -> 'r }

  let rec handle :
    type m. wit: m wit -> inj: (m, _, _) inj -> hdl: (m, _, _) hdl -> dlg: _ -> cont: _ -> _ =
    fun ~wit:((module W) as wit) ~inj ~hdl ~dlg ~cont ->
      let handle m = handle ~wit ~inj ~hdl ~dlg ~cont m in
      function
      | Impure (W.W, m, f) -> hdl.f (inj.f m) (handle % f)
      | Impure (w, m, f)   -> dlg @@ Impure (w, m, fun x -> Pure (handle @@ f x))
      | Pure x             -> cont x

  let rec handle0 :
    type m. wit: m wit -> inj: (m, _, _) inj -> hdl: (m, _, _) hdl -> _ =
    fun ~wit:((module W) as wit) ~inj ~hdl ->
      let handle0 m = handle0 ~wit ~inj ~hdl m in
      function
      | Impure (W.W, m, f) -> hdl.f (inj.f m) (handle0 % f)
      | Impure (w, m, f)   -> Impure (w, m, fun x -> handle0 @@ f x)
      | Pure _ as m        -> m
end

module type T1 = sig type 'a t end

module Type1 (T : T1) () = struct
  module Wit = struct
    type w = W
    type _ wit += W : w wit
  end

  include Wit

  let wit : w Freer.wit = (module Wit)

  type 'a t = 'a T.t

  type ('a, 'w, 'e) app += App : 'a t -> ('a, w, [>]) app

  let inj0 o = App o

  let prj =
    function
    | App o -> o
    | _     -> assert false

  let inj : _ Freer.inj = { Freer.f = fun x -> inj0 @@ prj x }

  let lift x = Freer.lift W @@ inj0 x
end

module Option : sig
  type 'a t = 'a option

  type w

  val wit : w Freer.wit
  val inj0 : 'a t -> ('a, w, [> `Option]) app
  val prj : ('a, w, [> `Option]) app -> 'a t
  val inj : (w, [>], [> `Option]) Freer.inj

  val return : 'a -> ('a, [> `Option], [>]) Freer.t
  val fail : unit -> ('a, [> `Option], [>]) Freer.t

  val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a option -> unit
end = struct
  include Type1 (struct type 'a t = 'a option end) ()

  let return x = lift @@ Some x
  let fail () = lift None

  let pp pp_x fmt =
    let pp f = Format.fprintf fmt f in
    function
    | None   -> pp "<Nothing>"
    | Some x -> pp "%a" pp_x x
end

module List_ : sig
  type 'a t = 'a list

  type w

  val wit : w Freer.wit
  val inj0 : 'a t -> ('a, w, [> `Nondet]) app
  val prj : ('a, w, [> `Nondet]) app -> 'a t
  val inj : (w, [>], [> `Nondet]) Freer.inj

  val return : 'a -> ('a, [> `Nondet], [>]) Freer.t
  val choose : 'a list -> ('a, [> `Nondet], [>]) Freer.t

  val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a list -> unit
end = struct
  include Type1 (struct type 'a t = 'a list end) ()

  let return x = lift @@ [x]
  let choose = lift

  let pp pp_x fmt =
    let pp f = Format.fprintf fmt f in
    let rec loop =
      function
      | []      -> pp "]"
      | x :: xs -> pp "; %a" pp_x x; loop xs
    in
    function
    | x :: xs -> pp "[ %a" pp_x x; loop xs
    | []      -> pp "[]"
end

module Num : sig
  type num = ..

  type ('r, 'a) t =
    | Of_int : int          -> (num, [>] as 'a) t
    | Of_env : ([>]  as 'a) -> (num, 'a) t
    | Add    : num * num    -> (num, 'a) t
    | Sub    : num * num    -> (num, 'a) t
    | Mul    : num * num    -> (num, 'a) t
    | Div    : num * num    -> (num, 'a) t
    | Neg    : num          -> (num, 'a) t
    | Gt     : num * num    -> (bool, 'a) t
    | Lt     : num * num    -> (bool, 'a) t
    | Ge     : num * num    -> (bool, 'a) t
    | Le     : num * num    -> (bool, 'a) t
    | Eq     : num * num    -> (bool, 'a) t
    | Neq    : num * num    -> (bool, 'a) t
    | Show   : num          -> (string, 'a) t

  type w

  val wit : w Freer.wit

  val inj0 : ('r, [>] as 'a) t -> ('r, w, [> `Num of 'a]) app
  val prj : ('r, w, [> `Num of 'a]) app -> ('r, [>] as 'a) t
  val inj : (w, [> `Num of [>] as 'a], [> `Num of 'a]) Freer.inj

  val (!) : int -> (num, [> `Num of [>]], [>]) Freer.t
  val env : ([>] as 'a) -> (num, [> `Num of 'a], [>]) Freer.t
  val (+) : (num, [> `Num of [>] as 'a] as 'b, [>] as 'c) Freer.t
    -> (num, 'b, 'c) Freer.t
    -> (num, 'b, 'c) Freer.t
  val (-) : (num, [> `Num of [>] as 'a] as 'b, [>] as 'c) Freer.t
    -> (num, 'b, 'c) Freer.t
    -> (num, 'b, 'c) Freer.t
  val ( * ) : (num, [> `Num of [>] as 'a] as 'b, [>] as 'c) Freer.t
    -> (num, 'b, 'c) Freer.t
    -> (num, 'b, 'c) Freer.t
  val (/) : (num, [> `Num of [>] as 'a] as 'b, [>] as 'c) Freer.t
    -> (num, 'b, 'c) Freer.t
    -> (num, 'b, 'c) Freer.t
  val (~-) : (num, [> `Num of ([>] as 'a)] as 'b, [>] as 'c) Freer.t -> (num, 'b, 'c) Freer.t
  val (>) : (num, [> `Num of [>] as 'a] as 'b, [>] as 'c) Freer.t
    -> (num, 'b, 'c) Freer.t
    -> (bool, 'b, 'c) Freer.t
  val (<) : (num, [> `Num of [>] as 'a] as 'b, [>] as 'c) Freer.t
    -> (num, 'b, 'c) Freer.t
    -> (bool, 'b, 'c) Freer.t
  val (>=) : (num, [> `Num of [>] as 'a] as 'b, [>] as 'c) Freer.t
    -> (num, 'b, 'c) Freer.t
    -> (bool, 'b, 'c) Freer.t
  val (<=) : (num, [> `Num of [>] as 'a] as 'b, [>] as 'c) Freer.t
    -> (num, 'b, 'c) Freer.t
    -> (bool, 'b, 'c) Freer.t
  val (=) : (num, [> `Num of [>] as 'a] as 'b, [>] as 'c) Freer.t
    -> (num, 'b, 'c) Freer.t
    -> (bool, 'b, 'c) Freer.t
  val (<>) : (num, [> `Num of [>] as 'a] as 'b, [>] as 'c) Freer.t
    -> (num, 'b, 'c) Freer.t
    -> (bool, 'b, 'c) Freer.t
  val show : (num, [> `Num of [>] as 'a] as 'b, [>] as 'c) Freer.t -> (string, 'b, 'c) Freer.t
  val if_ : (bool, [>] as 'b, [>] as 'c) Freer.t
    -> then_:(unit -> ('a, 'b, 'c) Freer.t) -> else_:(unit -> ('a, 'b, 'c) Freer.t)
    -> ('a, 'b, 'c) Freer.t
end = struct
  module Wit = struct
    type w = W
    type _ wit += W : w wit
  end

  include Wit

  let wit : w Freer.wit = (module Wit)

  type num = ..

  type ('r, 'a) t =
    | Of_int : int          -> (num, [>] as 'a) t
    | Of_env : ([>]  as 'a) -> (num, 'a) t
    | Add    : num * num    -> (num, 'a) t
    | Sub    : num * num    -> (num, 'a) t
    | Mul    : num * num    -> (num, 'a) t
    | Div    : num * num    -> (num, 'a) t
    | Neg    : num          -> (num, 'a) t
    | Gt     : num * num    -> (bool, 'a) t
    | Lt     : num * num    -> (bool, 'a) t
    | Ge     : num * num    -> (bool, 'a) t
    | Le     : num * num    -> (bool, 'a) t
    | Eq     : num * num    -> (bool, 'a) t
    | Neq    : num * num    -> (bool, 'a) t
    | Show   : num          -> (string, 'a) t

  type ('a, 'w, 'e) app += App : ('r, [>] as 'b) t -> ('r, w, [> `Num of 'b]) app

  let inj0 o = App o

  let prj =
    function
    | App o -> o
    | _     -> assert false

  let inj : _ Freer.inj = { Freer.f = fun x -> inj0 @@ prj x }

  let lift0 x = Freer.lift W @@ inj0 x

  let (!) i = lift0 (Of_int i)
  let env x = lift0 (Of_env x)
  let app f a b =
    let open Freer in
    a >>= fun a ->
    b >>= fun b ->
    lift0 (f a b)
  let (+) a b = app (fun a b -> Add (a, b)) a b
  let (-) a b = app (fun a b -> Sub (a, b)) a b
  let ( * ) a b = app (fun a b -> Mul (a, b)) a b
  let ( /) a b = app (fun a b -> Div (a, b)) a b
  let (>) a b = app (fun a b -> Gt (a, b)) a b
  let (<) a b = app (fun a b -> Lt (a, b)) a b
  let (>=) a b = app (fun a b -> Ge (a, b)) a b
  let (<=) a b = app (fun a b -> Gt (a, b)) a b
  let (=) a b = app (fun a b -> Eq (a, b)) a b
  let (<>) a b = app (fun a b -> Neq (a, b)) a b
  let (~-) a = Freer.bind a (fun a -> lift0 @@ Neg a)
  let show a = Freer.bind a (fun a -> lift0 @@ Show a)
  let if_ x ~then_ ~else_ = let open Freer in x >>= fun c -> if c then then_ () else else_ ()
end

module type T0 = sig type t end

module type State = sig
  module S : T0

  type 'a t = S.t -> 'a * S.t

  type w

  val wit : w Freer.wit
  val inj0 : 'a t -> ('a, w, [> `State of w]) app
  val prj : ('a, w, [> `State of w]) app -> 'a t
  val inj : (w, [>], [> `State of w]) Freer.inj

  val get : unit -> (S.t, [> `State of w], [>]) Freer.t
  val put : S.t -> (unit, [> `State of w], [>]) Freer.t
end

module State (S : T0) () : State with module S = S = struct
  module S = S

  include Type1 (struct type 'a t = S.t -> 'a * S.t end) ()

  let get () = lift @@ fun s -> s, s
  let put x = lift @@ fun _ -> (), x
end

module type Num = sig
  type t
  type 'a env constraint 'a = [>]

  type Num.num += N : t -> Num.num

  val of_int : int -> t
  val of_env : ([>] as 'a) env -> 'a -> t
  val (+) : t -> t -> t
  val (-) : t -> t -> t
  val ( * ) : t -> t -> t
  val (/) : t -> t -> t
  val (~-) : t -> t
  val (<) : t -> t -> bool
  val (=) : t -> t -> bool
  val show : t -> string
end

module Num_handler (N : Num) = struct
  let handle e =
    Freer.handle0
      ~wit:Num.wit
      ~inj:Num.inj
      ~hdl:{ Freer.f = fun (type r) (m : (r, _, _) app) (k : r -> _) ->
          k @@
          let open Num in
          let open! N in
          let r = function N x -> x | _ -> assert false in
          match prj m with
          | Of_int n   -> N (of_int n)
          | Of_env a   -> N (of_env e a)
          | Add (a, b) -> N (r a + r b)
          | Sub (a, b) -> N (r a - r b)
          | Mul (a, b) -> N (r a * r b)
          | Div (a, b) -> N (r a / r b)
          | Neg a      -> N (~- (r a))
          | Gt (a, b)  -> r b < r a
          | Lt (a, b)  -> r a < r b
          | Ge (a, b)  -> r b < r a || r b = r a
          | Le (a, b)  -> r a < r b || r b = r a
          | Eq (a, b)  -> r a = r b
          | Neq (a, b) -> not (r a = r b)
          | Show a     -> show @@ r a }
end

module State_option_list_handler (State : State) = struct
  let handle s m =
    let option =
      Freer.handle
        ~wit:Option.wit
        ~inj:Option.inj
        ~hdl:{ Freer.f = fun m k -> match Option.prj m with Some x -> k x | None -> None }
        ~dlg:(fun m -> assert false)
        ~cont:(fun x -> Some x)
    in
    let list =
      Freer.handle
        ~wit:List_.wit
        ~inj:List_.inj
        ~hdl:{ Freer.f = fun m k -> List.(concat @@ map (fun x -> k x) @@ List_.prj m) }
        ~dlg:(fun m -> match option m with Some r -> r | None -> [])
        ~cont:(fun r -> [r])
    in
    let state =
      Freer.handle
        ~wit:State.wit
        ~inj:State.inj
        ~hdl:{ Freer.f = fun m k -> fun x -> let x, s = (State.prj m) x in k x s }
        ~dlg:(fun m s -> List.(concat @@ map ((|>) s) @@ list m))
        ~cont:(fun r _ -> [r])
    in
    state m s
end

module Num0 = struct type t = Num.num end

module Num_state = State (Num0) ()

let check_ge_0 x =
  let open Freer in
  let open Num in
  if_ (x >= !0) ~then_:(fun () -> Option.return x) ~else_:(fun () -> Option.fail ())

let repeat_minus =
  let open Freer in
  let rec loop n =
    if n <= 0 then return ()
    else
      Num.(!2 * Num_state.get () - env `C) >>=
      Num_state.put >>= fun () ->
      loop (n - 1)
  in
  loop

let test0 =
  let open Freer in
  List_.choose [1; 2; 3; 4; 5; 6; 7; 8] >>= fun x ->
  repeat_minus x >>= fun () ->
  check_ge_0 (Num_state.get ()) >>= fun x ->
  Num.show x

module N0 = struct
  type t = float
  type 'a env = (([>] as 'a) * t) list

  type Num.num += N : float -> Num.num

  let of_int = float_of_int
  let of_env e x = List.assoc x e
  let (+) = (+.)
  let (-) = (-.)
  let ( * ) = ( *.)
  let (/)  = (/.)
  let (~-) = (~-.)
  let (<) = (<)
  let (=) = (=)
  let show = string_of_float
end

module Num_handler0 = Num_handler (N0)

module State_option_list_handler0 = State_option_list_handler (Num_state)

let () =
  Format.fprintf
    Format.std_formatter
    "%a\n%a"
    (List_.pp Format.pp_print_string)
    ((test0 : (string, [`Nondet | `Num of [`C ] | `Option | `State of Num_state.w ] as 'a, 'a) Freer.t) |>
     Num_handler0.handle [`C, 5.] |>
     State_option_list_handler0.handle (N0.N 4.))
    (List_.pp Format.pp_print_string)
    (test0 |>
     Num_handler0.handle [`C, 3.] |>
     State_option_list_handler0.handle (N0.N 14.))
