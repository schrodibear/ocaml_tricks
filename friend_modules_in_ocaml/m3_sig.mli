module M1_intf : module type of Intf
module M2_intf : module type of Intf
module M3_intf : module type of Intf

module type M3_for_Main = sig val do_it : unit -> unit end
module type M3_for_M3 = sig val a : int val b : string val c : float include M3_for_Main end
module type M3_for_M2 = sig val a : int val c : float end
module type M3_for_M1 = sig val a : int val b : string end

module type M3 = sig
  type _ accessor =
    | I'm_M1 : M1_intf.wit -> (module M3_for_M1) accessor
    | I'm_M2 : M2_intf.wit -> (module M3_for_M2) accessor
    | I'm_main : (module M3_for_Main) accessor

  val access : 'a accessor -> 'a
end
