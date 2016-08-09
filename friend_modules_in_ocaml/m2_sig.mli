module M1_intf : module type of Intf
module M2_intf : module type of Intf
module M3_intf : module type of Intf

module type M2_for_Main = sig val do_it : unit -> unit end
module type M2_for_M2 = sig val a : int val b : string val c : float include M2_for_Main end
module type M2_for_M1 = sig val a : int val b : string end
module type M2_for_M3 = sig val b : string val c : float end

module type M2 = sig
  type _ accessor =
    | I'm_M1 : M1_intf.wit -> (module M2_for_M1) accessor
    | I'm_M3 : M3_intf.wit -> (module M2_for_M3) accessor
    | I'm_main : (module M2_for_Main) accessor

  val access : 'a accessor -> 'a
end
