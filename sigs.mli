module type Intf = sig type wit end
module type Common = sig
  module M1_intf : Intf
  module M2_intf : Intf
  module M3_intf : Intf

  module type M1_for_Main = sig val do_it : unit -> unit end
  module type M2_for_Main = M1_for_Main
  module type M3_for_Main = M1_for_Main

  module type M1_for_M1 = sig val a : int val b : string val c : float include M1_for_Main end
  module type M1_for_M2 = sig val a : int val c : float end
  module type M1_for_M3 = sig val b : string val c : float end

  module type M2_for_M2 = M1_for_M1
  module type M2_for_M1 = sig val a : int val b : string end
  module type M2_for_M3 = M1_for_M3

  module type M3_for_M3 = M1_for_M1
  module type M3_for_M2 = M1_for_M2
  module type M3_for_M1 = M2_for_M1

  module type M1 = sig
    type _ accessor =
      | I'm_M2 : M2_intf.wit -> (module M1_for_M2) accessor
      | I'm_M3 : M3_intf.wit -> (module M1_for_M3) accessor
      | I'm_main : (module M1_for_Main) accessor

    val access : 'a accessor -> 'a
  end

  module type M2 = sig
    type _ accessor =
      | I'm_M1 : M1_intf.wit -> (module M2_for_M1) accessor
      | I'm_M3 : M3_intf.wit -> (module M2_for_M3) accessor
      | I'm_main : (module M2_for_Main) accessor

    val access : 'a accessor -> 'a
  end

  module type M3 = sig
    type _ accessor =
      | I'm_M1 : M1_intf.wit -> (module M3_for_M1) accessor
      | I'm_M2 : M2_intf.wit -> (module M3_for_M2) accessor
      | I'm_main : (module M3_for_Main) accessor

    val access : 'a accessor -> 'a
  end
end
