module Intf = struct
  type wit = W
end

module Make (M1_intf : Sigs.Intf) (M2_intf : Sigs.Intf) = struct
  module rec Common :
    Sigs.Common with module M1_intf := M1_intf and module M2_intf := M2_intf and module M3_intf := Intf =
    Common
  open Common
  module Make (M1 : M1) (M2 : M2) = struct
    module M3 = struct
      let a = 3
      let b = "3"
      let c = 3.

      let do_it () =
        let (module M1) = M1.(access @@ I'm_M3 Intf.W) in
        let (module M2) = M2.(access @@ I'm_M3 Intf.W) in
        Printf.printf "M3: M1: b = %s c = %f M2: b = %s c = %f\n" M1.b M1.c M2.b M2.c;
        Printf.printf "M3 called from main\n"
    end

    type _ accessor =
      | I'm_M1 : M1_intf.wit -> (module M3_for_M1) accessor
      | I'm_M2 : M2_intf.wit -> (module M3_for_M2) accessor
      | I'm_main : (module M3_for_Main) accessor

    let access : type a. a accessor -> a =
      function
      | I'm_M1 _ -> (module M3)
      | I'm_M2 _ -> (module M3)
      | I'm_main -> (module M3)
  end
end


