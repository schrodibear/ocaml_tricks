module Intf = struct
  type wit = W
end

module Make (M1_intf : Sigs.Intf) (M3_intf : Sigs.Intf) = struct
  module rec Common :
    Sigs.Common with module M1_intf := M1_intf and module M2_intf := Intf and module M3_intf := M3_intf =
    Common
  open Common

  module Make (M1 : M1) (M3 : M3) = struct
    module M2 = struct
      let a = 2
      let b = "2"
      let c = 2.

      let do_it () =
        let (module M1) = M1.(access @@ I'm_M2 Intf.W) in
        let (module M3) = M3.(access @@ I'm_M2 Intf.W) in
        Printf.printf "M2: M1: a = %d c = %f M3: a = %d c = %f\n" M1.a M1.c M3.a M3.c;
        Printf.printf "M2 called from main\n"
    end

    module rec I : Common.M2 = I
    include I

    let access : type a. a accessor -> a =
      function
      | I'm_M1 _ -> (module M2)
      | I'm_M3 _ -> (module M2)
      | I'm_main -> (module M2)
  end
end
