module Intf = struct
  type wit = W
end

module Make (M2_intf : Sigs.Intf) (M3_intf : Sigs.Intf) = struct
  module rec Common :
    Sigs.Common with module M1_intf := Intf and module M2_intf := M2_intf and module M3_intf := M3_intf =
    Common
  open Common

  module Make (M2 : M2) (M3 : M3) = struct
    module M1 = struct
      let a = 1
      let b = "1"
      let c = 1.

      let do_it () =
        let (module M2) = M2.(access @@ I'm_M1 Intf.W) in
        let (module M3) = M3.(access @@ I'm_M1 Intf.W) in
        Printf.printf "M1: M2: a = %d b = %s M3: a = %d b = %s\n" M2.a M2.b M3.a M3.b;
        Printf.printf "M1 called from main\n"
    end

    module rec I : Common.M1 = I
    include I

    let access : type a. a accessor -> a =
      function
      | I'm_M2 _ -> (module M1)
      | I'm_M3 _ -> (module M1)
      | I'm_main -> (module M1)
  end
end
