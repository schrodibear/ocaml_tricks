module M1_maker = M1.Make (M2.Intf) (M3.Intf)
module M2_maker = M2.Make (M1.Intf) (M3.Intf)
module M3_maker = M3.Make (M1.Intf) (M2.Intf)

module rec Common :
  Sigs.Common with module M1_intf := M1.Intf and module M2_intf := M2.Intf and module M3_intf = M3.Intf =
  Common
open Common

module rec M1_ : M1 = M1_maker.Make (M2_) (M3_)
and M2_ : M2 = M2_maker.Make (M1_) (M3_)
and M3_ : M3 = M3_maker.Make (M1_) (M2_)

module M1 = (val M1_.(access I'm_main))
module M2 = (val M2_.(access I'm_main))
module M3 = (val M3_.(access I'm_main))

let () =
  M1.do_it ();
  M2.do_it ();
  M3.do_it ()
