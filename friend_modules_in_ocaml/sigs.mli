module type Intf = module type of Intf
module type Common = sig
  module M1_intf : Intf
  module M2_intf : Intf
  module M3_intf : Intf

  include module type of M1_sig with
    module M1_intf := M1_intf and module M2_intf := M2_intf and module M3_intf := M3_intf
  include module type of M2_sig with
    module M1_intf := M1_intf and module M2_intf := M2_intf and module M3_intf := M3_intf
  include module type of M3_sig with
    module M1_intf := M1_intf and module M2_intf := M2_intf and module M3_intf := M3_intf
end
