module Intf : sig
  type wit
end

module Make (M1_intf : Sigs.Intf) (M3_intf : Sigs.Intf) : sig
  module Common : Sigs.Common with module M1_intf := M1_intf and module M2_intf := Intf and module M3_intf := M3_intf
  open Common
  module Make (M1 : M1) (M3 : M3) : M2
end
