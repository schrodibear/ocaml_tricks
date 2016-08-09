module Intf : sig
  type wit
end

module Make (M2_intf : Sigs.Intf) (M3_intf : Sigs.Intf) : sig
  module Common : Sigs.Common with module M1_intf := Intf and module M2_intf := M2_intf and module M3_intf := M3_intf
  open Common
  module Make (M2 : M2) (M3 : M3) : M1
end
