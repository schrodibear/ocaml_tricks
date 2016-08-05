module Intf : sig
  type wit
end

module Make (M1_intf : Sigs.Intf) (M2_intf : Sigs.Intf) : sig
  module Common : Sigs.Common with module M1_intf := M1_intf and module M2_intf := M2_intf and module M3_intf := Intf
  open Common
  module Make (M1 : M1) (M2 : M2) : M3
end
