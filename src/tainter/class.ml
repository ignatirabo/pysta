module SSet = String_helper.SSet
module SMap = String_helper.SMap

module L = Language
module Lo = Location
module E = L.Expr
module F = Function
module S = Signature

(** Classes *)
type clss = {
  name: string;
  methods: F.t ;
  body: L.block
}

type t = clss SMap.t

let empty = SMap.empty