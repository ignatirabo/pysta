module SSet = String_helper.SSet
module SMap = String_helper.SMap

module L = Language
module Lo = Location
module S = Signature

(** Procedures *)
type func = { sign:     S.t ; 
              body:     L.block option ; (** procedure body *)
              location: Lo.location }

type t = func SMap.t

let empty = SMap.empty
