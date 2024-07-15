module SSet = String_helper.SSet
module SMap = String_helper.SMap

module E  = Language.Expr
type expr = E.texpr
type elt = expr
type t    = elt SMap.t
type svm  = t

let empty = SMap.empty
let add = SMap.add
let iter = SMap.iter
let fold = SMap.fold
let mapi = SMap.mapi
let find = SMap.find
let find_opt = SMap.find_opt
let find_first_opt = SMap.find_first_opt
let update = SMap.update
