module SSet := String_helper.SSet
module SMap := String_helper.SMap

module E := Language.Expr
type expr := E.texpr
type elt := expr
type vname := string
type svm = elt SMap.t
type t = svm
val empty: svm
val add: vname -> elt -> svm -> svm
val update: vname -> (elt option -> elt option) -> svm -> svm
val iter: (vname -> elt -> unit) -> svm -> unit
val fold: (vname -> elt -> 'a -> 'a) -> svm -> 'a -> 'a
val mapi: (vname -> elt -> 'a) -> svm -> 'a SMap.t
val find: vname -> svm -> elt
val find_opt: vname -> svm -> elt option
val find_first_opt: (vname -> bool) -> svm -> (vname * elt) option
