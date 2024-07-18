module SSet = String_helper.SSet
module SMap = String_helper.SMap
module P = Prog
module L = Language
module Lo = Location
module T = Taint
module C = Counter.CounterConstant4
module D = Domain
module E = L.Expr
module F = Function
module S = Signature
module SMT = Smt

type context = { prog : P.prog; sign : S.t list; locs : Lo.location option; }
type config = {
  block : L.block_ext;
  state : D.state;
  flag : Flag.flag;
  id : Id.id;
  counter : C.t;
}

val analyze_prog : ?json:bool -> string -> context -> (D.SMT.zsolver * E.texpr option * config) list
