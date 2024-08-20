type typ =
  | Int
  | String
  | List of typ
[@@deriving show]

type id = string
[@@deriving show]
type annotation = id
[@@deriving show]
type number = int
[@@deriving show]

type uop = Not
[@@deriving show]
type bop =
  | And | Or
  | Add | Sub | Mult | Div | Pow
[@@deriving show]
type cop =
  | Eq | NotEq | Lt | LtE | Gt | GtE
[@@deriving show]

type const =
  | True | False
  | Integer of int
  | String of string
[@@deriving show]
type expr =
  | Const of const
  | List of expr list
  | Dict of expr list * expr list
  | Name of id
  | UOp  of uop * expr
  | BOp  of bop * expr * expr
  | Comp of cop * expr * expr
  | Call of expr * expr list
  | Attr of expr * id        (* Get attribute of expression *)
  | Subscript of expr * expr (* Last expr is constant or slice. Slice is not implemented yet. *)
  | Slice of expr option * expr option * expr option
  | Joined of expr list
[@@deriving show]
type lexpr =
  | Name of id
  | Subscript of id * expr
  | Attrib of id * id
[@@deriving show]

type location = { start:int ; stop:int }
[@@deriving show]

type stmt =
  | Assign    of lexpr * typ option * expr
  | If        of expr * block * block
  | While     of expr * block
  | Return    of expr
  | Expr      of expr
  | For       of id * expr * block
[@@deriving show]

and stmt_ln = { location:location ; stmt:stmt }

and block = stmt_ln list
[@@deriving show]

type proc = { id:id ; inp:(id * annotation option) list ; block:block ; location:location }
[@@deriving show]

type clss = { name:id ; block:block ; methods:proc list ; location:location }
[@@deriving show]

type prog = { main:block ; procs:proc list ; lib:proc list ; clss:clss list }
[@@deriving show]
let empty_prog : prog = { main=[] ; procs=[] ; lib=[] ; clss=[] }
