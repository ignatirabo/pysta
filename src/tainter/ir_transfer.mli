open Pyreir

val from_proc : ?sign:Signature.t list -> ?clss:bool -> Ir_sig.proc -> Function.func
val from_prog : Ir_sig.prog -> Signature.t list -> Prog.t