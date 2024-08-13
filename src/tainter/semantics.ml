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

type context = { prog: P.prog ; sign: Signature.t list ; locs: Lo.location option }
type config = { block: L.block_ext ; state: D.state ; flag: Flag.t ; id: Id.t ; counter: C.t }

let rec parse_subscript (expr: E.expr) (attr_ls, key_ls) : string list * E.const list =
  match expr with
  | Esubscript (attr, key) ->
    parse_subscript attr (attr_ls, key :: key_ls)
  | Eattrib (name, attr) ->
    parse_subscript attr (name :: attr_ls, key_ls)
  | Evar name ->
    (List.rev (name::attr_ls), key_ls)
  | _ -> failwith "parse_subscript: unexpected case."

let rec find_loc_block (loc: Lo.location) (block: L.block) : L.block option =
  match block with
  | [] -> None
  | (loc',stmt)::block ->
    if !Utils.debug then
      Printf.printf "loc'={ start=%d ; stop=%d }, stmt=%a\n" loc'.start loc'.stop (P.pp_stat "") stmt;
    if loc.start = loc'.start then
      Some ((loc',stmt)::block)
    else if loc.start <= loc'.stop then
      match stmt with
      | If (_,b0,b1) ->
        let block = find_loc_block loc b0 in
        if Option.is_some block then block
        else
          let block = find_loc_block loc b1 in
          if Option.is_some block then block
          else None
      | While (_,b) ->
        let block = find_loc_block loc b in
        if Option.is_some block then block
        else None
      | _ -> find_loc_block loc block
    else
      find_loc_block loc block

let rec find_loc (loc: Lo.location) ctx : L.block * D.state option =
  (* Try to find location in all the programs *)
  (* 1. Check main body *)
  let oblock = find_loc_block loc ctx.prog.pmain in
  if Option.is_some oblock then
    Option.get oblock, None
  else
    (* 2. check procs *)
    let func_loc (func : F.func) : L.block option * D.state option =
        if Lo.start loc = Lo.start func.location then
          let input = S.get_input func.sign in
          let state: D.state = List.fold_left
            (fun (acc: D.state) (arg: S.arg) ->
              if Option.is_some arg.annotation then
                (* For now we only have type annotation for classes *)
                D.new_obj acc arg.name arg.annotation
              else
                D.new_var acc arg.name) D.empty input in
          func.body, Some state
        else
          failwith "func_loc: unexepcted." in
    let loop_funcs (funcs: F.t) : L.block option * D.state option =
      try
        let name, func = SMap.find_first (fun name ->
          let func = SMap.find name funcs in
          Lo.start loc = Lo.start func.location) funcs in
        func_loc func
      with Not_found -> failwith "not a proc" in
    let obody, state = loop_funcs ctx.prog.funcs in
    Option.get obody, state

(** Create initial abstract configuration from program *)
let init ?(svm=None) (ctx: context) : config =
  let block, stateo =
    if Option.is_none ctx.locs then
      L.block_to_ext ~sign:ctx.sign ctx.prog.pmain, None
    else
      let locs = Option.get ctx.locs in
      let block, stateo = find_loc locs ctx in
      block |> L.block_to_ext ~sign:ctx.sign, stateo in
  let state =
    if Option.is_some stateo then
      Option.get stateo
    else
      D.new_state ~svm (Some ctx.prog) in
  { state ; block ; id=Id.make false; counter=C.empty ; flag=[] }

(** Auxiliary: add symbolic mapping on tmap. *)
let add_symb_tmap (tmap: E.texpr SMap.t) (name: string) (taint: T.t) : E.texpr * E.texpr SMap.t =
  let symbname = SMT.new_var name in
  let te: E.texpr = Tvar (taint,symbname) in
  te, SMap.add name te tmap

(** Auxiliary for assignemnt on dictionaries or objects. *)
let update_lexpr (e: E.lexpr) (te: E.texpr) (s: D.state) : D.state =
  let d, s = match e with
    | Attrib (v,a) | Subscript (v,a) ->
      let a = E.expr_2str a in
      let o, s = D.find v s in
      let tmap = E.get_tmap o in
      let d = SMap.add a te tmap in
      d, s
    | _ -> failwith "update_lexpr: not a dict or object." in
  match e with
  | Attrib (v,_) ->
    let o, _ = D.find v s in
    let clssname =
      try
        Some (E.get_clssname o)
      with E.Vague_obj _ ->
        None in
    D.update_elt v (Tobj (clssname, d)) s
  | Subscript (v,_) ->
    D.update_elt v (Tdict d) s
  | _ -> failwith "update_lexpr: not a dict or object."

let get_taint_clss clssname attr ctx : T.t option =
  if !Utils.debug then Printf.printf "get_taint_clss: trying to find signature for attribute %s\n" attr;
  try
    if Option.is_none clssname then
      begin
        if !Utils.debug then Printf.printf "get_taint_clss: no class name with attribute %s\n" attr;
        None
      end
    else (* Class is Some *)
      let cls = Option.get clssname in
      let sign = Option.get (S.find ctx.sign (cls ^ "." ^ attr)) in
      if S.is_attr sign then
        begin
          if !Utils.debug then Printf.printf "get_taint_clss: succesfully found signature for attribute %s in class %s\n" attr cls;
          Some (S.get_output sign |> Option.get |> T.source_2t)
        end
      else
          (* Printf.printf "Function signature found: cls = %s; attr = %s\n" cls attr; *)
          failwith "expr_eval: function signature in attribute."
        (* end *)
  with
  | Not_found ->
    (* Search in signature *)
    if !Utils.debug then Printf.printf "get_taint_clss: cannot find signature for attribute %s in class %s\n" attr (Option.get clssname);
    None
  | Invalid_argument _ ->
    if !Utils.debug then Printf.printf "get_taint_clss: no taint for attribute %s in class %s\n" attr (Option.get clssname);
    None
    (* failwith "eval_subscript: empty output." *)

let fun_taint_input sign te_ls fname loc cnf ctx : Flag.flag * (string * E.texpr) list=
  let sanitize = S.get_sanitize sign in
  let input_args = ref (S.get_input sign) in
  if S.get_self sign then
    input_args := List.tl !input_args;
  let flag = ref cnf.flag in
  (* let met_sink_source = ref false in *)
  (* Match each input with the signature of the procedure. *)
  let te_ls: (string * E.texpr) list = List.map
    (fun (te: E.texpr) ->
      let input_arg = List.hd !input_args in
      input_args := List.tl !input_args;
      match sanitize with
      (* Clear taint for te if function is sanitized *)
      | Some All | Some Parameters ->
        input_arg.name, E.clear_taint te
      | _ ->
        let te =
          try
            E.add_taint te (Option.get input_arg.taint |> T.sink_add_loc loc |> T.sink_2t)
          with Invalid_argument _ -> te in
        if List.length (E.is_rule te) > 0 then begin
          if !Utils.debug then Printf.printf "Met sink in function %s.\n" fname;
          (* met_sink_source := true; *)
          flag := Flag.add Flag.FINAL !flag end
        else
          if !Utils.debug then Printf.printf "Did not meet sink in function %s.\n" fname;
        input_arg.name, te) te_ls in
  !flag, te_ls

let rec eval_input_list (inp_ls: E.expr list) cnf ctx =
  let (te_ls, cnf): E.texpr list * config =
    begin try
      List.fold_left (fun (acc,cnf) inp_expr ->
        let tecnfs = expr_eval inp_expr cnf ctx in
        if List.length tecnfs > 1 then failwith "enter_fun: input generated multiple values.";
        let te, cnf = List.hd tecnfs |> fst |> Option.get, List.hd tecnfs |> snd in
        (te::acc,cnf)) ([],cnf) inp_ls
    with exn ->
      raise exn end in
      (* failwith "enter_fun: exception raised when evaluating inputs." end in *)
  let te_ls = List.rev te_ls in
  te_ls, cnf

(** Enter function f with input arguments. Here we set the configuration to execute on
    the body of the function. If an argument with a source is sinking, we add a FINAL flag.
    If the function is sanitized we do the proper "cleaning". *)
and enter_fun (f,inp_exprs) cnf ctx : config =
  (* Try to find procedure in context *)
  let f_proc = try
      Some (P.get_func f ctx.prog)
    with Not_found -> None in
  (* Get line of code of current block *)
  let loc = List.hd cnf.block |> fst in
  if Option.is_some f_proc then
    (* Calculate value of input expressions.                                                 *
     * Configuration is updated in each evaluation expression.                               *
     * Each expression should generate just one value, if not implies that it is a function,
       therefore evaluation fails. *)
    let te_ls, cnf = eval_input_list inp_exprs cnf ctx in
    let f_proc = Option.get f_proc in
    let flag, te_ls = fun_taint_input f_proc.sign te_ls f loc cnf ctx in
    let state = List.fold_left (fun state (name,te) -> D.update_elt name te state) cnf.state te_ls in
    let block = try
        L.block_to_ext ~sign:ctx.sign (Option.get f_proc.body)
      with Invalid_argument _ -> [ Lo.location_temp, L.Return (List.hd te_ls |> snd |> E.texpr_2expr) ] in
    let id = Id.make ~message:["Function evaluation"] true in
    { cnf with state ; block ; flag ; id }
  else begin
    (* If proc is not defined, return first argument. *)
    let e =
      try
        List.hd inp_exprs
      with _ ->
        (* Expression that creates a new symbol *)
        E.Ecall (E.Evar "Symbolic.new", []) in
        (* failwith ("enter_fun: function " ^ f ^ " not defined and no input argument.") in *)
    let block: L.block_ext = [ Lo.location_temp, L.Return e ] in
    { cnf with block }
  end

(** Need to make better documentation for this function, but it should be used similarly to
    enter_fun, but specifically for methods. *)
and enter_method ?(obj=None) ((f, inpls): F.func * E.expr list) (cnf: config) (ctx: context) : config =
  (* The first argument is always self *)
  (* Evaluate input expressions *)
  let te_ls, cnf = eval_input_list inpls cnf ctx in
  let loc = List.hd cnf.block |> fst in
  let flag, te_ls = fun_taint_input f.sign te_ls (S.get_name f.sign) loc cnf ctx in
  (* Create state from scratch with self and arguments passed *)
  let state = D.new_state (Some ctx.prog) in
  let state = List.fold_left (fun state (name,te) -> D.update_elt name te state) state te_ls in
  let state =
    if S.get_self f.sign then
      let obj = Option.get obj in
      D.update_elt "self" obj state
    else
      state in
  let block = try
      L.block_to_ext ~sign:ctx.sign (Option.get f.body)
    with Invalid_argument _ -> [ Lo.location_temp, L.Return (List.hd te_ls |> snd |> E.texpr_2expr) ] in
  let id = Id.make ~message:["Function evaluation"] true in
  { cnf with state ; block ; flag ; id }

(** Add taint on return value of function. *)
and fun_taint_output (f,inps) cnf ctx (oe,cnf_temp: E.texpr option * config) =
  if Flag.check_flag Flag.FINAL cnf_temp.flag then begin
    None, cnf_temp
  end else
    let f_proc = try
        Some (P.get_func f ctx.prog)
      with Not_found -> None in
    let loc = List.hd cnf.block |> fst in
    let te =
      try
        Option.get oe
      with Invalid_argument _ -> failwith "fun_taint_output: No output when returning?" in
    (* Get signature and remove taint accordingly *)
    let teo, cnf =
      if Option.is_some f_proc then
        let f_proc = Option.get f_proc in
        let sanitize = S.get_sanitize f_proc.sign in
        let te = match sanitize with
          | Some All -> E.clear_taint te
          | _ ->
            begin match S.get_output f_proc.sign with
            | None -> te
            | Some out ->
              let out = T.source_add_loc loc out in
              E.add_taint te (T.source_2t out)
            end in
        Some te, cnf
      else
        Some te, cnf in
    let state = D.set_sp (D.get_sp cnf_temp.state) cnf.state in
    teo, { cnf with state }

(** Evaluate language expression: turns expressions into taint-expressions.
    All Evar constructors must be replaced by the actual value mapped in the state. *)
and expr_eval ?(typ=None) (e : E.expr) (cnf: config) (ctx: context) : (E.texpr option * config) list =
    match e with
    | Econst c -> [ Some (Tconst (Taint.empty,c)), cnf ]
    | Evar v ->
      (* TODO: fix bug, not checking type of v *)
      let te, state = D.find v cnf.state in
      [ Some te, { cnf with state } ]
    | Euop (op, e) ->
      let tecnfs = expr_eval ~typ e cnf ctx in
      List.map (fun (te,cnf) ->
        let te = try Option.get te with Invalid_argument _ -> failwith "expr_eval: None" in
        let t, e = E.get_taint te, E.get_expr te in
        Some (E.Tuop (t, op, e)), cnf) tecnfs
    | Eop (e0, op, e1) ->
      let tecnfs = expr_eval ~typ e0 cnf ctx in
      let tecnfs = List.fold_left (fun acc (te0,cnf) -> (expr_eval ~typ e1 cnf ctx |> List.map (fun (te1,cnf) -> te0,te1,cnf)) @ acc) [] tecnfs in
      (* Since expr_eval returns a list, we have to run expr_eval of e1 for each of the generated configs *)
      (* Before we were ignoring modifications to the configuration. These are important since variables might be created. *)
      List.map (fun (te0,te1,cnf) ->
        let te0, te1 = try Option.get te0, Option.get te1
          with Invalid_argument _ -> failwith "expr_eval: None" in
        let t0, e0 = E.get_taint te0, E.get_expr te0 in
        let t1, e1 = E.get_taint te1, E.get_expr te1 in
        let t = Taint.comb t0 t1 in
        Some (E.Tbop (t, e0, op, e1)), cnf) tecnfs
    | Ecall (f, exprs) ->
      eval_call ~typ e cnf ctx
    | Elist ls ->
      let ls = List.fold_left (fun acc e ->
        (* List of possible values of e, because e may be a function *)
        let teo, _ = expr_eval ~typ e cnf ctx |> List.hd in
        let te = Option.get teo in
        te::acc) [] ls in
      let ls = List.rev ls in
      [ Some (Tlist ls), cnf ]
    | Edict ed ->
      let td = SMap.fold (fun k e acc ->
        (* List of possible values of e, because e may be a function *)
        let teo, _ = expr_eval ~typ e cnf ctx |> List.hd in
        let te = Option.get teo in
        SMap.add k te acc) ed SMap.empty in
      [ Some (Tdict td), cnf ]
    | Eattrib (v, expr) ->
      (* Get keys and attributes *)
      (* let attrs, _ = parse_subscript e ([v],[]) in *)
      (* eval_subscript_aux ~typ attrs [] (E.Tvar (Taint.empty,v)) None ctx in *)
      failwith "to fix simple eattrib"
    | Esubscript (expr, c) ->
      eval_subscript ~typ e cnf ctx
    | Ejoined values ->
      if !Utils.debug then Printf.printf "expr_eval: Joined of length %d\n" (List.length values);
      let te = D.new_tsymbol "joined" in
      let (te, cnf): E.texpr * config = List.fold_left (fun (te,cnf) e ->
        let (teo',cnf') = expr_eval e cnf ctx |> List.hd in
        let t = try Option.get teo' |> E.get_taint with _ -> T.empty in
        let teo = E.add_taint te t in
        (teo,cnf')) (te, cnf) values in
      [Some te, cnf]

(** This function evaluates any function/method. All of these expressions start with the expression
    Ecall. The subexpression f might have attributes inside, or just an Evar expression. *)
and eval_call ?(typ=None) (e: E.expr) (cnf: config) (ctx: context) : (E.texpr option * config) list =
  match e with
  | Ecall (f, inpls) ->
    (* 1st case; hd of attr chain is a variable. *)
    (* This case implies that there is a method call. *)
    (* The head is stored in the last attr field *)
    (* For now, I will only accept programs that have attr chain of at most length 1. *)
    let hd = E.get_attr_name f in
    let te = Svm.find_opt hd (D.get_svm cnf.state) in
    if Option.is_some te then
      let f = E.get_last_attr f in
      Printf.printf "This is hd = %s and f = %s\n\n" hd f;
      (* If te is an object, try to search for method *)
      let te = Option.get te in
      match (te: E.texpr) with
      | Tobj (Some cls,tmap) ->
        begin try (* to find class and method *)
          Printf.printf "We have object\n";
          (* 1. Get function from class *)
          let c = SMap.find cls ctx.prog.classes in
          let f' = SMap.find f c.methods in
          (* 2. First argument of function is self, create temporary state *)
          let cnf_method = enter_method ~obj:(Some te) (f', inpls) cnf ctx in
          (* 3. Evaluate with temporary state *)
          if Flag.check_flag Flag.FINAL cnf_method.flag then
            [ None, { cnf_method with block=[] } ]
          else
            let ls = exec_block cnf_method ctx in
            let state = cnf.state in
            List.map (fun_taint_output (f,inpls) { cnf with state } ctx) ls
        with Not_found -> (* if class doesn't exist, check signatures *)
          Printf.printf "Could not find well-defined class\n";
          try
            (* Use new function fun_taint_input *)
            let fname = (cls ^ "." ^ f) in
            let te_ls, cnf = eval_input_list inpls cnf ctx in (* has to be a list of the input of the function evaluated. *)
            let sign = S.find ctx.sign fname in
            let flag, te_ls = fun_taint_input (Option.get sign) te_ls fname Lo.location_temp cnf ctx in
            if Flag.check_flag Flag.FINAL flag then
              (* In this next line I need to connect the argument in te_ls that has met the sink to the
                state of cnf *)
              let state = List.fold_left (fun acc (v,te) -> D.update_elt v te acc) cnf.state te_ls in
              [ None, { cnf with state ; block=[] ; flag } ]
            else
              let block = (Lo.location_temp, L.SymbolicNew f) :: cnf.block in
              (* Need to use taint from signature output *)
              let taint  = try Some (S.get_output (Option.get sign) |> Option.get |> T.source_2t) with _ -> None in
              let teo, cnf = stmt_symbolic_new ~typ ~taint { cnf with block } ctx in
              [ teo, cnf ]
          with Not_found ->
            Printf.printf "Could not find signature\n";
            let block = (Lo.location_temp, L.SymbolicNew f) :: cnf.block in
            (* Need to use taint from signature output *)
            let teo, cnf = stmt_symbolic_new ~typ { cnf with block } ctx in
            [ teo, cnf ]
        end
      | _ ->
        (* If te is a variable, we fail for now, but might want to cast later. *)
        failwith "eval_call: Not an object"
    (* Turns f into string of the shape "attr.attr ..." *)
    else
      begin
        let f_flat = E.flatten_attr f in
        if !Utils.debug then Printf.printf "expr_eval: calling proc %s\n" f_flat;
        (* Check if it's class constructor *)
        if Utils.is_class_constructor f_flat then
          (* 2nd case; function is class constructor. *)
          (* Search for the class, and if it's defined, call definition, if not return empty object with
            class name. *)
          begin try
            Printf.printf "It's a class constructor\nThere is a bug here, we are not checking the input sinks\n";
            let clss = SMap.find f_flat ctx.prog.classes in
            let cnf' = init ctx in
            let cnf' = { cnf' with block=L.block_to_ext clss.body } in
            let cnf' = exec_block cnf' ctx |> List.hd |> snd in
            (* Get svm *)
            let m = cnf'.state.svm in
            [ Some (Tobj (Some f_flat, m)), cnf ]
          with Not_found ->
            Printf.printf "Class not found.\n";
            (* Check for signature of construct in ctx.sign *)
            Printf.printf "f_flat = %s\n" f_flat;
            let sign_f = Signature.find ctx.sign f_flat in
            (* Evaluate inpls *)
            let te_ls, cnf = eval_input_list inpls cnf ctx in
            (* In the signature, look for sinks and sources. *)
            let flag, te_ls =
              if Option.is_some sign_f then
                fun_taint_input (Option.get sign_f) te_ls f_flat Lo.location_temp cnf ctx
              else
                cnf.flag, [] in
            (* If there is a sink, we return a FINAL flag. *)
            if Flag.check_flag Flag.FINAL flag then
              begin
                Printf.printf "There is a sink in the constructor.\n";
                (* Update SVM with te_ls *)
                let state = List.fold_left (fun acc (v,te) -> D.update_elt v te acc) cnf.state te_ls in
                [ None, { cnf with state ; block=[] ; flag } ]
              end
            else
              [ Some (Tobj (Some f_flat, SMap.empty)), { cnf with flag}]
          end
        else
          (* 3rd case; Is just some function, maybe from a library. *)
          let cnf_f = enter_fun (f_flat,inpls) cnf ctx in
          let state = cnf_f.state in
          if !Utils.debug then Printf.printf "expr_eval: is cnf final? %b\n" (Flag.check_flag Flag.FINAL cnf_f.flag);
          if Flag.check_flag Flag.FINAL cnf_f.flag then
            [ None, { cnf_f with block=[] } ]
          else
            let ls = exec_block cnf_f ctx in
            (* Printf.printf "expr_eval: ls length = %d\n" (List.length ls); *)
            List.map (fun_taint_output (f_flat,inpls) { cnf with state } ctx) ls
      end
  | _ -> failwith "eval_call: wrong expression."

(* and eval_attrib ?(typ=None) (expr: E.expr) (cnf: config) (ctx: context) : (E.texpr option * config) list =
  match expr with
  | Eattrib (v, Evar(attr)) -> begin
    match D.find_limited v cnf.state with
    | Tobj (clssname,tmap), state ->
      let te, tmap = (* We also return tmap because it may grow. *)
        try
          SMap.find attr tmap, tmap (* If attrib already exists, we don't do any guessing. *)
        with Not_found ->
          let taint = get_taint_clss clssname attr ctx in
          add_symb_tmap tmap attr taint in
    (* We need to update posibbly bigger tmap *)
    let state = D.update_elt v (Tobj (clssname,tmap)) state in
    [ Some te, { cnf with state } ]
    | _ -> failwith "expr_eval: trying to access an attribute of a non-object value."
  end
  | _ ->
    failwith "expr_eval: expression on eattrib is not a var." *)

and eval_subscript ?(typ=None) (expr: E.expr) (cnf: config) (ctx: context) : (E.texpr option * config) list =
  let attrs, keys = parse_subscript expr ([],[]) in
  (* 1. Squash ATTR list *)
  (* 2. Squash KEY list *)
  match expr with
  | Esubscript (attr, key) ->
    if E.is_int key then
      (* This line of code is wrong. Creates a stack overflow *)
      let (teo, cnf): E.texpr option * config = expr_eval attr cnf ctx |> List.hd in
      let te = Option.get teo in
      let ls = E.get_list te in
      let i = E.get_int key in
      let te = List.nth ls i in
      [ Some te, cnf ]
    else
      let keys = List.map E.get_str keys in
      (* We create fake initial obj with the SVM *)
      let svm = D.get_svm cnf.state in
      (* Printf.printf "after parsing subscript: keys=%d khd=%s attrs=%d ahd=%s\n" (List.length keys) (List.hd keys) (List.length attrs) (List.hd attrs); *)
      let te, obj = eval_subscript_aux ~typ attrs keys (E.Tdict svm) None None ctx in
      (* Printf.printf "te=%a ; obj=%a\n" E.pp_texpr te E.pp_texpr obj; *)
      let state = D.set_svm (E.get_tmap obj) cnf.state in
      [ Some te, { cnf with state } ]
  | _ -> failwith "eval_subscript: not a subscript."

and eval_subscript_aux ?(typ=None) (attrs: string list) (keys: string list) (obj: E.texpr) (carried_taint: T.t option) (carried_clssname: string option) (ctx: context) : E.texpr * E.texpr =
  match attrs with
  | [] -> failwith "eval_subscript_attrs: empty attrs"
  | [a] ->
    (* Create var or dict *)
    let rec crunch_keys keys obj sign_taint : E.texpr * E.texpr =
      (* E.get_tmap might fail *)
      let obj_map, taint = try E.get_tmap obj, None with _ -> SMap.empty, Some (E.get_taint obj) in
      if List.length keys = 0 then
        failwith "eval_subscript_attrs: empty keys"
      else if List.length keys = 1 then
        (* Get last part *)
        let key = List.hd keys in
        (* Printf.printf "BASE CASE key=%s\n" key; *)
        let te, obj_map =
          try SMap.find key obj_map, obj_map
          with Not_found ->
            (* E.Tdict SMap.empty in *)
            let taint = T.comb_option taint carried_taint |> T.comb_option sign_taint in
              (* try Some (T.comb (Option.get taint) (Option.get carried_taint)) with _ -> taint in *)
            let te = D.new_tsymbol ~typ ~taint key in
            te, SMap.add key te obj_map in
        let obj = E.Tdict obj_map in
        te, obj
      else
        (* Create recursive dicts *)
        let key = List.hd keys in
        Printf.printf "REC CASE key=%s\n" key;
        let keys = List.tl keys in
        let obj' =
          try SMap.find key obj_map
          with Not_found -> E.Tdict SMap.empty in
            (* D.new_tsymbol ~typ ~taint:carried_taint key in *)
        let te, obj' = crunch_keys keys obj' sign_taint in
        let obj_map = SMap.add key obj' obj_map in
        let obj: E.texpr = Tdict obj_map in
        te, obj in
    let obj_map = E.get_tmap obj in
    let obj_a = try SMap.find a obj_map with _ -> E.Tdict SMap.empty in
    let taint = get_taint_clss carried_clssname a ctx in
    let te, obj_a = crunch_keys keys obj_a taint in
    let obj = E.Tdict (SMap.add a obj_a obj_map) in
    te, obj
  | a::attrs ->
    let obj_map = E.get_tmap obj in
    let obj_name = SMap.find a obj_map in
    let clssname = E.get_clssname_option obj_name in
    let taint = get_taint_clss clssname a ctx in
    let taint = T.comb_option carried_taint taint in
    let te, obj_sub = eval_subscript_aux attrs keys obj_name taint clssname ctx in
    let obj_map = SMap.add a obj_sub obj_map in
    let obj: E.texpr = try
        Tobj (clssname, obj_map)
      with E.Vague_obj _ ->
        Tobj (None, obj_map) in
    te, obj
    (* failwith "TODO case more attribs" *)

(*
(** This function should only be used for when *)
and eval_subscript_attrs ?(typ=None) (attrs: string list) (keys: string list) (obj: E.texpr) (carried_taint: T.t option) (ctx: context) : E.texpr * E.texpr =
  if List.length attrs = 0 then
    (* BASE CASE: obj is a dictionary *)
    match keys, obj with
    | [key], Tdict obj_map ->
      let te =
        try SMap.find key obj_map
        with Not_found -> D.new_tsymbol ~typ ~taint:carried_taint key in
      let obj_map = SMap.add key te obj_map in
      te, (Tdict obj_map)
    | [key], Tvar (taint,sv) ->
      (* It's Tvar, replace with Tdict *)
      let taint = try T.comb taint (Option.get carried_taint) with _ -> taint in
      let te = D.new_tsymbol ~typ ~taint:(Some taint) key in
      let obj_map = SMap.add key te SMap.empty in
      te, Tdict obj_map
    | _ ->
      failwith "eval_subscript: unexpected expression."
  else
    (* Elements in attrs are all the atributes until the last element which has no sub-attributes *)
    (* Last element will be treated as a variable *)
    match attrs with
    | [] -> failwith "eval_subscript_aux: empty attrs"
    | [x] ->
      (* VAR case *)


    | a::x::attrs -> (* ATTR case *) a, (x::attrs)
    (* let name, new_attr =
      match attr with
      | E.Evar name -> name, None
      | E.Eattrib (name, attr) -> name, Some attr
      | _ ->
        Printf.printf "eval_subscript_aux: unexpected, attr=%a\n" E.pp_expr attr;
        failwith "eval_subscript_aux: unexpected." in *)
    (*
    let obj_map = E.get_tmap obj in
    let obj_name = try SMap.find name obj_map
      with Not_found ->
        E.Tdict SMap.empty in
    let clssname = E.get_clssname_option obj in
    let taint = get_taint_clss clssname name ctx in
    let taint = try Some (T.comb (Option.get carried_taint) taint) with _ -> Some taint in
    let te, te_a = eval_subscript_aux ~typ attrs keys obj_name taint ctx in
    let obj_map = SMap.add name te_a obj_map in
    let obj: E.texpr = try
        Tobj (Some (E.get_clssname obj), obj_map)
      with E.Vague_obj _ ->
        Tobj (None, obj_map) in
    te, obj *)

(*
and eval_subscript_aux (key: string) (obj: E.texpr) (attr: E.expr option) (carried_taint: T.t option) (ctx: context) : E.texpr * E.texpr =
  match attr with
  | None -> begin
    (* BASE CASE: obj is a dictionary *)
    (* TODO: what if E.get_tmap fails? what is SMap.find fails? *)
    (* let obj_map = E.get_tmap obj in
    let te = SMap.find key obj_map in *)
    match obj with
    | Tdict obj_map ->
      let te =
        try SMap.find key obj_map
        with Not_found -> D.new_tsymbol ~taint:carried_taint key in
      let obj_map = SMap.add key te obj_map in
      te, (Tdict obj_map)
    | Tvar (taint,sv) ->
      (* It's Tvar, replace with Tdict *)
      let taint = try T.comb taint (Option.get carried_taint) with _ -> taint in
      let te = D.new_tsymbol ~taint:(Some taint) key in
      let obj_map = SMap.add key te SMap.empty in
      te, Tdict obj_map
    | _ ->
      failwith "eval_subscript: unexpected expression."
  end
  | Some (Evar a) -> begin
    (* BASE CASE: obj is an object *)
    let obj_map = E.get_tmap obj in (* obj can be either a object or a dictionary *)
    let te_a =
      try
        SMap.find a obj_map (* This is a dictionary. What if it doesn't exist yet? *)
      with Not_found ->
        E.Tdict SMap.empty in
    (* CHECK FOR TAINT IN CLASS SIGNATURE*)
    let clssname = E.get_clssname_option obj in
    let taint = get_taint_clss clssname a ctx in
    let taint = try Some (T.comb (Option.get carried_taint) taint) with _ -> Some taint in
    let te, te_a = eval_subscript_aux key te_a None taint ctx in
    let obj_map = SMap.add a te_a obj_map in
    let obj: E.texpr = try
        Tobj (Some (E.get_clssname obj), obj_map)
      with E.Vague_obj _ ->
        Tobj (None, obj_map) in
    te, obj
  end
  | Some (Eattrib (name, attr)) -> begin (* RECURSIVE CASE*)
    let obj_map = E.get_tmap obj in
    let obj_name = SMap.find name obj_map in
    let clssname = E.get_clssname_option obj in
    let taint = get_taint_clss clssname name ctx in
    let taint = try Some (T.comb (Option.get carried_taint) taint) with _ -> Some taint in
    let te, obj_sub = eval_subscript_aux key obj_name (Some attr) taint ctx in
    let obj_map = SMap.add name obj_sub obj_map in
    let obj: E.texpr = try
        Tobj (Some (E.get_clssname obj), obj_map)
      with E.Vague_obj _ ->
        Tobj (None, obj_map) in
    te, obj
  end
  | Some _ ->
    failwith "eval_subscript_aux: unexpected attribute."
*) *)

and stmt_assign cnf ctx : config list =
  match List.hd cnf.block |> snd with
  | Assign (v,typ,e) ->
    let tecnfs = expr_eval ~typ e cnf ctx in
    let tecnfs = List.map (fun (te,cnf_e) ->
      (* Take incoming cnf for gaining knowledge *)
      if Option.is_some te then begin
        let te = Option.get te in
        let block = List.tl cnf.block in
        match v with
        | Name v ->
          let state = D.update_elt v te cnf_e.state in
          { cnf with state ; block }
        | Subscript (v,i) ->
          begin try
            let i = expr_eval i cnf ctx |> List.hd |> fst |> Option.get |> E.texpr_2expr |> E.expr_2int in
            let ls = D.find v cnf.state |> fst |> E.get_list in
            let ls = E.Tlist (Aux.list_nth_replace i te ls) in
            let state = D.update_elt v ls cnf_e.state in
            { cnf with state ; block }
          with E.Not_a_number ->
            (* It must be a dictionary *)
            let state = update_lexpr (Subscript (v,i)) te cnf_e.state in
            { cnf with state ; block }
          end
        | Attrib (v,a) ->
          let state = update_lexpr (Attrib (v,a)) te cnf_e.state in
          { cnf with state ; block }
      end else if Flag.check_flag Flag.FINAL cnf_e.flag then
        { cnf_e with block=[] }
      else
        failwith "stmt_assign: eval is None, and no FINAL flag.") tecnfs in
    tecnfs
  | _ ->
    Printf.printf "block: %a\n" (Prog.pp_block_ext "") cnf.block;
    failwith "stmt_assign: wrong block"

and stmt_return cnf ctx : (E.texpr option * config) list =
  match List.hd cnf.block |> snd with
  | Return e ->
    let tecnfs = expr_eval e cnf ctx |> List.map (fun (te,cnf) -> te, {cnf with block=[] }) in
    tecnfs
  | _ ->
    failwith "stmt_return: wrong block"

and stmt_expr cnf ctx : E.texpr option * config =
  match List.hd cnf.block |> snd with
  | Expr e ->
    let tecnfs = expr_eval e cnf ctx in
    begin match tecnfs with
    | [ (te,cnf') ] ->
      (* TODO check this line *)
      Printf.printf "stmt_expr: one expression returned\n";
      if Flag.check_flag Flag.FINAL cnf'.flag then
        te, { cnf' with block=[] }
      else
        let block = try List.tl cnf.block with _ -> [] in
        te, { cnf with block }
    | (te,cnf')::_ ->
      Printf.printf "stmt_expr: WARNING!!! more than one expression returned\n";
      if Flag.check_flag Flag.FINAL cnf'.flag then
        te, { cnf' with block=[] }
      else
        let block = try List.tl cnf.block with _ -> [] in
        te, { cnf with block }
    | [] -> failwith "stmt_expr: list is empty."
    end
  | _ ->
    Printf.printf "block: %a\n" (Prog.pp_block_ext "") cnf.block;
    failwith "stmt_expr: wrong block"

and stmt_symbolic_new ?(typ=None) ?(taint=None) cnf ctx : E.texpr option * config =
  match List.hd cnf.block |> snd with
  | SymbolicNew v ->
    let te = D.new_tsymbol ~typ ~taint v in
    let block = List.tl cnf.block in
    Some te, { cnf with block }
  | _ ->
    Printf.printf "block: %a\n" (Prog.pp_block_ext "") cnf.block;
    failwith "stmt_input: wrong block"

and stmt_if cnf ctx : (bool -> config option) list =
  let stmt = List.hd cnf.block in
  let line_number = (fst stmt).start in
  match snd stmt with
  | If (e,b0,b1) ->
    let tecnfs = expr_eval (* ~typ:(Some L.Bool) *) e cnf ctx in
    List.fold_left (fun acc (te,cnf) ->
    let te = try Option.get te with Invalid_argument _ -> failwith "stmt_if: te is None" in
    let t_branch = begin match D.check_argument cnf.state te |> fst with
      | SAT ->
        if !(Utils.debug) then Printf.printf "stmt_if t_branch SAT\n";
        let state = D.add_constraint ~line_number te cnf.state in
        let block = b0 @ List.tl cnf.block in
        let id = Id.add_message ["IF true"] cnf.id in
        Some { cnf with state ; block ; id }
      | _ ->
        if !(Utils.debug) then Printf.printf "stmt_if t_branch UNSAT\n";
        None
      end in
    let nte = E.texpr_neg te in
    let f_branch = begin match D.check_argument cnf.state nte |> fst with
      | SAT ->
        if !(Utils.debug) then Printf.printf "stmt_if f_branch SAT\n";
        let state = D.add_constraint ~line_number nte cnf.state in
        let block = b1 @ List.tl cnf.block in
        let id = Id.make ~message:("IF false"::(Id.get_message cnf.id)) false in
        Some { cnf with state ; block ; id}
      | _ ->
        if !(Utils.debug) then Printf.printf "stmt_if f_branch UNSAT\n";
        None
      end in
    (fun b -> match b with true -> t_branch | false -> f_branch)::acc) [] tecnfs
  | _ ->
    Printf.printf "block: %a\n" (Prog.pp_block_ext "") cnf.block;
    failwith "stmt_if: wrong block"

and stmt_for cnf ctx : (bool -> config option) list =
  let stmt = List.hd cnf.block in
  let line_number = (fst stmt).start in
  match snd stmt with
  | For (v,e,b) ->
    (* e is the guard *)
    (* e evaluates to an iterable *)
    (* either e is a constant list, or a 'symbol' *)
    let tecnfs = expr_eval e cnf ctx in
    List.fold_left (fun acc (te,cnf) ->
      let te = try Option.get te with Invalid_argument _ -> failwith "stmt_for: te is None" in
      let taint = E.get_taint te in
      let guard = E.get_expr te in
      let hd = E.expr_2texpr (E.hd guard) taint in
      let empty = E.Econst Cempty in
      let t_te = E.Tbop (taint,empty,Oeq,guard) in
      let f_te = E.Tbop (taint,empty,One,guard) in
      let t_branch = begin match D.check_argument cnf.state t_te |> fst with
        | SAT ->
          if !(Utils.debug) then Printf.printf "stmt_for: t_branch SAT\n";
          let state = D.add_constraint ~line_number t_te cnf.state in
          let svm = Svm.add v hd (D.get_svm state) in
          let state = D.set_svm svm state in
          let block = b @ [ (Lo.location_temp, L.Clear v) ] @ List.tl cnf.block in
          let id = Id.add_message ["FOR true"] cnf.id in
          Some { cnf with state ; block ; id }
        | _ ->
          if !(Utils.debug) then Printf.printf "stmt_for: t_branch UNSAT\n";
          None
        end in
      let f_branch = begin match D.check_argument cnf.state f_te |> fst with
        | SAT ->
          if !(Utils.debug) then Printf.printf "stmt_for: f_branch SAT\n";
          let state = D.add_constraint ~line_number f_te cnf.state in
          let block = List.tl cnf.block in
          let counter = C.delete cnf.counter in
          let id = Id.make ~message:("FOR false"::(Id.get_message cnf.id)) false in
          Some { cnf with state ; block ; id ; counter }
        | _ ->
          if !(Utils.debug) then Printf.printf "stmt_for: f_branch UNSAT\n";
          None
        end in
      (fun b -> match b with true -> t_branch | false -> f_branch)::acc) [] tecnfs
  | _ -> failwith "stmt_for: wrong block."

and stmt_while cnf ctx : (bool -> config option) list =
  let stmt = List.hd cnf.block in
  let line_number = (fst stmt).start in
  match snd stmt with
  | Loop (e,b) | While (e,b) ->
    let tecnfs = expr_eval e cnf ctx in
    List.fold_left (fun acc (te,cnf) ->
      let te = try Option.get te with Invalid_argument _ -> failwith "stmt_while: te is None" in
      let t_branch = begin match D.check_argument cnf.state te |> fst with
        | SAT ->
          if !(Utils.debug) then Printf.printf "stmt_while: t_branch SAT\n";
          let state = D.add_constraint ~line_number te cnf.state in
          let block = b @ ((fst stmt, Loop (e,b)) :: List.tl cnf.block) in
          let id = Id.add_message ["WHILE true"] cnf.id in
          Some { cnf with state ; block ; id }
        | _ ->
          if !(Utils.debug) then Printf.printf "stmt_while: t_branch UNSAT\n";
          None
        end in
      let nte = E.texpr_neg te in
      let f_branch = begin match D.check_argument cnf.state nte |> fst with
        | SAT ->
          if !(Utils.debug) then Printf.printf "stmt_while: f_branch SAT\n";
          let state = D.add_constraint ~line_number nte cnf.state in
          let block = List.tl cnf.block in
          let counter = C.delete cnf.counter in
          let id = Id.make ~message:("WHILE false"::(Id.get_message cnf.id)) false in
          Some { cnf with state ; block ; counter ; id }
        | _ ->
          if !(Utils.debug) then Printf.printf "stmt_while: f_branch UNSAT\n";
          None
        end in
      (fun b -> match b with true -> t_branch | false -> f_branch)::acc) [] tecnfs
  | _ ->
    Printf.printf "block: %a\n" (Prog.pp_block_ext "") cnf.block;
    failwith "stmt_while: wrong block"

and stmt_loop_max cnf ctx : (config option) list =
  let stmt = List.hd cnf.block in
  let line_number = (fst stmt).start in
  match snd stmt with
  | Loop (e,b) ->
    let tecnfs = expr_eval e cnf ctx in
    List.fold_left (fun acc (te,cnf) ->
    let te = try Option.get te with Invalid_argument _ -> failwith "stmt_loop_max: te is None" in
    let nte = E.texpr_neg te in
    let f_branch = begin match D.check_argument cnf.state nte |> fst with
      | SAT ->
        if !(Utils.debug) then Printf.printf "stmt_if f_branch SAT\n";
        let state = D.add_constraint ~line_number nte cnf.state in
        let block = List.tl cnf.block in
        Some { cnf with state ; block }
      | _ ->
        if !(Utils.debug) then Printf.printf "stmt_if f_branch UNSAT\n";
        None
      end in
    f_branch::acc) [] tecnfs
  | _ ->
    Printf.printf "block: %a\n" (Prog.pp_block_ext "") cnf.block;
    failwith "stmt_loop: wrong block"

and stmt_constr cnf ctx : config list =
  let stmt = List.hd cnf.block in
  let line_number = (fst stmt).start in
  match snd stmt with
  | Constr e ->
    let tecnfs = expr_eval e cnf ctx in
    List.fold_left (fun acc (te,cnf) ->
    let te = try Option.get te with Invalid_argument _ -> failwith "stmt_constr: te is None" in
    begin match D.check_argument cnf.state te |> fst with
    | SAT ->
      let state = D.add_constraint ~line_number ~important:true te cnf.state in
      let block = List.tl cnf.block in
      [{ cnf with state ; block }]
    | _ -> []
    end@acc) [] tecnfs
  | _ ->
    Printf.printf "block: %a\n" (Prog.pp_block_ext "") cnf.block;
    failwith "stmt_constr: wrong block"

and stmt_clear cnf ctx : E.texpr option * config =
  let stmt = List.hd cnf.block in
  match snd stmt with
  | Clear v ->
    let state = D.update_elt_opt v (fun _ -> None) cnf.state in
    let block = List.tl cnf.block in
    None, { cnf with state ; block }
  | _ ->
    failwith "stmt_clear: wrong block"

(** Execute head of configuration's block. *)
and exec_stmt cnf ctx : (E.texpr option * config) list =
  if List.length cnf.block = 0  || Flag.check_flag Flag.FINAL cnf.flag then [ None, cnf ] else
  let lc, stmt = List.hd cnf.block in
  let tcounter = C.step cnf.block cnf.counter in
  if !Utils.debug then
    Printf.printf "State %a: C -> %b,%a\n%aSTMT:\n%aBLOCK:\n%a\n" (Id.pp "") cnf.id (fst tcounter) C.pp (snd tcounter) (D.pp "  ") cnf.state (Prog.pp_stmt_ext "  ") stmt (Prog.pp_block_ext "  ") (List.tl cnf.block);
  flush stdout;
  let conf_post : (E.texpr option * config) list = match tcounter, stmt with
    | (true, counter), Assign _ ->
        List.map (fun cnf -> None, cnf) (stmt_assign { cnf with counter } ctx)
    | (true, counter), If _ ->
        let os = stmt_if { cnf with counter } ctx in
        List.fold_left (fun acc o ->
        let t_branch = try [ o true  |> Option.get ] with _ -> [] in
        let f_branch = try [ o false |> Option.get ] with _ -> [] in
        let tf = List.map (fun cnf -> None, cnf) (t_branch @ f_branch) in
        tf@acc) [] os
    | (true, counter), Constr _ ->
        List.map (fun cnf -> None, cnf) (stmt_constr cnf ctx)
    | (true, counter), Fun _ -> failwith "exec_stmt: funtion call is not yet implemented."
    | (true, counter), Return _ ->
        stmt_return { cnf with counter } ctx
    | (true, counter), Expr _ ->
        [ stmt_expr { cnf with counter } ctx ]
    | (true, counter), For _ ->
        let os : (bool -> config option) list = stmt_for { cnf with counter } ctx in
        List.fold_left (fun acc o ->
          let t_branch = try [ o true  |> Option.get ] with _ -> [] in
          let f_branch = try [ o false |> Option.get ] with _ -> [] in
          let tf  = List.map (fun cnf -> None, cnf) (t_branch @ f_branch) in
          tf@acc) [] os
    | (true, counter), While _
    | (true, counter), Loop _ ->
        let os = stmt_while { cnf with counter } ctx in
        List.fold_left (fun acc o ->
          let t_branch = try [ o true  |> Option.get ] with _ -> [] in
          let f_branch = try [ o false |> Option.get ] with _ -> [] in
          let tf  = List.map (fun cnf -> None, cnf) (t_branch @ f_branch) in
          tf@acc) [] os
    | (false, counter), Loop _ ->
        (* Stopping loop forcibly *)
        if !(Utils.debug) then Printf.printf "exec_stmt: loop reached max unfolds. Analyzer will try false branch.\n";
        let os = stmt_loop_max { cnf with counter } ctx in
        List.fold_left (fun acc f_branch ->
          if Option.is_some f_branch then (None, Option.get f_branch)::acc
          else acc) [] os
    | (true, counter), SymbolicNew _ ->
      [ stmt_symbolic_new cnf ctx ]
    | (true, counter), Clear _ ->
      [ stmt_clear cnf ctx ]
    | (false, _), _ -> failwith "exec_stmt: counter/block discrepancy." in
    conf_post

(** Execute list of configurations until all configurations are final. *)
and exec_list (cnfs: config list) ctx : (E.texpr option * config) list =
  let rec aux (cnfs: config list) (final_cnfs: (E.texpr option * config) list) = (
  match cnfs with
  | [] -> final_cnfs
  | cnf::cnfs ->
    let ls = exec_stmt cnf ctx in
    (* Separate final configs from others *)
    let ls_final, ls_cont = List.fold_left
      (fun (ls_final, ls_cont) oecnf ->
        match List.length (snd oecnf).block = 0 with
        | true  -> (oecnf :: ls_final), ls_cont
        | false -> ls_final, (snd oecnf::ls_cont))
      ([],[]) ls in
    aux (cnfs @ ls_cont) (ls_final @ final_cnfs)) in
  aux cnfs []

(** Execute configuration. *)
and exec_block (cnf: config) ctx : (E.texpr option * config) list =
  if Flag.check_flag Flag.FINAL cnf.flag then
    [ None, cnf ]
  else
    exec_list [ cnf ] ctx

let check_illegal_flows (cnf: config) (_: context) : bool * T.rule list =
  let svm = D.get_svm cnf.state in
  let ro = Svm.fold (fun v te acc -> E.is_rule te @ acc) svm [] in
  if List.length ro > 0 then
    true, ro
  else
    false, ro

let analyze_prog ?(json=false) filename ctx =
  let cnf_init = init ctx in
  let cnf_final = exec_block cnf_init ctx in
  let cnf_final = List.map (fun (ote,cnf) -> ote, { cnf with id=Id.add_message ["Final"] cnf.id }) cnf_final in
  let cnf_final = List.fold_left (fun acc (ote,cnf) ->
    let c = D.check cnf.state in
    match fst c with
    | SAT -> (snd c,ote,cnf)::acc
    | _ -> acc) [] cnf_final in
  Printf.printf "\nFinal branches for %s: %d\n" filename (List.length cnf_final);
  if json then List.iter
    (fun (zsolver,ote,cnf) ->
      let is_iflow, rules = check_illegal_flows cnf ctx in
      if is_iflow then
        Printf.printf "FINAL STATE, RULE TRIGGERED:\n   id: %a\n%a" (Id.pp "") cnf.id (D.pp "  ") cnf.state
      else
        Printf.printf "FINAL STATE:\n   id: %a\n%a" (Id.pp "") cnf.id (D.pp "  ") cnf.state;
      if is_iflow then begin
        Printf.printf "Rule %d triggered. '%s'\n" (List.hd rules).code (List.hd rules).name;
        let model = SMT.get_string_model zsolver in
        if Option.is_some model then
          let model = Option.get model in
          if String.length model > 0 then
            Printf.printf "MODEL:\n%s\n" model
          else
            Printf.printf "Trace can be reached with any initial memory.\n"
        else
          Printf.printf "Model cannot be generated.\n"
      end;
      if Option.is_some ote then
        Printf.printf "Return value: %a\n" E.pp_texpr (Option.get ote)
      else
        Printf.printf "No return value.\n";
      Printf.printf "\n")
    cnf_final;
  cnf_final
