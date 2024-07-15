%{
  module E = Expr.TExpr
  module Ops = Expr.Ops
  module L = Syntax.TLang
%}
%token <string> NAME
%token <int>    INT

%token C_TRUE C_FALSE

%token O_ADD O_SUB O_MUL O_DIV O_MOD
%token O_LE  O_LT O_GE O_GT O_EQ O_NE
%token O_AND O_OR

%token K_ASSERT K_BOOL K_ELSE K_IF K_INPUT K_INT K_MAIN K_SEC K_SINK K_SOURCE K_TAINT K_VIOL K_WHILE

%token LARROW LPAR RPAR LBRACE RBRACE COLON SEMICOLON COMMA
%token EOF
%type <Syntax.TLang.stmt>  stmt
%type <Syntax.TLang.block> block
%type <Prog_taint.t>  prog
%start stmt block prog
%%

var:
| NAME              { $1 }
expr:
| expr0             { $1 }
expr0:
| expr0 O_OR expr1  { E.Eop ($1, Ops.Oor, $3) }
| expr1             { $1 }
expr1:
| expr1 O_AND expr2 { E.Eop ($1, Ops.Oand, $3) }
| expr2             { $1 }
expr2:
| expr2 O_EQ expr3  { E.Eop ($1, Ops.Oeq, $3) }
| expr2 O_NE expr3  { E.Eop ($1, Ops.One, $3) }
| expr2 O_LE expr3  { E.Eop ($1, Ops.Ole, $3) }
| expr2 O_LT expr3  { E.Eop ($1, Ops.Olt, $3) }
| expr2 O_GE expr3  { E.Eop ($1, Ops.Oge, $3) }
| expr2 O_GT expr3  { E.Eop ($1, Ops.Ogt, $3) }
| expr3             { $1 }
expr3:
| expr3 O_ADD expr4 { E.Eop ($1, Ops.Oadd, $3) }
| expr3 O_SUB expr4 { E.Eop ($1, Ops.Osub, $3) }
| expr4             { $1 }
expr4:
| expr4 O_MUL expr5 { E.Eop ($1, Ops.Omul, $3) }
| expr4 O_DIV expr5 { E.Eop ($1, Ops.Odiv, $3) }
| expr4 O_MOD expr5 { E.Eop ($1, Ops.Omod, $3) }
| expr5             { $1 }
expr5:
| C_TRUE                 { E.Ecstb true }
| C_FALSE                { E.Ecstb false }
| INT                    { E.Ecsti $1 }
| var                    { E.Evar $1 }
| K_SOURCE LPAR var COMMA source RPAR { E.Esource ($3,$5) }
| LPAR expr0 RPAR        { $2 }

decl:
| K_BOOL var SEMICOLON { Syntax.Var.make $2 Tbool }
| K_INT  var SEMICOLON { Syntax.Var.make $2 Tint }

funname:
| NAME LPAR RPAR SEMICOLON { $1 }

sink:
| NAME { Taint.string_2sink $1 }
source:
| NAME { Taint.string_2source $1 }

stmt:
| var O_EQ expr SEMICOLON
    { L.Sassign ($1, $3) }
| K_INPUT LPAR var RPAR SEMICOLON
    { L.Sinput $3 }
| K_IF LPAR expr RPAR LBRACE block RBRACE K_ELSE LBRACE block RBRACE
    { L.Sif ($3, $6, $10) }
| K_WHILE LPAR expr RPAR LBRACE block RBRACE
    { L.Swhile ($3, $6) }
| K_SINK LPAR var COMMA sink RPAR SEMICOLON
    { L.Ssink ($3, $5)}
| var LARROW NAME LPAR RPAR SEMICOLON
    { L.Sfun ($3, Some $1, None) }
| var LARROW NAME LPAR var RPAR SEMICOLON
    { L.Sfun ($3, Some $1, Some $5) }
| NAME LPAR RPAR SEMICOLON
    { L.Sfun ($1, None, None) }
| NAME LPAR var RPAR SEMICOLON
    { L.Sfun ($1, None, Some $3) }


block:
|               { [ ] }
| stmt block    { Prog_taint.lc := !(Prog_taint.lc)+1; (!(Prog_taint.lc),$1) :: $2 }
// TODO: fix this 0

globals:
|               { [ ] }
| decl globals  { $1 :: $2 }

delta:
| var O_EQ INT SEMICOLON { ($1,$3) }

sec:
| { [ ] }
| delta sec { $1 :: $2 }

security:
| K_SEC COLON LBRACE sec RBRACE { $4 }
| { [ ] }

viol:
| LPAR NAME COMMA INT COMMA INT RPAR { ($2, $4, $6) }

viols:
| { [ ] }
| viol viols { $1 :: $2 }

violation:
| K_VIOL COLON LBRACE viols RBRACE { $4 }
| { [ ] }

taint:
| NAME { Taint.string_2taint $1 }

tainted:
| { [ ] }
| var COLON taint tainted { ($1, Some $3)::$4}

tainteds:
| K_TAINT COLON LBRACE tainted RBRACE { $4 }
| { [ ] }

proc_def:
| LBRACE block RBRACE { $2 }

proc:
| NAME COLON proc_def
    { { Prog_taint.pname=$1 ;
        Prog_taint.pbody=$3 ;
        Prog_taint.pinput=None ;
        Prog_taint.poutput=None} }
| NAME COLON NAME proc_def 
    { { Prog_taint.pname=$1 ;
        Prog_taint.pbody=$4 ;
        Prog_taint.pinput=None ;
        Prog_taint.poutput=Some (Taint.string_2taint $3) } }
| NAME LPAR NAME RPAR COLON proc_def
    { { Prog_taint.pname=$1 ;
        Prog_taint.pbody=$6 ;
        Prog_taint.pinput=Some (Taint.string_2taint $3) ;
        Prog_taint.poutput=None } }
| NAME LPAR NAME RPAR COLON NAME proc_def
    { { Prog_taint.pname=$1 ;
        Prog_taint.pbody=$7 ;
        Prog_taint.pinput=Some (Taint.string_2taint $3) ;
        Prog_taint.poutput=Some (Taint.string_2taint $6) } }

procs:
|               { [ ] }
| proc procs    { $1 :: $2 }

main:
| K_MAIN COLON proc_def { $3 }

prog:
| globals tainteds violation security procs main EOF
    { { Prog_taint.pglobs = $1;
        Prog_taint.psec   = Sec_map.of_list $4 $1 ;
        Prog_taint.funcs = $5 ;
        Prog_taint.pmain  = $6 ;
        Prog_taint.ptaint = $2 ;
        Prog_taint.pviol  = $3} }

