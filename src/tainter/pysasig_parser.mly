%{
  module S = Signature
%}
%token <string> NAME
// %token <int>    INT

%token K_DEF K_TSOURCE K_TSINK K_SANITIZE K_PARAMETERS K_TITO

%token EQ LARROW RARROW LPAR RPAR LBRACK RBRACK COLON STAR COMMA DOT THREEDOTS NUMERAL
%token EOF
%type <Signature.t list> signatures
%type <Signature.t> signature
%type <Signature.arg> arg
%start signatures
%%

source:
| NAME { Some (Taint.Source { name=$1 ; locs=[] }) }

output:
|                                     { None }
| RARROW K_TSOURCE LBRACK source RBRACK { $4 }

sink:
| K_TSINK LBRACK NAME RBRACK { Taint.Sink { name=$3 ; locs=[] } }

argname:
| NAME      { $1 }
| STAR NAME { $2 }

arg:
| argname            { { name=$1; taint=None ; annotation=None } }
| argname COLON sink { { name=$1; taint=(Some $3) ; annotation=None } }

args:
|                { [ ] }
| arg            { [ $1 ] }
| arg COMMA args { $1::$3 }

sanitize:
| K_SANITIZE LPAR K_PARAMETERS RPAR { Some Signature.Parameters }
| K_SANITIZE LPAR K_TITO RPAR       { Some Signature.TaintInTaintOut }
| K_SANITIZE                        { Some Signature.All }

signature:
| sanitize K_DEF NAME LPAR args RPAR output COLON THREEDOTS { SigFun { sanitize=$1 ; name=$3 ; input=(List.rev $5) ; output=$7 ; self=false } }
| K_DEF NAME LPAR args RPAR output COLON THREEDOTS { SigFun { sanitize=None ; name=$2 ; input=$4 ; output=$6 ; self=false } }
| NAME COLON K_TSOURCE LBRACK source RBRACK EQ THREEDOTS { SigAttr { name=$1 ; output=$5 }}

signatures:
|                      { [ ] }
| signature signatures { $1 :: $2 }
