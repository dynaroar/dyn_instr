%{
  open Cil
  open Common
%}

%token <int> INT
%token <float> FLOAT
%token <string> STRING
%token <string> ID
%token TRUE
%token FALSE
%token LPAREN
%token RPAREN
%token PLUS MINUS MULT DIV MOD
%token GT GE LT LE EQ NEQ
%token AND OR NOT
%token EOF

%left OR
%left AND
%right NOT 
%right EQ
%left LT LE GT GE
%left PLUS MINUS
%left MULT DIV MOD

%start <Cil.exp> inv
%%

inv:
  | e=expr; EOF { e }

expr:
  | LPAREN e=expr RPAREN { e }
  | i=INT { Cil.integer i }
  | s=STRING { Cil.mkString s }
  | f=FLOAT { Cil.Const (CReal (f, FFloat, None)) }
  | TRUE { Cil.one }
  | FALSE { Cil.zero }
  | id=ID { vi2e (Cil.makeVarinfo false id intType) }
  | op=unop e=expr { Cil.UnOp (op, e, intType) }
  | e1=expr op=binop e2=expr { Cil.BinOp (op, e1, e2, intType) }

%inline unop:
  | NOT { Cil.LNot }
  | MINUS { Cil.Neg }

%inline binop:
  | PLUS { Cil.PlusA }
  | MINUS { Cil.MinusA }
  | MULT { Cil.Mult }
  | DIV { Cil.Div } 
  | MOD { Cil.Mod }
  | LT { Cil.Lt }
  | LE { Cil.Le }
  | GT { Cil.Gt }
  | GE { Cil.Ge }
  | EQ { Cil.Eq }
  | NEQ { Cil.Ne }
  | AND { Cil.LAnd } 
  | OR { Cil.LOr }