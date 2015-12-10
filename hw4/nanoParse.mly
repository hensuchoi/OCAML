%{
open Nano 
%}

%token <int> Num
%token TRUE FALSE
%token <string> Id
%token LET
%token REC
%token EQ
%token IN
%token FUN ARROW
%token IF THEN ELSE
%token EOF
%token PLUS MINUS
%token MUL DIV
%token LT LE NE
%token OR
%token AND
%token LPAREN RPAREN
%token LBRAC SEMI RBRAC
%token COLONCOLON


%left OR
%left AND 
%left NE LE EQ LT
%left PLUS MINUS 
%left MUL DIV
%left APP

%start exp 
%type <Nano.expr> exp

%%    
exp:| LET REC Id EQ exp IN exp  { Letrec($3,$5,$7) }
    | LET Id EQ exp IN exp      { Let($2,$4,$6) }  
    | FUN Id ARROW exp          { Fun($2,$4) }
    | IF exp THEN exp ELSE exp  { If($2,$4,$6) }
    | expr                      {$1}

expr: expr OR nexpr              { Bin($1,Or,$3) }   
    | nexpr                      { $1 }

nexpr: nexpr AND eqexpr             { Bin($1,And,$3) }
    | eqexpr                      { $1 }

eqexpr: eqexpr EQ colexpr              { Bin($1,Eq,$3) }
    | eqexpr LT colexpr              { Bin($1,Lt,$3) }
    | eqexpr LE colexpr             { Bin($1,Le,$3) }
    | eqexpr NE colexpr             { Bin($1,Ne,$3) }
    | colexpr                      { $1 }

colexpr: colexpr COLONCOLON brexpr  { Bin($1,Cons,$3) }
    | COLONCOLON colexpr SEMI brexpr { Bin($2,Cons,$4) }
    | colexpr SEMI brexpr { Bin($1,Cons,$3) }
    | colexpr RBRAC { Bin($1,Cons,NilExpr) }
    | LBRAC colexpr {$2}
    | LBRAC RBRAC { NilExpr }
    | brexpr                    { $1 }

brexpr: LBRAC RBRAC { NilExpr }
    | plexp              { $1 }

plexp: plexp PLUS texp            { Bin($1,Plus,$3) }
    | plexp MINUS texp           { Bin($1,Minus,$3) }
    |texp                         {$1}

texp:
    | texp MUL dexp             { Bin($1,Mul,$3) }
    | texp DIV dexp             { Bin($1,Div,$3) }
    | dexp                      { $1 }

dexp: dexp atom{ App($1,$2) }
   | atom      { $1 }

atom:    
    | Num                       { Const $1 }
    | TRUE                       { True }
    | FALSE                      { False }
    | Id                         { Var($1) }
    | LPAREN exp RPAREN        { $2 }
