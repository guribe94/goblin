/* Ocamlyacc parser for Goblin */

%{
open Ast
%}

%token SEMI LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK COMMA
%token PLUS MINUS TIMES DIVIDE MOD ASSIGN NOT
%token PLUSAS MINUSAS TIMESAS DIVIDEAS MODAS
%token EQ NEQ LT LEQ GT GEQ TRUE FALSE AND OR IS
%token RETURN EXIT IF ELSE FOR WHILE
%token NUM BOOL ENTITY EMPTY CHAR
%token DOES BUILD THIS
%token ENTITIES FUNCTIONS WORLD
%token DOT
%token <char> SYM CHARLIT
%token <int> NUMLIT
%token <string> STRLIT
%token <string> ID
%token EOF

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN PLUSAS MINUSAS TIMESAS DIVIDEAS MODAS
%left OR
%left AND
%left EQ NEQ IS 
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE MOD
%right NOT NEG
%left DOT

%start program
%type <Ast.program> program

%%

program:
    world entities functions EOF   { ($1, $2, $3) }

entities:
    ENTITIES LBRACE edecl_list RBRACE { $3 }

edecl_list:
    /* nothing */    { [] }
  | edecl_list edecl { $2 :: $1 }

edecl:
  SYM  ID LBRACE vdecl_list build_opt does_opt RBRACE  { { symbol = $1;
                                                           ename = $2;
                                                           evars = $4;
                                                           ebuild = $5;
                                                           edoes = $6 } }

build:
    BUILD LBRACE vdecl_list stmt_list RBRACE { { varlist = $3;
                                                 block = List.rev $4; } }

build_opt:
    /* nothing */   { None }
  | build           { Some($1) }

does:
    DOES LBRACE vdecl_list stmt_list RBRACE { { varlist = $3;
                                                block = List.rev $4; } }

does_opt:
  /* nothing */     { None }
  | does            { Some($1) }

functions:
    FUNCTIONS LBRACE fdecl_list RBRACE { $3 }

fdecl_list:
    /* nothing */    { [] }
  | fdecl_list fdecl { $2 :: $1 }

fdecl:
    ftyp ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
        { { ftyp = $1;
	    fname = $2;
	    formals = $4;
	    locals = List.rev $7;
	    body = List.rev $8 } }

formals_opt:
    /* nothing */ { [] }
  | formal_list   { List.rev $1 }

formal_list:
    ftyp ID                   { [($1,$2)] }
  | formal_list COMMA ftyp ID { ($3,$4) :: $1 }

ftyp:
    NUM         { Num }
  | BOOL        { Bool }
  | CHAR        { Char }
  | ENTITY      { Entity }

world:
    WORLD LBRACK NUMLIT COMMA NUMLIT RBRACK LBRACE vdecl_list stmt_list RBRACE
        { { rows = $3;
            cols = $5;
            wvars = $8;
            wstmts = List.rev $9 } }

vdecl_list:
    /* nothing */    { [] }
  | vdecl_list vdecl { $2 :: $1 }

vdecl:
    NUM ID SEMI                     { (Num, $2) }
  | BOOL ID SEMI                    { (Bool, $2) }
  | CHAR ID SEMI                    { (Char, $2) }
  | ENTITY ID SEMI                  { (Entity, $2) }

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
    expr SEMI { Expr $1 }
  | RETURN expr SEMI { Return $2 }
  | LBRACE stmt_list RBRACE { Block(List.rev $2) }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
  | FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN stmt
        { For($3, $5, $7, $9) }
  | WHILE LPAREN expr RPAREN stmt { While($3, $5) }
  | EXIT SEMI { Expr (Assign("_exit", BoolLit(true))) }

expr_opt:
    /* nothing */ { BoolLit(false) }
  | expr          { $1 }

expr:
    NUMLIT           { NumLit($1) }
  | CHARLIT          { CharLit($1) }
  | STRLIT           { StrLit($1) }
  | TRUE             { BoolLit(true) }
  | FALSE            { BoolLit(false) }
  | ID               { Id($1) }
  | THIS             { Id("this") }
  | expr DOT ID      { Field($1, $3) }
  | expr IS     ID   { Binop($1, Is, Id($3))}
  | expr IS     EMPTY{ Binop($1, Is, Id("empty"))}
  | expr PLUS   expr { Binop($1, Add,   $3) }
  | expr MINUS  expr { Binop($1, Sub,   $3) }
  | expr TIMES  expr { Binop($1, Mult,  $3) }
  | expr DIVIDE expr { Binop($1, Div,   $3) }
  | expr MOD    expr { Binop($1, Mod,   $3) }
  | expr EQ     expr { Binop($1, Equal, $3) }
  | expr NEQ    expr { Binop($1, Neq,   $3) }
  | expr LT     expr { Binop($1, Less,  $3) }
  | expr LEQ    expr { Binop($1, Leq,   $3) }
  | expr GT     expr { Binop($1, Greater, $3) }
  | expr GEQ    expr { Binop($1, Geq,   $3) }
  | expr AND    expr { Binop($1, And,   $3) }
  | expr OR     expr { Binop($1, Or,    $3) }
  | MINUS expr %prec NEG { Unop(Neg, $2) }
  | NOT expr         { Unop(Not, $2) }
  | ID ASSIGN expr   { Assign($1, $3) }
  | ID PLUSAS expr   { Assign($1, Binop(Id($1), Add, $3)) }
  | ID MINUSAS expr  { Assign($1, Binop(Id($1), Sub, $3)) }
  | ID TIMESAS expr  { Assign($1, Binop(Id($1), Mult, $3)) }
  | ID DIVIDEAS expr { Assign($1, Binop(Id($1), Div, $3)) }
  | ID MODAS expr    { Assign($1, Binop(Id($1), Mod, $3)) }
  | expr DOT ID ASSIGN expr   { FAssign($1, $3, $5) }
  | expr DOT ID PLUSAS expr   { FAssign($1, $3, Binop(Field($1, $3), Add, $5)) }
  | expr DOT ID MINUSAS expr  { FAssign($1, $3, Binop(Field($1, $3), Sub, $5)) }
  | expr DOT ID TIMESAS expr  { FAssign($1, $3, Binop(Field($1, $3), Mult, $5)) }
  | expr DOT ID DIVIDEAS expr { FAssign($1, $3, Binop(Field($1, $3), Div, $5)) }
  | expr DOT ID MODAS expr    { FAssign($1, $3, Binop(Field($1, $3), Mod, $5)) }
  | call             { $1 }
  | LPAREN expr RPAREN { $2 }

call:
    ID LPAREN actuals_opt RPAREN { Call($1, $3) }

actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }

