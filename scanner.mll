(* Ocamllex scanner for Goblin *)

{ open Parser 
  let unescape s =
    Scanf.sscanf ("\"" ^ s ^ "\"") "%S%!" (fun x -> x)
}

let escape = '\\' ['\\' ''' '"' 'n' 'r' 't']
let escape_char = ''' (escape) '''
let ascii = ([' '-'!' '#'-'[' ']'-'~'])
let digit = ['0'-'9']
let string = '"' ( (ascii | escape)* as s) '"'
let char = ''' ( ascii | digit ) '''

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "/*"     { comment lexbuf }           (* Comments *)
| '.'      { DOT }
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| '['      { LBRACK }
| ']'      { RBRACK }
| ';'      { SEMI }
| ','      { COMMA }
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { TIMES }
| '/'      { DIVIDE }
| '%'      { MOD }
| '='      { ASSIGN }
| "+="     { PLUSAS }
| "-="     { MINUSAS }
| "*="     { TIMESAS }
| "/="     { DIVIDEAS }
| "%="     { MODAS }
| "=="     { EQ }
| "!="     { NEQ }
| '<'      { LT }
| "<="     { LEQ }
| ">"      { GT }
| ">="     { GEQ }
| "and"    { AND }
| "or"     { OR }
| "not"    { NOT }
| "if"     { IF }
| "else"   { ELSE }
| "for"    { FOR }
| "while"  { WHILE }
| "return" { RETURN }
| "exit"   { EXIT }
| "num"    { NUM }
| "bool"   { BOOL }
| "char"   { CHAR }
| "entity" { ENTITY }
| "true"   { TRUE }
| "false"  { FALSE }
| "does"   { DOES }
| "build"  { BUILD }
| "this"   { THIS }
| "is"     { IS }
| "empty"  { EMPTY }
| "entities" { ENTITIES }
| "functions" { FUNCTIONS }
| "world" { WORLD }
| (_ as c)':' { SYM(c) }
| ['0'-'9']+ as lxm { NUMLIT(int_of_string lxm) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| char as lxm     { CHARLIT( String.get lxm 1 ) }
| escape_char as lxm{ CHARLIT( String.get (unescape lxm) 1) }
| string          { STRLIT(unescape s) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }
