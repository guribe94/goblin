(* Abstract Syntax Tree and functions for printing it *)

type op = Add | Sub | Mult | Div | Mod | Equal | Neq | Less | Leq | Greater | Geq |
          And | Or | Is

type uop = Neg | Not

type typ = Num | String | Bool | Char | Entity | Board of int * int

type expr =
    NumLit of int
  | StrLit of string
  | CharLit of char 
  | BoolLit of bool
  | Id of string
  | Field of expr * string
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Assign of string * expr
  | FAssign of expr * string * expr
  | Call of string * expr list
  | Does of expr

type vdecl = typ * string

type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt

type body = {
  varlist : vdecl list;
  block : stmt list;
}

type edecl = {
  symbol : char;
  ename : string;
  evars : vdecl list;
  ebuild : body option;
  edoes : body option;
}

type fdecl = {
  ftyp : typ;
  fname : string;
  formals : vdecl list;
  locals : vdecl list;
  body : stmt list;
}

type world = {
  rows : int;
  cols : int;
  wvars : vdecl list;
  wstmts : stmt list;
}

type program = world * edecl list * fdecl list

(* Pretty-printing functions *)

let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | And -> "and"
  | Or -> "or"
  | Is -> "is"

let string_of_uop = function
    Neg -> "-"
  | Not -> "not "

let rec string_of_expr = function
    NumLit(l) -> string_of_int l
  | StrLit(l) -> l
  | CharLit(l) -> String.make 1 l
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | Id(s) -> s
  | Field(l, s) ->  string_of_expr l ^ "." ^ s
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | FAssign(e, f, v) -> string_of_expr e ^ "." ^ f ^ " = " ^ string_of_expr v
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Does(e) -> "DOES(" ^ string_of_expr e ^ ")"

let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s

let string_of_typ = function
    Num -> "num"
  | Bool -> "bool"
  | Char -> "char"
  | String -> "string"
  | Entity -> "entity"
  | Board (_,_) -> ""

let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"

let string_of_edecl edecl =
  String.make 1 edecl.symbol ^ ":" ^
  edecl.ename ^ " {\n" ^
  String.concat "" (List.map string_of_vdecl edecl.evars) ^ "\nbuild {" ^ 
  String.concat "" (List.map string_of_vdecl (match edecl.ebuild with
  Some(a) -> a.varlist | None -> [])) ^
  String.concat "" (List.map string_of_stmt (match edecl.ebuild with
  Some(a) -> a.block | None -> [])) ^ "}\nbuild {\n" ^
  String.concat "" (List.map string_of_vdecl (match edecl.edoes with
  Some(a) -> a.varlist | None -> [])) ^
  String.concat "" (List.map string_of_stmt (match edecl.edoes with
  Some(a) -> a.block | None -> [])) ^ "}\ndoes\n"

let string_of_fdecl fdecl =
  string_of_typ fdecl.ftyp ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_world world =
  String.concat ("world [" ^ string_of_int world.rows ^ ", " ^ string_of_int world.cols ^ "] {\n")
  (List.map string_of_vdecl world.wvars) ^ "\n" ^ 
  String.concat "" (List.map string_of_stmt world.wstmts) ^ "\n}\n" 

let string_of_program (w, e, f) =
  String.concat ((string_of_world w) ^ "entities {\n") (List.map string_of_edecl e) ^ "}\n" ^
  String.concat "functions {\n" (List.map string_of_fdecl f) ^ "}\n"
