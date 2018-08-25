(* Semantic checking for Goblin *)

open Ast

module StringMap = Map.Make(String)


 (****************** Translate to Structs **********************)
let translate (world, entities, functions) = 

  let init_world = {
    ftyp = Bool;
    fname = "_init_world";
    formals = [];
    locals = world.wvars;
    body = world.wstmts
  } in
  let edecl_build l e = (match e.ebuild with
      Some(b) -> { ftyp = Bool;
                   fname = "_build_" ^ e.ename;
                   formals = [(Entity, "this")];
                   locals = b.varlist;
                   body = b.block;
      } :: l
    | None -> { ftyp = Bool;
                fname = "_build_" ^ e.ename;
                formals = [(Entity, "this")];
                locals = [];
                body = [];
      } :: l)
  in
  let edecl_does l e = (match e.edoes with
      Some(d) -> { ftyp = Bool;
                   fname = "_does_" ^ e.ename;
                   formals = [(Entity, "this")];
                   locals = d.varlist;
                   body = [
                     If(
                       Unop(Not, Field(Id("this"), "_called")),
                       Block(Expr(FAssign(Id("this"), "_called", BoolLit(true))) :: d.block),
                       Expr(BoolLit(false))
                     )
                   ];
      } :: l
    | None -> { ftyp = Bool;
                fname = "_does_" ^ e.ename;
                formals = [(Entity, "this")];
                locals = [];
                body = [];
      } :: l)
  in
  let functions = List.fold_left edecl_build functions entities in
  let functions = List.fold_left edecl_does functions entities in
  let functions = init_world :: functions in
  let nothing = {
    ftyp = Bool;
    fname = "_nothing";
    formals = [(Entity, "this")];
    locals = [];
    body = []
  } in
  let main = {
    ftyp = Num;
    fname = "main";
    formals = [];
    locals = [(Num, "i"); (Num, "j"); (Entity, "e")];
    body = [
      Expr(Assign("rows", NumLit(world.rows)));
      Expr(Assign("cols", NumLit(world.cols)));
      For(Assign("i", NumLit(0)),
          Binop(Id("i"), Less, Id("rows")),
          Assign("i", Binop(Id("i"), Add, NumLit(1))),

          For(Assign("j", NumLit(0)),
              Binop(Id("j"), Less, Id("cols")),
              Assign("j", Binop(Id("j"), Add, NumLit(1))),
              Expr(Call("_init_tile", [Id("i"); Id("j")]))
          )
      );
      Expr(Call("_init_world", []));
      While(Unop(Not, Id("_exit")), Block([
        Expr(Call("_clearScreen", [])); 
        For(
          Assign("i", NumLit(0)),
          Binop(Id("i"), Less, Id("rows")),
          Assign("i", Binop(Id("i"), Add, NumLit(1))),
          Block([
            For(
              Assign("j", NumLit(0)),
              Binop(Id("j"), Less, Id("cols")),
              Assign("j", Binop(Id("j"), Add, NumLit(1))),
              Block([
                Expr(Assign("e", Call("peek", [Id("i"); Id("j")])));
                If(
                  Unop(Not, Binop(Id("e"), Is, Id("empty"))),
                  Expr(FAssign(Id("e"), "_called", BoolLit(false))),
                  Expr(BoolLit(false))
                );
                Expr(Call("_print_symbol", [Id("e")]));
              ])
            );
            Expr(Call("prints", [StrLit("")]))
          ])
        );
        For(
          Assign("i", NumLit(0)),
          Binop(Id("i"), Less, Id("rows")),
          Assign("i", Binop(Id("i"), Add, NumLit(1))),
          Block([
            For(
              Assign("j", NumLit(0)),
              Binop(Id("j"), Less, Id("cols")),
              Assign("j", Binop(Id("j"), Add, NumLit(1))),
              Block([
                Expr(Assign("e", Call("peek", [Id("i"); Id("j")])));
                Expr(Does(Id("e")));
              ])
            );
          ])
        )
      ]));
      Return(NumLit(0))
    ]
  } in
  let functions = nothing :: functions in
  let functions = main :: functions in
  let edecl_struct l e = (e.ename, List.rev ((Bool, "_called") :: (List.rev e.evars))) :: l in
  let structs = List.fold_left edecl_struct [] entities in
  let edecl_symbol l e = (e.ename, e.symbol) :: l in
  let symbols = List.fold_left edecl_symbol [] entities in
  ([
    (Board(world.rows, world.cols), "_board");
    (Num, "rows");
    (Num, "cols");
    (Bool, "_exit")
  ], functions, structs, symbols)


(* Semantic checking of a program. Returns void if successful,
   throws an exception if something is wrong.

   Check each global variable, function, entity and world. *)


let check (globals, functions, structs, _) =
  
  (************************ Exceptions ****************************************)

  (* Raise an exception if the given list has a duplicate *)
  let report_duplicate exceptf list =
    let rec helper = function
  n1 :: n2 :: _ when n1 = n2 -> raise (Failure (exceptf n1))
      | _ :: t -> helper t
      | [] -> ()
    in helper (List.sort compare list)
  in


  (* Raise an exception of the given rvalue type cannot be assigned to the given lvalue type *)
  let check_assign lvaluet rvaluet err =
    if lvaluet == rvaluet then lvaluet else raise err
  in

  (****************** Checking Global Variables ******************************)

  report_duplicate (fun n -> "duplicate global " ^ n) (List.map snd globals);

   (************************ Structs *****************************************)
  let s_vars = List.map snd structs in
  let s_vars = List.flatten s_vars in
  let s_vars = List.fold_left (fun l (t, n) -> if n = "_called" then l else (t, n) :: l) [] s_vars in
  (* Checks for duplicates *)
  report_duplicate (fun s -> "Duplicate attribute. " ^ s) (List.map snd s_vars);

  let s_name = List.map fst structs in
  (* Checks for duplicate struct names. *)
  report_duplicate (fun s -> "Duplicate struct name." ^ s) s_name;

  let s_map = List.fold_left (fun m n -> StringMap.add n 0 m) StringMap.empty s_name in
  let sf_map = List.fold_left (fun m (t, n) -> StringMap.add n t m) StringMap.empty s_vars in
  let sf_map = StringMap.add "_called" Bool sf_map in

  (************************ Checking Functions *******************************)

  if List.mem "print" (List.map (fun fd -> fd.fname) functions)
  then raise (Failure ("function print may not be defined")) else ();

  if List.mem "prints" (List.map (fun fd -> fd.fname) functions)
  then raise (Failure ("function prints may not be defined")) else ();

  if List.mem "getKey" (List.map (fun fd -> fd.fname) functions)
  then raise (Failure ("function getKey may not be defined")) else ();

  if List.mem "row" (List.map (fun fd -> fd.fname) functions)
  then raise (Failure ("function row may not be defined")) else ();

  if List.mem "col" (List.map (fun fd -> fd.fname) functions)
  then raise (Failure ("function col may not be defined")) else ();

  if List.mem "move" (List.map (fun fd -> fd.fname) functions)
  then raise (Failure ("function move may not be defined")) else ();

  if List.mem "peek" (List.map (fun fd -> fd.fname) functions)
  then raise (Failure ("function peek may not be defined")) else ();

  if List.mem "remove" (List.map (fun fd -> fd.fname) functions)
  then raise (Failure ("function remove may not be defined")) else ();

  if List.mem "place" (List.map (fun fd -> fd.fname) functions)
  then raise (Failure ("function place may not be defined")) else ();

  report_duplicate (fun n -> "duplicate function " ^ n)
  (List.map (fun fd -> fd.fname) functions);

  (* Function declaration for a named function *)
  let built_in_decls = StringMap.empty in
  let built_in_decls = StringMap.add "printb"
     { ftyp = Bool; fname = "printb"; formals = [(Bool, "x")];
       locals = []; body = [] } built_in_decls in
  let built_in_decls = StringMap.add "print"
     { ftyp = Bool; fname = "print"; formals = [(Num, "x")];
       locals = []; body = [] } built_in_decls in
  let built_in_decls = StringMap.add "_print_symbol"
     { ftyp = Bool; fname = "_print_symbol"; formals = [(Entity, "x")];
       locals = []; body = [] } built_in_decls in
  let built_in_decls = StringMap.add "prints"
     { ftyp = Bool; fname = "prints"; formals = [(String, "s")];
       locals = []; body = [] } built_in_decls in
  let built_in_decls = StringMap.add "getKey"
     { ftyp = Char; fname = "getKey"; formals = [];
       locals = []; body = [] } built_in_decls in
  let built_in_decls = StringMap.add "_clearScreen"
     { ftyp = Num; fname = "_clearScreen"; formals = [];
       locals = []; body = [] } built_in_decls in
  let built_in_decls = StringMap.add "row"
     { ftyp = Num; fname = "row"; formals = [(Entity, "x")];
       locals = []; body = [] } built_in_decls in
  let built_in_decls = StringMap.add "col"
     { ftyp = Num; fname = "col"; formals = [(Entity, "x")];
       locals = []; body = [] } built_in_decls in
  let built_in_decls = StringMap.add "move"
     { ftyp = Bool; fname = "move"; formals = [(Entity, "x"); (Num, "y"); (Num, "z")];
       locals = []; body = [] } built_in_decls in
  let built_in_decls = StringMap.add "peek"
     { ftyp = Entity; fname = "peek"; formals = [(Num, "x"); (Num, "y")];
       locals = []; body = [] } built_in_decls in
  let built_in_decls = StringMap.add "remove"
     { ftyp = Bool; fname = "remove"; formals = [(Entity, "x")];
       locals = []; body = [] } built_in_decls in
  let built_in_decls = StringMap.add "place"
     { ftyp = Bool; fname = "place"; formals = [];
       locals = []; body = [] } built_in_decls in
  let built_in_decls = StringMap.add "_init_tile"
     { ftyp = Bool; fname = "_init_tile"; formals = [(Num, "x"); (Num, "y")];
       locals = []; body = [] } built_in_decls      
  in

  let function_decls = List.fold_left (fun m fd -> StringMap.add fd.fname fd m)
  built_in_decls functions
  in

  let function_decl s = try StringMap.find s function_decls
  with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in

  let check_function func =

    report_duplicate (fun n -> "duplicate formal " ^ n ^ " in " ^ func.fname)
    (List.map snd func.formals);

    report_duplicate (fun n -> "duplicate local " ^ n ^ " in " ^ func.fname)
    (List.map snd func.locals);

    (* Type of each variable (global, formal, or local *)
    let symbols = List.fold_left (fun m (t, n) -> StringMap.add n t m)
      StringMap.empty (globals @ func.formals @ func.locals )
    in

    let type_of_identifier s =
      try StringMap.find s symbols
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in

    (* Return the type of an expression or throw an exception *)
    let rec expr = function
        NumLit _ -> Num
      | BoolLit _ -> Bool
      | CharLit _ -> Char
      | StrLit _ -> String
      | Id s -> type_of_identifier s
      | Field(_, f) -> if StringMap.mem f sf_map then StringMap.find f sf_map
        else raise (Failure ("undefined field " ^ f))
      | Binop(e, Is, Id s) as ex -> let t = expr e in
        if StringMap.mem s s_map then
          (ignore (check_assign t Entity (Failure ("can only check if an entity is of a certain type, attempted to check " ^ string_of_typ t  ^ " in " ^ string_of_expr e))); Bool)
        else
          if s = "empty" then Bool else
          raise (Failure ("entity type being checked for not defined in " ^ string_of_expr ex))
      | Binop(e1, op, e2) as e -> let t1 = expr e1 and t2 = expr e2 in
        (match op with
          Add | Sub | Mult | Div | Mod when t1 = Num && t2 = Num -> Num
        | Equal | Neq when t1 = t2 -> Bool
        | Less | Leq | Greater | Geq when t1 = Num && t2 = Num -> Bool
        | And | Or when t1 = Bool && t2 = Bool -> Bool
        | _ -> raise (Failure ("illegal binary operator " ^
               string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
               string_of_typ t2 ^ " in " ^ string_of_expr e))
        )
      | Unop(op, e) as ex -> let t = expr e in
        (match op with
          Neg when t = Num -> Num
        | Not when t = Bool -> Bool
        | _ -> raise (Failure ("illegal unary operator " ^ string_of_uop op ^
               string_of_typ t ^ " in " ^ string_of_expr ex)))
      | Assign(var, e) as ex -> let lt = type_of_identifier var
                                and rt = expr e in
        check_assign lt rt (Failure ("illegal assignment " ^ string_of_typ lt ^
                                     " = " ^ string_of_typ rt ^ " in " ^ 
                                     string_of_expr ex))
      | FAssign(_, f, v) as ex -> let lt = if StringMap.mem f sf_map then StringMap.find f sf_map else raise (Failure ("undefined field " ^ f)) and rt = expr v in
        check_assign lt rt (Failure ("illegal assignment " ^ string_of_typ lt ^
                                     " = " ^ string_of_typ rt ^ " in " ^ 
                                     string_of_expr ex))
      | Does _ -> Bool
      | Call(fname, actuals) as call -> 
        (match fname with
          "prints" ->
          (match actuals with 
            [StrLit _] -> Bool
          | _ -> raise (Failure ("expecting 1 argument in " ^ string_of_expr call))
          )
        | "place" -> 
            if List.length actuals != 3 then
                raise (Failure ("expecting 3 arguments in " ^ string_of_expr call))
            else ();
            (match (List.hd actuals) with
                Id s -> if StringMap.mem s s_map then ()
                else raise (Failure ("first argument must be name of entity in " ^
                string_of_expr call))
              | _ -> raise (Failure ("first argument must be entity type in " ^
                string_of_expr call)) 
              );
                 ignore(check_assign Num (expr (List.nth actuals 1)) (Failure
                 ("illegal actual argument found " ^ string_of_typ (expr (List.nth actuals 1)) ^
                      " expected Num ")));
                 check_assign Num (expr (List.nth actuals 2)) (Failure
                 ("illegal actual argument found " ^ string_of_typ (expr (List.nth actuals 2)) ^
                      " expected Num "))
        | _ ->
          let fd = function_decl fname in
          if List.length actuals != List.length fd.formals then
            raise (Failure ("expecting " ^ string_of_int
            (List.length fd.formals) ^ " arguments in " ^ string_of_expr call))
          else
            List.iter2 (fun (ft, _) e -> let et = expr e in
            ignore (check_assign ft et
                 (Failure ("illegal actual argument found " ^ string_of_typ et ^
                 " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e))))
              fd.formals actuals;
              fd.ftyp
          )
          
    in

    let check_bool_expr e = if expr e != Bool
      then raise (Failure ("expected Boolean expression in " ^ string_of_expr e))
      else () in

    (* Verify a statement or throw an exception *)
    let rec stmt = function
      Block sl -> let rec check_block = function
        [Return _ as s] -> stmt s
          | Return _ :: _ -> raise (Failure "nothing may follow a return")
          | Block sl :: ss -> check_block (sl @ ss)
          | s :: ss -> stmt s ; check_block ss
         | [] -> ()
    in check_block sl
         | Expr e -> ignore (expr e)
         | Return e -> let t = expr e in if t = func.ftyp then () else
           raise (Failure ("return gives " ^ string_of_typ t ^ " expected " ^
                         string_of_typ func.ftyp ^ " in " ^ string_of_expr e))

         | If(p, b1, b2) -> check_bool_expr p; stmt b1; stmt b2
      | For(e1, e2, e3, st) -> ignore (expr e1); check_bool_expr e2;
                               ignore (expr e3); stmt st
      | While(p, s) -> check_bool_expr p; stmt s

    in

    stmt (Block func.body)
    in
  List.iter check_function functions
