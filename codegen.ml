(* Code generation: translate takes a semantically checked AST and
produces LLVM IR *)

module L = Llvm
module A = Ast

module StringMap = Map.Make(String)

type struct_info = {tcode: int; ltype: L.lltype; fields: int StringMap.t}

let translate (globals, functions, structs, symbols) =
  let context = L.global_context () in
  let the_module = L.create_module context "Goblin"
  and i32_t  = L.i32_type  context
  and i8_t   = L.i8_type   context
  and i1_t   = L.i1_type   context
  and ptr_t  = L.pointer_type
  and arr_t  = L.array_type
  and tile_t = L.named_struct_type context "_tile"
  and typedefs = 
    let structs_l = (fun (_, l) -> l) (List.fold_left (fun (c, l) f -> (c - 1, (c, f) :: l)) (List.length structs, []) structs) in
    let add_struct map (tcode, (name, fields)) =
      let ltype = L.named_struct_type context name in
      let fields_l = (fun (_, l) -> l) (List.fold_left (fun (c, l) f -> (c - 1, (c, f) :: l)) (List.length fields - 1, []) fields) in
      let fields_m = List.fold_left (fun m (i, (_, n)) -> StringMap.add n i m) StringMap.empty fields_l in
      let struct_info = {tcode = tcode; ltype = ltype; fields = fields_m} in
      StringMap.add name struct_info map
    in
    List.fold_left add_struct StringMap.empty structs_l
  in
  let func_t = L.function_type i1_t [|ptr_t tile_t|] in

  let field_info = 
    let add_field t m (_, n) = StringMap.add n (t, StringMap.find n (StringMap.find t typedefs).fields) m in
    let add_struct m (n, l) = List.fold_left (fun m e -> add_field n m e) m l in
    List.fold_left add_struct StringMap.empty structs
  in

  let ltype_of_typ = function
      A.Num -> i32_t
    | A.Entity -> ptr_t tile_t
    | A.Bool -> i1_t
    | A.Char -> i8_t
    | A.String -> ptr_t i8_t
    | A.Board (row, col) -> arr_t (arr_t tile_t col) row
  in

  (* Define tile *)
  ignore(L.struct_set_body tile_t [|i32_t; i32_t; i32_t; ptr_t i8_t|] false);
  (* Define struct *)
  let build_struct_body (name, fields) =
    let types = Array.of_list (List.rev (List.map (fun (t, _) -> ltype_of_typ t) fields)) in
    ignore(L.struct_set_body (StringMap.find name typedefs).ltype types false)
  in
  List.iter build_struct_body structs;
  
  
  (* Declare each global variable; remember its value in a map *)
  let global_vars =
    let global_var m (t, n) = 
      let init = (match t with
          A.Board (r, c) -> let z = L.const_int tile_t 0 in
                            let rz = L.const_array tile_t (Array.make c z) in
                            L.const_array (arr_t tile_t c) (Array.make r rz)
        | _ -> L.const_int (ltype_of_typ t) 0)
      in
      StringMap.add n (L.define_global n init the_module) m
    in
    List.fold_left global_var StringMap.empty globals
  in

  (* Add "struct type code to symbol representation" array to global vars *)
  let global_vars = 
    let n = "_symbols" in
    let sym_arr = Array.make (List.length symbols + 1) (L.const_int i8_t 32) in
    let build_sym (n, c) = sym_arr.((StringMap.find n typedefs).tcode) <- (L.const_int i8_t (int_of_char c)) in
    List.iter build_sym symbols;
    let init = L.const_array i8_t sym_arr in
    StringMap.add n (L.define_global n init the_module) global_vars
  in

  (* Declare printf(), which the print built-in function will call *)
  let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func = L.declare_function "printf" printf_t the_module in

  let getKey_t = L.function_type i8_t [| |] in
  let getKey_func = L.declare_function "getKey" getKey_t the_module in

  let clearScreen_t = L.function_type i32_t [| |] in
  let clearScreen_func = L.declare_function "_clearScreen" clearScreen_t the_module in
  
  (* Define each function (arguments and return type) so we can call it *)
  let function_decls =
    let function_decl m fdecl =
      let name = fdecl.A.fname
      and formal_types =
	Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.A.formals)
      in let ftype = L.function_type (ltype_of_typ fdecl.A.ftyp) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty functions in

  (* Add "struct type code to build function pointer" array to global vars *)
  let global_vars = 
    let n = "_build_tbl" in
    let nothing_func = fst (StringMap.find "_nothing" function_decls) in
    let fn_arr = Array.make (List.length symbols + 1) nothing_func in
    let build_fn (n, _) =
      let tcode = (StringMap.find n typedefs).tcode in
      let fname = "_build_" ^ n in
      fn_arr.(tcode) <- fst (StringMap.find fname function_decls)
    in
    List.iter build_fn structs;
    let init = L.const_array (ptr_t func_t) fn_arr in
    StringMap.add n (L.define_global n init the_module) global_vars
  in 
  
  (* Add "struct type code to does function pointer" array to global vars *)
  let global_vars = 
    let n = "_does_tbl" in
    let nothing_func = fst (StringMap.find "_nothing" function_decls) in
    let fn_arr = Array.make (List.length symbols + 1) nothing_func in
    let does_fn (n, _) =
      let tcode = (StringMap.find n typedefs).tcode in
      let fname = "_does_" ^ n in
      fn_arr.(tcode) <- fst (StringMap.find fname function_decls)
    in
    List.iter does_fn structs;
    let init = L.const_array (ptr_t func_t) fn_arr in
    StringMap.add n (L.define_global n init the_module) global_vars
  in 

  (* Fill in the body of the given function *)
  let build_function_body fdecl =
    let (the_function, _) = StringMap.find fdecl.A.fname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in
    let char_format_str = L.build_global_stringptr "%c" "fmt" builder in
    
    (* Construct the function's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map *)
    let local_vars =
      let add_formal m (t, n) p = L.set_value_name n p;
	      let local = L.build_alloca (ltype_of_typ t) n builder in
	      ignore (L.build_store p local builder);
	      StringMap.add n local m in

      let add_local m (t, n) =
	      let local_var = L.build_alloca (ltype_of_typ t) n builder
	      in StringMap.add n local_var m in

      let formals = List.fold_left2 add_formal StringMap.empty fdecl.A.formals
          (Array.to_list (L.params the_function)) in
      List.fold_left add_local formals fdecl.A.locals in

    (* Return the value for a variable or formal argument *)
    let lookup n = try StringMap.find n local_vars
                   with Not_found -> StringMap.find n global_vars
    in

    (* Helper function for accessing a tile of the board *)
    let get_tile_ptr (r, c, b) = 
      let s = "_board" in
      L.build_gep (lookup s) [|L.const_int tile_t 0; r; c|] s b
    in

    (* Helper function for accessing a field of a tile that's in the board*)
    let get_tile_field_ptr (e, f, b) =
      let idx = (match f with
          "row" -> 0
        | "col" -> 1
        | "type" -> 2
        | _ -> 3)
      in
      L.build_struct_gep e idx "_tile_fld_ptr" b
    in

    (* Helper function for accessing a field of a struct that's in a tile *)
    let get_entity_field_ptr (e, f, b) =
      let tile_field_ptr = get_tile_field_ptr (e, "ptr", b) in
      let void_ptr = L.build_load tile_field_ptr "_void_ptr" b in
      if f = "_called" then
        L.build_bitcast void_ptr (ptr_t i1_t) "_ent_ptr" b
      else
        let (t, idx) = StringMap.find f field_info in
        let lt = (StringMap.find t typedefs).ltype in
        let ent_ptr = L.build_bitcast void_ptr (ptr_t lt) "_ent_ptr" b in
        L.build_struct_gep ent_ptr idx "_ent_fld_ptr" b
    in

    (* Construct code for an expression; return its value *)
    let rec expr builder = function
	A.NumLit i -> L.const_int i32_t i
      | A.StrLit s -> L.const_string context s
      | A.CharLit c -> L.const_int i8_t (int_of_char c)
      | A.BoolLit b -> L.const_int i1_t (if b then 1 else 0)
      | A.Id s -> L.build_load (lookup s) s builder
      | A.Field (e, s) -> 
          let e' = expr builder e in
          let ptr = get_entity_field_ptr (e', s, builder) in
          L.build_load ptr s builder
      | A.Binop (e, A.Is, A.Id t) ->
          let e' = expr builder e in
          let e_type_ptr = get_tile_field_ptr (e', "type", builder) in
          let e_type = L.build_load e_type_ptr "_e_type_ptr" builder in
          let tcode = if t = "empty" then 0 else
            (StringMap.find t typedefs).tcode in
          L.build_icmp L.Icmp.Eq e_type (L.const_int i32_t tcode) "_tmp" builder
      | A.Binop (e1, op, e2) ->
	  let e1' = expr builder e1
	  and e2' = expr builder e2 in
	  (match op with
	    A.Add     -> L.build_add
	  | A.Sub     -> L.build_sub
	  | A.Mult    -> L.build_mul
          | A.Div     -> L.build_sdiv
          | A.Mod     -> L.build_srem
	  | A.And     -> L.build_and
	  | A.Or      -> L.build_or
	  | A.Equal | A.Is -> L.build_icmp L.Icmp.Eq
	  | A.Neq     -> L.build_icmp L.Icmp.Ne
	  | A.Less    -> L.build_icmp L.Icmp.Slt
	  | A.Leq     -> L.build_icmp L.Icmp.Sle
	  | A.Greater -> L.build_icmp L.Icmp.Sgt
	  | A.Geq     -> L.build_icmp L.Icmp.Sge
	  ) e1' e2' "tmp" builder
      | A.Unop(op, e) ->
	  let e' = expr builder e in
	  (match op with
	    A.Neg     -> L.build_neg
          | A.Not     -> L.build_not) e' "tmp" builder
      | A.Assign (s, e) -> let e' = expr builder e in
                          ignore (L.build_store e' (lookup s) builder); e'
      | A.FAssign (e, s, v) ->
          let e' = expr builder e in
          let v' = expr builder v in
          let ptr = get_entity_field_ptr (e', s, builder) in
          ignore (L.build_store v' ptr builder); v'
      | A.Call ("print", [e]) | A.Call ("printb", [e]) ->
	  L.build_call printf_func [| int_format_str ; (expr builder e) |] "printf" builder
      | A.Call ("_print_symbol", [e]) ->
          let e' = expr builder e in
          let typ_ptr = get_tile_field_ptr (e', "type", builder) in
          let tcode = L.build_load typ_ptr "_tcode" builder in
          let arr_ptr = lookup "_symbols" in
          let sym_ptr = L.build_gep arr_ptr [|L.const_int i8_t 0; tcode|] "_sym_ptr" builder in
          let sym = L.build_load sym_ptr "_sym" builder in
	  L.build_call printf_func [| char_format_str ; sym |] "printf" builder
      | A.Call ("prints", [e]) ->
          let get_string = function A.StrLit s -> s | _ -> "" in
          let s_ptr = L.build_global_stringptr ((get_string e) ^ "\n") ".str" builder in
          L.build_call printf_func [| s_ptr |] "printf" builder
      | A.Call ("getKey", []) -> 
          L.build_call getKey_func [| |] "getKey" builder
      | A.Call ("_clearScreen", []) -> 
          L.build_call clearScreen_func [| |] "_clearScreen" builder
      | A.Call ("row", [e]) ->
          let e' = expr builder e in
          L.build_load (get_tile_field_ptr (e', "row", builder)) "_row" builder
      | A.Call ("col", [e]) ->
          let e' = expr builder e in
          L.build_load (get_tile_field_ptr (e', "col", builder)) "_col" builder
      | A.Call ("move", [e; r; c]) ->
          let tile_ptr = expr builder e in
          let r' = expr builder r in
          let c' = expr builder c in
          let new_tile_ptr = get_tile_ptr (r', c', builder) in
          let type_ptr = get_tile_field_ptr (tile_ptr, "type", builder) in
          let ptr_ptr = get_tile_field_ptr (tile_ptr, "ptr", builder) in
          let typ = L.build_load type_ptr "_typ" builder in
          let ptr = L.build_load ptr_ptr "_ptr" builder in
          let new_type_ptr = get_tile_field_ptr (new_tile_ptr, "type", builder) in
          let new_ptr_ptr = get_tile_field_ptr (new_tile_ptr, "ptr", builder) in
          ignore(L.build_store typ new_type_ptr builder);
          ignore(L.build_store ptr new_ptr_ptr builder);
          ignore(L.build_store (L.const_int i32_t 0) type_ptr builder);
          ignore(L.build_store (L.const_pointer_null (ptr_t i8_t)) ptr_ptr builder);
          L.const_int i1_t 1 
      | A.Call ("peek", [r; c]) ->
          let r' = expr builder r in
          let c' = expr builder c in
          get_tile_ptr (r', c', builder)
      | A.Call ("remove", [e]) ->
          let e' = expr builder e in
          let type_ptr = get_tile_field_ptr (e', "type", builder) in
          let ptr_ptr = get_tile_field_ptr (e', "ptr", builder) in
          ignore(L.build_store (L.const_int i32_t 0) type_ptr builder);
          ignore(L.build_store (L.const_pointer_null (ptr_t i8_t)) ptr_ptr builder);
          let ptr = L.build_load ptr_ptr "_ptr" builder in
          L.build_free ptr builder
      | A.Call ("place", [A.Id (t); r; c]) ->
          let r' = expr builder r in
          let c' = expr builder c in
          let tile_ptr = get_tile_ptr (r', c', builder) in
          let tinfo = StringMap.find t typedefs in
          let tcode = L.const_int i32_t tinfo.tcode in
          ignore (L.build_store tcode (get_tile_field_ptr (tile_ptr, "type", builder)) builder);
          let s_ptr = L.build_malloc tinfo.ltype "_ent_ptr" builder in
          let void_s_ptr = L.build_bitcast s_ptr (ptr_t i8_t) "_v_ent_ptr" builder in
          ignore (L.build_store void_s_ptr (get_tile_field_ptr (tile_ptr, "ptr", builder)) builder);
          let fn_ptr = fst (StringMap.find ("_build_" ^ t) function_decls) in
	  L.build_call fn_ptr [| tile_ptr |] "_build" builder
      | A.Call ("_init_tile", [r; c]) ->
          let r' = expr builder r in
          let c' = expr builder c in
          let tile_ptr = get_tile_ptr (r', c', builder) in
          ignore (L.build_store r' (get_tile_field_ptr (tile_ptr, "row", builder)) builder);
          ignore (L.build_store c' (get_tile_field_ptr (tile_ptr, "col", builder)) builder);
          L.const_int i1_t 1 
      | A.Call (f, act) ->
          let (fdef, _) = StringMap.find f function_decls in
	  let actuals = List.rev (List.map (expr builder) (List.rev act)) in
	  let result = f ^ "_result" in
          L.build_call fdef (Array.of_list actuals) result builder
      | A.Does e ->
          let e' = expr builder e in
          let typ_ptr = get_tile_field_ptr (e', "type", builder) in
          let tcode = L.build_load typ_ptr "_tcode" builder in
          let arr_ptr = lookup "_does_tbl" in
          let fn_ptr_ptr = L.build_gep arr_ptr [|L.const_int i8_t 0; tcode|] "_fn_ptr_ptr" builder in
          let fn_ptr = L.build_load fn_ptr_ptr "_fn_ptr" builder in
	  L.build_call fn_ptr [| e' |] "_does" builder
    in

    (* Invoke "f builder" if the current block doesn't already
       have a terminal (e.g., a branch). *)
    let add_terminal builder f =
      match L.block_terminator (L.insertion_block builder) with
	Some _ -> ()
      | None -> ignore (f builder) in
	
    (* Build the code for the given statement; return the builder for
       the statement's successor *)
    let rec stmt builder = function
	A.Block sl -> List.fold_left stmt builder sl
      | A.Expr e -> ignore (expr builder e); builder
      | A.Return e -> ignore (L.build_ret (expr builder e) builder); builder
      | A.If (predicate, then_stmt, else_stmt) ->
         let bool_val = expr builder predicate in
	 let merge_bb = L.append_block context "merge" the_function in

	 let then_bb = L.append_block context "then" the_function in
	 add_terminal (stmt (L.builder_at_end context then_bb) then_stmt)
	   (L.build_br merge_bb);

	 let else_bb = L.append_block context "else" the_function in
	 add_terminal (stmt (L.builder_at_end context else_bb) else_stmt)
	   (L.build_br merge_bb);

	 ignore (L.build_cond_br bool_val then_bb else_bb builder);
	 L.builder_at_end context merge_bb

      | A.While (predicate, body) ->
	  let pred_bb = L.append_block context "while" the_function in
	  ignore (L.build_br pred_bb builder);

	  let body_bb = L.append_block context "while_body" the_function in
	  add_terminal (stmt (L.builder_at_end context body_bb) body)
	    (L.build_br pred_bb);

	  let pred_builder = L.builder_at_end context pred_bb in
	  let bool_val = expr pred_builder predicate in

	  let merge_bb = L.append_block context "merge" the_function in
	  ignore (L.build_cond_br bool_val body_bb merge_bb pred_builder);
	  L.builder_at_end context merge_bb

      | A.For (e1, e2, e3, body) -> stmt builder
	    ( A.Block [A.Expr e1 ; A.While (e2, A.Block [body ; A.Expr e3]) ] )
    in

    (* Build the code for each statement in the function *)
    let builder = stmt builder (A.Block fdecl.A.body) in

    (* Add a return if the last block falls off the end *)
    add_terminal builder (L.build_ret (L.const_int (ltype_of_typ fdecl.A.ftyp) 0))
  in

  List.iter build_function_body functions;
  the_module
