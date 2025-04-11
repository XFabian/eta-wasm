let is_arr = function
  | [ Types.Array _ ] -> true
  | _ -> false

let is_lval = function
  | Ast.Var _
  | Subscript _ ->
      true
  | _ -> false

let rec dim_check es1 es2 =
  let helper e1 e2 =
    match (e1, e2) with
    | Ast.ArrayLit es1', Ast.ArrayLit es2' -> dim_check es1' es2'
    | _, Ast.ArrayLit _
    | Ast.ArrayLit _, _ ->
        failwith "Arrays do not have the correct dimensions to add"
    | _ -> ()
  in
  let _ = List.map2 helper es1 es2 in
  ()

let rec typecheck_expr e : Types.t list =
  match e with
  | Ast.Var x -> [ (Symbols.get x).t ]
  | Ast.Int _ -> [ Types.Int ]
  | Ast.Bool _ -> [ Types.Bool ]
  | Ast.ArrayLit es -> (
      let es_t = List.map typecheck_expr es |> List.flatten in
      match es_t with
      | [] -> [ Types.Unknown ] (* {} *)
      | x :: xs ->
          let res = List.for_all (( = ) x) xs in
          if res then [ Types.Array { elem_type = x } ]
          else failwith "Not all elements in array literal have same type")
  | Ast.Unary (Not, e) ->
      let e_ty = typecheck_expr e in
      if e_ty <> [ Types.Bool ] then
        failwith "Expression has wrong type in Unary"
      else e_ty
  | Ast.Unary (Neg, e) ->
      let e_ty = typecheck_expr e in
      if e_ty <> [ Types.Int ] then
        failwith "Expression has wrong type in Unary"
      else e_ty
  | Ast.Binary (op, e1, e2) -> (
      let e1_ty = typecheck_expr e1 in
      let e2_ty = typecheck_expr e2 in
      match op with
      | Add when is_arr e1_ty && is_arr e2_ty -> (
          if e1_ty <> e2_ty then failwith "Array types are not equal"
          else
            (* Check dimensions of the array *)
            match (e1, e2) with
            | Ast.ArrayLit es1, Ast.ArrayLit es2 ->
                if List.length es1 <> List.length es2 then
                  failwith "Array dimensions are requiered to be equal"
                else dim_check es1 es2;
                e1_ty
            | _ -> failwith "Internal Error Type: Non arrays have array type")
      | Add
      | Sub
      | Mult
      | Div
      | Mod
      | Lt
      | Leq
      | Gt
      | Geq
        when e1_ty = [ Types.Int ] && e2_ty = [ Types.Int ] ->
          e1_ty
      | And
      | Or
        when e1_ty = [ Types.Bool ] && e2_ty = [ Types.Bool ] ->
          e1_ty
      | Eq
      | Neq
        when e1_ty = e2_ty ->
          e1_ty
      | _ -> failwith "Tried to apply Binary op to wrong types!")
  | Ast.FunCall { name; args } -> typecheck_fun_call name args
  | Ast.Subscript (e1, e2) -> (
      let arr_t = typecheck_expr e1 in
      let idx_t = typecheck_expr e2 in
      match (arr_t, idx_t) with
      | [ Types.Array { elem_type } ], [ Types.Int ] -> [ elem_type ]
      | _, [ Types.Int ] -> failwith "Trying to index into non-array"
      | _, _ -> failwith "Array Index must be an integer")

(* TODO: Fun calls can only appear on the rhs of assignments *)
(* Useful for function calls and procedures *)
and fun_helper name args =
  let p_t, ret_t =
    match Symbols.get_opt name with
    | None -> failwith "Calling undefined function!"
    | Some { t = Types.FunType { param_types; ret_type }; attr = Fun _ } ->
        (param_types, ret_type)
    | Some { t = Types.FunType _; _ } ->
        failwith "Internal Error: Function has non-Function attribute"
    | Some _ -> failwith "Function has non-functin type"
  in
  if List.length args <> List.length p_t then
    failwith "Number of arguments provided does not fit function type"
  else
    let args_t = List.concat_map typecheck_expr args in
    if not (List.equal ( = ) args_t p_t) then
      failwith "Some arguments do not have the correct type"
    else ret_t

and typecheck_fun_call name args =
  let ret_t = fun_helper name args in
  (* Return has to have some value otherwise it would be a procedure *)
  match ret_t with
  | [] -> failwith "Function call needs a non-empty return type!"
  | ts -> ts

let typecheck_arr_decl name t dims init =
  (* Check out dimensions Convert dims with value to true empty ones to false
  [1][][2] -> true, false, true*)
  let dims_b = List.map Option.is_some dims in
  (* If false we check if the rest has a true. then its an error *)
  let rec dim_check_cont ds =
    match ds with
    | false :: _ ->
        if List.exists Fun.id ds then
          failwith
            "A specified dimension cannot follow after an unspecified one"
        else ()
    | true :: ds' -> dim_check_cont ds'
    | [] -> ()
  in
  dim_check_cont dims_b;
  (* Only indices with type Int in dimensions 
  So remove all None values and check their types*)
  let dims_t = dims |> List.filter_map Fun.id |> List.map typecheck_expr in
  (* A function call [f(x)] is not allowed to return two elements
  Right now the list [[int], [int]] etc. SO if there are two elements its wrong*)
  List.iter
    (fun eds ->
      match eds with
      | [ x ] -> if x <> Types.Int then failwith "Non Int value used as Index!"
      | _ -> failwith "too many types for one index in array")
    dims_t;

  (* If one dim is specified we cannot have an initialzier *)
  let dim_spec = List.exists Fun.id dims_b in
  match init with
  | Some e ->
      if dim_spec then
        failwith "Cannot specify dimensions and initialize array. Not allowed"
      else
        let e_t = typecheck_expr e in
        if [ t ] <> e_t then failwith "Initializer has wrong type for the array"
  | None ->
      ();
      Symbols.add_local_var name t

let rec typecheck_stmt ret_type = function
  | Ast.Return es ->
      let es_ty = List.concat_map typecheck_expr es in
      if List.length es_ty <> List.length ret_type then
        failwith "Return has the wrong type"
      else if List.equal ( = ) es_ty ret_type then ()
        (* Return the position of difference *)
      else failwith "A Return type does not match with the Return statements"
  | Ast.LocalDecl { name; init; var_type } ->
      (match init with
      | Some e ->
          let e_ty = typecheck_expr e in
          if e_ty <> [ var_type ] then failwith "Initializer has the wrong type"
          else ()
      | None -> ());
      Symbols.add_local_var name var_type
  | Ast.ArrayDecl { name; a_type; dims; init } ->
      typecheck_arr_decl name a_type dims init
  | Ast.Assign { lhs; rhs } ->
      if not (List.for_all is_lval lhs) then
        failwith "Not all expression on lhs are lvalues!"
      else
        (* This unfolds function calls to its reutnr types. So 
        this is really the amount of expressions that will be assigned to lhs *)
        let rhs_t = List.concat_map typecheck_expr rhs in
        (* List.iter (fun a -> print_endline (Types.show a)) rhs_t; *)
        let lhs_t = List.concat_map typecheck_expr lhs in
        if List.length lhs_t <> List.length rhs_t then
          failwith "Amount of lvalues and rhs values mismatch!"
          (* TODO defined a helper that returns the values of where the mismatch happened *)
        else if not (List.equal ( = ) lhs_t rhs_t) then
          failwith "Types of LHS and RHS are mismatched"
  | Ast.Compound (Block bl) -> List.iter (typecheck_stmt ret_type) bl
  | Ast.If { guard; then_br; else_br } ->
      let _e_t = typecheck_expr guard in
      typecheck_stmt ret_type then_br;
      let _ = Option.map (typecheck_stmt ret_type) else_br in
      ()
  | Ast.While _ -> failwith "Statement typecheck not implemented"
  | Ast.Procedure { name; args } -> (
      let ret_t = fun_helper name args in
      (* Return has to have some value otherwise it would be a procedure *)
      match ret_t with
      | [] -> ()
      | _ -> failwith "Procedure call! Return type needs to be empty")

let typecheck_fun_decl Ast.{ name; params; f_type; body = Block bl } =
  let p_t, ret_ty =
    match f_type with
    | FunType { param_types; ret_type } -> (param_types, ret_type)
    | _ -> failwith "Function has non-function type"
  in
  (* Check if prev declared in module and if that matches  *)
  let check_old (old_entry : Symbols.entry) =
    if old_entry.t <> f_type then failwith "Incompatible function declarations"
    else
      match old_entry.attr with
      | Symbols.Fun { defined; _ } ->
          if defined then failwith "Function was already previously defined"
          else ()
      | _ ->
          failwith
            "Internal Error: Symbol has function but no function attributes"
  in
  (match Symbols.get_opt name with
  | Some entry -> check_old entry
  | None -> ());
  Symbols.add_fun name f_type true;
  (* Add params to table *)
  List.iter2 (fun p ty -> Symbols.add_local_var p ty) params p_t;
  List.iter (typecheck_stmt ret_ty) bl

let typecheck_top_level = function
  | Ast.FunDecl fn -> typecheck_fun_decl fn
  | Ast.Global { name; init; var_type } ->
      let init_t = Option.map typecheck_expr init in
      let _ =
        match init_t with
        | None -> ()
        | Some i_t ->
            if i_t <> [ var_type ] then
              failwith "Type of global and its initialzier mismatch"
            else ()
      in
      Symbols.add_global_var name var_type

let typecheck (Ast.Program decls) = List.iter typecheck_top_level decls
