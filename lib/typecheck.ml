let rec typecheck_expr e : Types.t =
  match e with
  | Ast.Var x -> (Symbols.get x).t
  | Ast.Int _ -> Types.Int
  | Ast.Bool _ -> Types.Bool
  | Ast.ArrayLit es -> (
      let es_t = List.map typecheck_expr es in
      match es_t with
      | [] -> Types.Unknown (* {} *)
      | x :: xs ->
          let res = List.for_all (( = ) x) xs in
          if res then x
          else failwith "Not all elements in array literal have same type")
  | Ast.Unary (Not, e) ->
      let e_ty = typecheck_expr e in
      if e_ty != Types.Bool then failwith "Expression has wrong type in Unary"
      else e_ty
  | Ast.Unary (Neg, e) ->
      let e_ty = typecheck_expr e in
      if e_ty != Types.Int then failwith "Expression has wrong type in Unary"
      else e_ty
  | Ast.Binary (op, e1, e2) -> (
      let e1_ty = typecheck_expr e1 in
      let e2_ty = typecheck_expr e2 in
      match op with
      | Add
      | Sub
      | Mult
      | Div
      | Mod
      | Lt
      | Leq
      | Gt
      | Geq
        when e1_ty = Types.Int && e2_ty = Types.Int ->
          e1_ty
      | And
      | Or
        when e1_ty = Types.Bool && e2_ty = Types.Bool ->
          e1_ty
      | Eq
      | Neq
        when e1_ty = e2_ty ->
          e1_ty
      | _ -> failwith "Tried to apply Binary op to wrong types!")
  | Ast.FunCall _
  | Ast.Subscript (_, _) ->
      failwith "Not implemented"

let typecheck_top_level = function
  | Ast.FunDecl _ -> ()
  | Ast.Global { name; init; var_type } ->
      let init_t = Option.map typecheck_expr init in
      let _ =
        match init_t with
        | None -> ()
        | Some i_t ->
            if i_t != var_type then
              failwith "Type of global and its initialzier mismatch"
            else ()
      in
      Symbols.add_global_var name var_type

let typecheck (Ast.Program decls) = List.iter typecheck_top_level decls
