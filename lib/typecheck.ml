let typecheck_expr _e : Types.t = Types.Int
let typecheck_globals () = ()

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
