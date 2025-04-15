(* Resolves variables found in the file *)

(* Checks if functoin or variable was not declared beforehand and
   changes the name to a unique name *)
(* This returns the AST again
   For pretty printing purposes we have reverse bindings as well
   so that we can recover the origina names
*)
open Ast

(* State could contain current functoin as well
So we could start at 0 for each function and rename parameters according to the function they are in
Since functoin names need to be unique *)
module StackMap = struct
  module StringMap = Map.Make (String)

  (* Global is not really necessary, since a variable is global
     if it is in the topmost scope *)
  type id_entry = { unique_name : string; global : bool }
  type t = { map_scope : id_entry StringMap.t list }

  let pp_map ppf (m : id_entry StringMap.t) =
    StringMap.iter
      (fun k v -> Format.fprintf ppf "%s -> %s@\n" k v.unique_name)
      m

  let new_map () = { map_scope = [ StringMap.empty ] }

  let push ms =
    let m = StringMap.empty in
    { map_scope = m :: ms.map_scope }

  let pop ms =
    match ms.map_scope with
    | [] -> failwith "Tried to pop empty Var delcaration Map"
    | _ :: ms' -> { map_scope = ms' }

  let add k v ms =
    match ms.map_scope with
    | [] -> failwith "Scope map cannot be empty"
    | m :: ms' ->
        let m' = StringMap.add k v m in
        { map_scope = m' :: ms' }

  let rec find_opt (x : string) (ms : t) : id_entry option =
    match ms.map_scope with
    | [] -> None
    | m :: ms' -> (
        match StringMap.find_opt x m with
        | None -> find_opt x { map_scope = ms' }
        | Some entry -> Some entry)

  (* Only looks in th ecurrent active scope *)
  let find_opt_curr (x : string) (ms : t) : id_entry option =
    match ms.map_scope with
    | [] -> None
    | m :: _ -> (
        match StringMap.find_opt x m with
        | None -> None
        | Some entry -> Some entry)
end

(* Maybe use a bidirectional map to get back the original values
   But maybe not worth the hassle
   Also pretty easy to implement it is pretty printing where you do alook up first
*)
(* Where we found the variable *)
type context = Glob | Fn of string

(* Stack of maps representing scopes
   Pretty cool data structure that should be covered in lecture!
*)
let resolve_local_var_decl_helper state name =
  match StackMap.find_opt_curr name state with
  | Some _entry -> failwith "Duplicate variable declaration"
  | None ->
      let unique_name = Unique_ids.make_named_tmp name in
      let open StackMap in
      let new_entry = { unique_name; global = false } in
      let new_state = StackMap.add name new_entry state in
      (new_state, unique_name)

(* Unique names is for shadowing in nested blocks *)
let rec resolve_expr state = function
  | Ast.Var x -> (
      match StackMap.find_opt x state with
      | Some entry -> Var entry.unique_name
      | None -> failwith "Duplicate Variable found")
  | Ast.Unary (op, e) ->
      let res_e = resolve_expr state e in
      Unary (op, res_e)
  | Ast.Binary (op, e1, e2) ->
      let res_e1 = resolve_expr state e1 in
      let res_e2 = resolve_expr state e2 in
      Binary (op, res_e1, res_e2)
  | (Ast.Int _ | Ast.Bool _) as const -> const
  | FunCall { name; args } -> (
      match StackMap.find_opt name state with
      | None -> failwith "Function not declared!"
      | Some entry ->
          let args_res = List.map (resolve_expr state) args in
          FunCall { name = entry.unique_name; args = args_res })
  | ArrayLit es -> ArrayLit (List.map (resolve_expr state) es)
  | Subscript (e1, e2) ->
      let res_e1 = resolve_expr state e1 in
      let res_e2 = resolve_expr state e2 in
      Subscript (res_e1, res_e2)

let resolve_local_var_decl state (vdecl : var_declaration) =
  let new_state, unique_name = resolve_local_var_decl_helper state vdecl.name in
  let init_res = Option.map (resolve_expr new_state) vdecl.init in
  (new_state, { vdecl with name = unique_name; init = init_res })

(* TODO: I probably do not need to return the state here *)
let rec resolve_stmt state = function
  | Ast.Return e ->
      let res_e = List.map (resolve_expr state) e in
      (state, Return res_e)
  | LocalDecl vdecl ->
      let fin_state, res_vdecl = resolve_local_var_decl state vdecl in
      (fin_state, LocalDecl res_vdecl)
  | ArrayDecl decl ->
      let fin_state, unique_name =
        resolve_local_var_decl_helper state decl.name
      in
      let init_res = Option.map (resolve_expr fin_state) decl.init in
      let dims_res = List.map (Option.map (resolve_expr fin_state)) decl.dims in
      ( fin_state,
        ArrayDecl
          { decl with name = unique_name; dims = dims_res; init = init_res } )
  | Assign { lhs; rhs } ->
      let res_rhs = List.map (resolve_expr state) rhs in
      let res_lhs = List.map (resolve_expr state) lhs in
      (state, Assign { lhs = res_lhs; rhs = res_rhs })
  | Compound bl ->
      let new_state = StackMap.push state in
      let fin_state, res_bl = resolve_block new_state bl in
      (StackMap.pop fin_state, Compound res_bl)
  | If { guard; then_br; else_br } ->
      let res_guard = resolve_expr state guard in
      let inter_state1, res_then = resolve_stmt state then_br in
      let fin_state, res_else =
        match Option.map (resolve_stmt inter_state1) else_br with
        | Some (f_state, res) -> (f_state, Some res)
        | None -> (inter_state1, None)
      in
      let if_res =
        If { guard = res_guard; then_br = res_then; else_br = res_else }
      in
      (fin_state, if_res)
  | While { guard; body } ->
      let res_guard = resolve_expr state guard in
      let fin_state, res_body = resolve_stmt state body in
      let res_while = While { guard = res_guard; body = res_body } in
      (fin_state, res_while)
  | Procedure { name; args } ->
      let res_args = List.map (resolve_expr state) args in
      (state, Procedure { name; args = res_args })

and resolve_block state (Block stmts) =
  let fin_state, res_stmts = List.fold_left_map resolve_stmt state stmts in
  (fin_state, Block res_stmts)

let resolve_params state params =
  let fold_params new_map param = resolve_local_var_decl_helper new_map param in
  List.fold_left_map fold_params state params

let resolve_function state { name; params; f_type; body } =

      let new_scope = StackMap.push state in
      let inner_scope, res_params = resolve_params new_scope params in
      let fin_state, new_body = resolve_block inner_scope body in
      ( StackMap.pop fin_state,
        { name; params = res_params; f_type; body = new_body } )

let resolve_top_level state = function
  | Ast.FunDecl fn ->
      let fin_state, res_fn = resolve_function state fn in
      (fin_state, FunDecl res_fn)
  | Global v_decl -> (
      match StackMap.find_opt_curr v_decl.name state with
      | Some _entry ->
          failwith (Printf.sprintf "Global %s already defined" v_decl.name)
      | None ->
          let open StackMap in
          let entry = { unique_name = v_decl.name; global = true } in
          let fin_state = StackMap.add v_decl.name entry state in
          (fin_state, Global v_decl))

let resolve_only_top_level state = function 
  | Ast.FunDecl fn ->
    let open StackMap in 
    let en = {unique_name= fn.name; global = true} in
    let new_state = StackMap.add fn.name en state in
    new_state
  | Global _ -> 
    (* Globals need to be declared first so do nothing here *)
    state

let resolve (Ast.Program decls) =
  let init_state = StackMap.new_map () in
  (* Add length function *)
  let open StackMap in 
  let en = {unique_name="length"; global = true} in
  let ini_state = StackMap.add "length" en init_state in
  let new_state = List.fold_left resolve_only_top_level ini_state decls in
  let fin_state, resolved_decls =
    List.fold_left_map resolve_top_level new_state decls
  in
  (fin_state, Ast.Program resolved_decls)

(* TODO we first need to resolve the top level identifers
   So that mutually recusrive functions are working
   Check f that is ok for globals as well
   So collect those identifiers first *)

(* Can parameters be shadowed? *)
