module W = struct
  include Wtac
end


let vars = ref []
let reset () = vars := []

(* Now that I have vars I need a symbol table to keep track of references in assignment
   So that they are correctly called
   That should be done in the semantic analysis passt: T
*)
let make_wtac_var ?name ty =
  let dst_name =
    match name with
    | Some x -> x
    | None -> Unique_ids.make_temp ()
  in
  let obj : Wtac.obj = {name=dst_name; t = ty} in
  vars := obj::(!vars);
  (* Symbols.add_var ~f_name dst_name ty; *)
  W.Var dst_name

let convert_binop = function
  | Ast.Add -> W.Add
  | Sub -> W.Sub
  | Mult -> W.Mult

let rec expr_gen = function
  | Ast.Int i -> ([], Wtac.Imm i)
  | Bool true -> ([], Wtac.Imm 1)
  | Bool false -> ([], Wtac.Imm 0)
  | Var x -> ([], Wtac.Var x)
  | Binary (op, e1, e2) ->
      let e1_instr, v_e1 = expr_gen e1 in
      let e2_instr, v_e2 = expr_gen e2 in
      let w_op = convert_binop op in
      (* TODO typing is wrong here! *)
      let dst = make_wtac_var Types.Int in
      let w_bin = W.Binary { op = w_op; src1 = v_e1; src2 = v_e2; dst } in
      let instrs = e1_instr @ e2_instr @ [ w_bin ] in
      (instrs, dst)

let stmt_gen = function
  | Ast.Return e ->
      let instrs, res = expr_gen e in
      instrs @ [ Return res ]
  | Ast.LocalDecl { name; init; var_type } -> (
      (* Ignore delcaration if it does not have an initializer.
         TODO: Wrong it still can be assigned to
         Then it is not useful *)
      match init with
      | None -> []
      | Some e ->
          let instrs, res = expr_gen e in
          let dst = make_wtac_var ~name var_type in
          (* Symbols.add_var ~f_name name var_type; *)
          instrs @ [ W.Copy { src = res; dst } ])
  | Ast.Assign { lhs; rhs } ->
      let instrs_rhs, rhs_res = expr_gen rhs in
      (* TODO with arrays this will be trickier *)
      let instrs_lhs, lhs_res = expr_gen lhs in
      instrs_lhs @ instrs_rhs @ [ W.Copy { src = rhs_res; dst = lhs_res } ]

let gen_function Ast.{ name; params; f_type; body = Block stmts } =
  let instrs = List.concat_map stmt_gen stmts in
  let f =
    Wtac.Function { name; body = instrs; params; f_type; locals = !vars }
  in
  reset ();
  f

let gen_top_level decl =
  match decl with
  | Ast.FunDecl fn -> gen_function fn
  | Ast.Global { name; var_type; init } -> 
    let value = match init with 
    | Some Ast.Int i -> Wtac.Imm i
    | Some Bool true -> Wtac.Imm 1
    | Some Bool false -> Wtac.Imm 0
    | Some _ -> failwith "Internal error. A non literal is used to initialzie a global"
    | None -> Wtac.Imm (-1) (* like an undefined*)
  in
   Data { name; t = var_type; value }

let gen (Ast.Program decls) : Wtac.program =
  let wtac_decls = List.map gen_top_level decls in
  Wtac.Program wtac_decls
