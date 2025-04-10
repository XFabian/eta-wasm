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
  let obj : Wtac.obj = { name = dst_name; t = ty } in
  vars := obj :: !vars;
  (* Symbols.add_var ~f_name dst_name ty; *)
  W.Var dst_name

let convert_unop = function
  | Ast.Neg -> W.Negate
  | Ast.Not -> W.Not

let convert_binop = function
  | Ast.Add -> W.Add
  | Sub -> W.Sub
  | Mult -> W.Mult
  | And -> W.And
  | Or -> W.Or
  | Div -> W.Div
  | Mod -> W.Mod
  | Lt -> W.Lt
  | Leq -> W.Leq
  | Gt -> W.Gt
  | Geq -> W.Geq
  | Eq -> W.Eq
  | Neq-> W.Neq

let rec expr_gen = function
  | Ast.Int i -> ([], Wtac.Imm i)
  | Bool true -> ([], Wtac.Imm 1)
  | Bool false -> ([], Wtac.Imm 0)
  | Var x -> ([], Wtac.Var x)
  | Unary (op, e) -> gen_unary op e
  | Binary (And, _, _)
  | Binary (Or, _, _) ->
      failwith "And and or not implemented"
  | Binary (op, e1, e2) ->
      let e1_instr, v_e1 = expr_gen e1 in
      let e2_instr, v_e2 = expr_gen e2 in
      let w_op = convert_binop op in
      (* TODO typing is wrong here! *)
      let dst = make_wtac_var Types.Int in
      let w_bin = W.Binary { op = w_op; src1 = v_e1; src2 = v_e2; dst } in
      let instrs = e1_instr @ e2_instr @ [ w_bin ] in
      (instrs, dst)
  | _ -> failwith "WTac not implemented"

and gen_unary op e =
  let e_instr, v_e = expr_gen e in
  (* TODO wrong type! *)
  let dst = make_wtac_var Types.Int in
  let w_op = convert_unop op in
  let w_un = W.Unary { op = w_op; src = v_e; dst } in
  let instrs = e_instr @ [ w_un ] in
  (instrs, dst)

(* Naive way for and expressions
and gen_and_exp e1 e2 =
  let e1_instr, src1 = expr_gen e1 in
  let e2_instr, src2 = expr_gen e2 in
  let false_label = Unique_ids.make_label "and_false" in
  let dst = make_wtac_var Types.Int in
  let end_label = Unique_ids.make_label "and_end" in
  (* CJump is a bit annoying since we need to mark the immediate locas as well
    and jump there. So there is a straightline jump added there.
    TODO. Check what LLVM does for that or if that is optimized away *)
  let e1_true_label = Unique_ids.make_label "and_true1_" in
  let e2_true_label = Unique_ids.make_label "and_true2_" in
  let instructions =
    e1_instr
    @ [
        W.CJump { cond = src1; l1 = e1_true_label; l2 = false_label };
        Label e1_true_label;
      ]
    @ e2_instr
    @ [
        CJump { cond = src2; l1 = e2_true_label; l2 = false_label };
        Label e2_true_label;
        Copy { src = W.Imm 1; dst };
        Jump end_label;
        Label false_label;
        Copy { src = W.Imm 0; dst };
        Label end_label;
      ]
  in
  (instructions, dst)
(* Naive way for or expressions *)

and gen_or_exp e1 e2 =
  let e1_instr, src1 = expr_gen e1 in
  let e2_instr, src2 = expr_gen e2 in
  let true_label = Unique_ids.make_label "or_true" in
  let dst = make_wtac_var Types.Int in
  let end_label = Unique_ids.make_label "or_end" in
  (* CJump is a bit annoying since we need to mark the immediate locas as well
    and jump there. So there is a straightline jump added there.
    TODO. Check what LLVM does for that or if that is optimized away *)
  let e1_false_label = Unique_ids.make_label "or_false1_" in
  let e2_false_label = Unique_ids.make_label "or_false2_" in
  let instructions =
    e1_instr
    @ [
        W.CJump { cond = src1; l1 = true_label; l2 = e1_false_label };
        Label e1_false_label;
      ]
    @ e2_instr
    @ [
        CJump { cond = src2; l1 = true_label; l2 = e2_false_label };
        Label e2_false_label;
        Copy { src = W.Imm 0; dst };
        Jump end_label;
        Label true_label;
        Copy { src = W.Imm 1; dst };
        Label end_label;
      ]
  in
  (instructions, dst) *)

let rec stmt_gen = function
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
  | Ast.Compound (Block bl) -> List.concat_map stmt_gen bl
  | If { guard; then_br; else_br } ->
      let else_label = Unique_ids.make_label "if_else" in
      let end_label = Unique_ids.make_label "if_end" in
      (* let instrs = control_flow_gen guard t_label else_label in  *)
      let instrs, dst = expr_gen guard in
      let then_instrs = stmt_gen then_br in
      let else_instrs =
        else_br |> Option.map stmt_gen |> Option.to_list |> List.flatten
      in
      let else_block =
        W.Block
          {
            label = else_label;
            body =
              (instrs
              @ W.[ BrIf { cond = dst; l = end_label } ]
              @ else_instrs
              @ W.[ Br end_label ]);
          }
      in
      [ W.Block { label = end_label; body = [ else_block ] @ then_instrs } ]
  | Ast.While _
  | Ast.Procedure _ ->
      failwith " while not implemeted"

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
      let value =
        match init with
        | Some (Ast.Int i) -> Wtac.Imm i
        | Some (Bool true) -> Wtac.Imm 1
        | Some (Bool false) -> Wtac.Imm 0
        | Some _ ->
            failwith
              "Internal error. A non literal is used to initialzie a global"
        | None -> Wtac.Imm (-1)
        (* like an undefined*)
      in
      Data { name; t = var_type; value }

let gen (Ast.Program decls) : Wtac.program =
  let wtac_decls = List.map gen_top_level decls in
  Wtac.Program wtac_decls
