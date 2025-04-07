module W = struct
  include Wtac
end

(* Maybe record that information somewhere. So during one function we can record all
created variable
then we reset it in the end
Very ugly rioght now
 *)
let vars = ref []
let reset () = vars := []

let make_wtac_var () =
  let dst_name = Unique_ids.make_temp () in
  vars := dst_name :: !vars;
  W.Var dst_name

let convert_binop = function
  | Ast.Add -> W.Add
  | Sub -> W.Sub
  | Mult -> W.Mult

let rec expr_gen = function
  | Ast.Int i -> ([], Wtac.Imm i)
  | Binary (op, e1, e2) ->
      let e1_instr, v_e1 = expr_gen e1 in
      let e2_instr, v_e2 = expr_gen e2 in
      let w_op = convert_binop op in
      let dst = make_wtac_var () in
      let w_bin = W.Binary { op = w_op; src1 = v_e1; src2 = v_e2; dst } in
      let instrs = e1_instr @ e2_instr @ [ w_bin ] in
      (instrs, dst)

let gen_function Ast.{ name; params; f_type; body } =
  let instrs, _res_v = expr_gen body in
  let f =
    Wtac.Function { name; body = instrs; params; f_type; locals = !vars }
  in
  reset ();
  f

let gen_top_level decl =
  match decl with
  | Ast.FunDecl fn -> gen_function fn

let gen (Ast.Program decls) : Wtac.program =
  let wtac_decls = List.map gen_top_level decls in
  Wtac.Program wtac_decls
