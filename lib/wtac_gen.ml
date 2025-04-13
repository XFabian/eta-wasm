module W = struct
  include Wtac
end

let vars = ref []
let reset () = vars := []
let update_locals name t = 
  let obj : Wtac.obj = { name; t } in
  vars := obj :: !vars
(* Now that I have vars I need a symbol table to keep track of references in assignment
   So that they are correctly called
   That should be done in the semantic analysis passt: T
*)
let make_wtac_var ty =
  let dst_name = Unique_ids.make_temp ()
  in
  let t =
    match ty with
    | [ t ] -> t
    | _ -> failwith "Internal Error: A variable cannot have multiple types"
  in
  Symbols.add_local_var dst_name t;
  update_locals dst_name t;
  (* Symbols.add_var ~f_name dst_name ty; *)
  W.Var dst_name



let mem_of_wtac_var v =
  match v with
  | W.Var name -> W.Mem name
  | _ -> failwith "Internal Error: Should only be used on variables"

(* Code for the length function that we emit everytime 
we use Var 0 Var 1 to access the parameters via index
fn lenght (:int[]) :int 
The length is stored at a[-1] space is reserved when we alloc*)
let length_conversion () =
  let offset_dst = make_wtac_var [Types.Int] in
  let instr_calc = W.Binary{op=Sub; src1= Var "0"; src2 = Imm 4; dst = offset_dst} in 
  let dst = make_wtac_var [Types.Int] in
  (* actually use load here *)
  (* It also does not encode operand size *)
  [instr_calc; W.Load{src_ptr= offset_dst; dst}]

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
  | Neq -> W.Neq

let rec expr_gen = function
  | Ast.Int i -> ([], Wtac.Imm i)
  | Bool true -> ([], Wtac.Imm 1)
  | Bool false -> ([], Wtac.Imm 0)
  | Var x -> (
      match Symbols.get_opt x with
      | None -> failwith "Internal Error: Variable should be in symbol Map"
      | Some _entry ->
           ([], Wtac.Var x))
  | Unary (op, e) -> gen_unary op e
  | Binary (op, e1, e2) ->
      let e1_instr, v_e1 = expr_gen e1 in
      let e2_instr, v_e2 = expr_gen e2 in
      let w_op = convert_binop op in
      let e_ty = Typecheck.typecheck_expr e1 in
      let dst = make_wtac_var e_ty in
      let w_bin = W.Binary { op = w_op; src1 = v_e1; src2 = v_e2; dst } in
      let instrs = e1_instr @ e2_instr @ [ w_bin ] in
      (instrs, dst)
  | FunCall { name; args } ->
      (* TODO : For type need to lookup function call.
         However, if it returns multiple ones then the return type is wrong
           and we need to do additional work *)
      let dst = make_wtac_var [ Types.Int ] in
      let arg_instrs, arg_vals = List.map expr_gen args |> List.split in
      ( List.flatten arg_instrs
        @ [ W.FCall { name; args = arg_vals; dst = Some dst } ],
        dst )
  | Subscript (e1, e2) ->
      (* Subscript itself returns a memory location
         Assign decides if we need to deref it or store to that location
           Well return needs to deref. SO maybe another function that the stmts can call *)
      let e1_instr, v_e1 = expr_gen e1 in
      let e2_instr, v_e2 = expr_gen e2 in
      let elem_t, fin_addr =
        match Typecheck.typecheck_expr e1 with
        | [ Types.Array { elem_type } ] ->
            elem_type, (make_wtac_var [ Types.Array { elem_type } ])
        | [t] -> failwith ("Internal Error: This should have array type. Is:" ^ (Types.show t))
        | _ -> failwith "Wrong type"
      in
      (* A lot of offset calculations... *)
      let dst_offset = make_wtac_var [Types.Int] in
      let offset = W.Binary{op=Mult; src1=v_e2; src2= Imm (Types.get_size elem_t); dst=dst_offset} in 
      let offset_instr =
        W.Binary { op = Add; src1 = v_e1; src2 = dst_offset; dst = fin_addr }
      in
      let mem_dst = mem_of_wtac_var fin_addr in
      (e2_instr @ e1_instr @ [offset; offset_instr ], mem_dst)
  | _ -> failwith "WTac not implemented"

and gen_unary op e =
  let e_instr, v_e = expr_gen e in
  let e_ty = Typecheck.typecheck_expr e in
  let dst = make_wtac_var e_ty in
  let w_op = convert_unop op in
  let w_un = W.Unary { op = w_op; src = v_e; dst } in
  let instrs = e_instr @ [ w_un ] in
  (instrs, dst)

(* Thos does the converision of a[1] to its value
   So for memory accesses it creates a load to load from
   This will be used everywhere except assignments *)
let gen_and_convert_expr e =
  let instrs, res = expr_gen e in
  let t =
    match Typecheck.typecheck_expr e with
    | [ x ] -> x
    | _ -> failwith "Only a single type should be returned here"
  in
  match res with
  | Mem _ as src ->
      let dst = make_wtac_var [ t ] in
      (instrs @ [ W.Copy { src; dst } ], dst)
  | _ -> (instrs, res)

let rec stmt_gen = function
  | Ast.Return es ->
      let instrs_ls, res_ls = List.map expr_gen es |> List.split in
      List.concat instrs_ls @ [ Return res_ls ]
  | Ast.LocalDecl { name; init; var_type} -> (
      (* Ignore delcaration if it does not have an initializer.
         TODO: Wrong it still can be assigned to
         Then it is not useful *)
      update_locals name var_type;
      match init with
      | None -> []
      | Some e ->
          let instrs, res = expr_gen e in
          (* Symbols.add_var ~f_name name var_type; *)
          instrs @ [ W.Copy { src = res; dst = Var name } ])
  | Ast.ArrayDecl { name; init; dims; a_type } -> (
      (* We know it typechecked
         We need to alloc when either init is there or dimensions are specified
      *)
      (* If no init we use dimensions
         if we have a dimension than we alloc space for elemt_type
          This way we hande nested arrays
         TODO not finished*)
         update_locals name a_type;
      match init with
      | Some e ->
          let instrs, res = expr_gen e in
          instrs @ [ W.Copy { src = res; dst = Var name } ]
      | None ->
          (* Check dimension
             Needs loop to malloc inner arrays*)
          let rec alloc_dims ds elem_t =
            match ds with
            | Some e :: ds' -> (
                let e_instr, e_res = expr_gen e in
                let dst_bytes = make_wtac_var [ Types.Int ] in
                let offset_instr =
                  W.Binary
                    {
                      op = Mult;
                      src1 = e_res;
                      src2 = W.Imm (Types.get_size elem_t);
                      dst = dst_bytes;
                    }
                in
                let ptr = W.Var name in
                let instr =
                  e_instr
                  @ [
                      offset_instr;
                      W.FCall
                        {
                          name = "malloc";
                          args = [ dst_bytes ];
                          dst = Some ptr;
                        };
                    ]
                in
                let inner_ptr, inner_instr =
                  match elem_t with
                  | Types.Array { elem_type } -> alloc_dims ds' elem_type
                  | _ -> (ptr, [])
                in
                match inner_instr with
                | [] -> (ptr, instr)
                | _ ->
                    let break_label = Unique_ids.make_label "malloc_exit" in
                    let loop_label = Unique_ids.make_label "malloc_loop" in
                    (* Manual while loop *)
                    let dst_counter = make_wtac_var [ Types.Int ] in
                    let copy_counter =
                      W.Copy { src = Imm 0; dst = dst_counter }
                    in
                    let dst_guard = make_wtac_var [ Types.Bool ] in
                    let guard =
                      W.Binary
                        {
                          op = Lt;
                          src1 = dst_counter;
                          src2 = e_res;
                          dst = dst_guard;
                        }
                    in
                    let dst_guard_not = make_wtac_var [ Types.Bool ] in
                    let guard_neg =
                      W.Unary { op = Not; src = dst_guard; dst = dst_guard_not }
                    in
                    let loop_add =
                      W.Binary
                        {
                          op = Add;
                          src1 = dst_counter;
                          src2 = Imm 1;
                          dst = dst_counter;
                        }
                    in
                    let loop_guard =
                      W.BrIf { cond = dst_guard_not; l = break_label }
                    in
                    (* Need access to their pointer *)
                    let offset = make_wtac_var [ Types.Int ] in
                    let offset_instr =
                      W.Binary
                        {
                          op = Mult;
                          src1 = dst_counter;
                          src2 = W.Imm (Types.get_size elem_t);
                          dst = offset;
                        }
                    in
                    let ptr_loc = make_wtac_var [ a_type ] in
                    let ptr_offset =
                      W.Binary
                        { op = Add; src1 = ptr; src2 = offset; dst = ptr_loc }
                    in
                    (* Todo There should be Mems in here *)
                    let mem_ptr = mem_of_wtac_var ptr_loc in
                    let copy_mem = W.Copy { src = inner_ptr; dst = mem_ptr } in
                    let loop_malloc =
                      W.Loop
                        {
                          label = loop_label;
                          body =
                            [ guard; guard_neg; loop_guard ]
                            @ inner_instr
                            @ [
                                offset_instr;
                                ptr_offset;
                                copy_mem;
                                loop_add;
                                W.Br loop_label;
                              ];
                        }
                    in
                    let block_m =
                      W.Block { label = break_label; body = [ loop_malloc ] }
                    in
                    (ptr, instr @ [ copy_counter; block_m ]))
            (* Inner stuff is not stored! *)
            (* This is very ugly to return soe imm here but necessart*)
            | None :: _ -> (Imm 0, [])
            | [] -> (Imm 0, [])
          in

          let _, instr =
            match List.hd dims with
            | Some _ -> (
                match a_type with
                | Types.Array { elem_type } -> alloc_dims dims elem_type
                | _ -> failwith "Internal Error: Array has non array type")
            | None ->
                (* This means no init and no dimensins to init, so we do nothing here *)
                (Imm 0, [])
          in
          instr)
  | Ast.Assign { lhs; rhs } ->
      let instrs_rhs, rhs_res = List.map gen_and_convert_expr rhs |> List.split in
      (* TODO with arrays this will be trickier This are not converted *)
      let instrs_lhs, lhs_res = List.map expr_gen lhs |> List.split in
      (* generate a copy for each pair
         TODO This does not work. This can be unbalanced
         right now x,y = add() which returns 2 elements
         Function can return a multi value
         Also rhs needs to be copied into temporaries and then transfered*)
      let dst_rhs, rhs_instrs_copies =
        List.fold_left_map
          (fun acc rhs ->
            let dst = make_wtac_var [ Types.Int ] in
            (acc @ [ dst ], W.Copy { src = rhs; dst }))
          [] rhs_res
      in
      let copies =
        List.combine lhs_res dst_rhs
        |> List.map (fun (lhs, rhs) -> W.Copy { src = rhs; dst = lhs })
      in
      List.concat instrs_rhs @ List.concat instrs_lhs @ rhs_instrs_copies
      @ copies
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
              @ W.[ BrIf { cond = dst; l = else_label } ]
              @ else_instrs
              @ W.[ Br end_label ]);
          }
      in
      [ W.Block { label = end_label; body = [ else_block ] @ then_instrs } ]
  | Ast.While { guard; body } ->
      let instrs, dst = expr_gen guard in
      let body_instrs = stmt_gen body in
      let block_label = Unique_ids.make_label "while_end" in
      let loop_label = Unique_ids.make_label "while_loop" in
      (* Negate condition *)
      let neg_dst = make_wtac_var [ Types.Bool ] in
      let neg_op = W.Unary { op = Not; src = dst; dst = neg_dst } in
      let loop_block =
        W.Loop
          {
            label = loop_label;
            body =
              instrs
              @ [ neg_op; W.BrIf { cond = neg_dst; l = block_label } ]
              @ body_instrs @ [ W.Br loop_label ];
          }
      in
      let block = W.Block { label = block_label; body = [ loop_block ] } in
      [ block ]
  | Ast.Procedure { name; args } ->
      let arg_instrs, arg_vals = List.map expr_gen args |> List.split in
      List.flatten arg_instrs
      @ [ W.FCall { name; args = arg_vals; dst = None } ]

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
