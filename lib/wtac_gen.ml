module W = struct
  include Wtac
end

let convert_type = function
  | Types.Int -> W.I64
  | Types.Bool
  | Types.Array _ ->
      W.I32
  | _ -> failwith "Internal Error: Cannot convert functoin type"

let vars = ref []
let reset () = vars := []

let update_locals name t =
  let obj : Wtac.obj = { name; t = convert_type t } in
  vars := obj :: !vars

(* Now that I have vars I need a symbol table to keep track of references in assignment
   So that they are correctly called
   That should be done in the semantic analysis passt: T
*)
let make_wtac_var ty =
  let dst_name = Unique_ids.make_temp () in
  let t =
    match ty with
    | [ t ] -> t
    | _ -> failwith "Internal Error: A variable cannot have multiple types"
  in
  Symbols.add_local_var dst_name t;
  update_locals dst_name t;
  (* Symbols.add_var ~f_name dst_name ty; *)
  W.Var dst_name

(* Helper to typecheck expression and unfold the type since [t] is returned
   Only function calls can return multiple values. In most cases we know
   it is only one *)
let type_and_unfold e =
  let e_ty = Typecheck.typecheck_expr e in
  match e_ty with
  | [ t ] -> t
  | _ ->
      failwith
        "Internal Error: Type and unfold should only be called\n\
        \  when it is clear that the expression is not a function call"

let mem_of_wtac_var v =
  match v with
  | W.Var name -> W.Mem name
  | _ -> failwith "Internal Error: Should only be used on variables"

(* Code for the length function that we emit everytime
   we use Var 0 Var 1 to access the parameters via index
   fn lenght (:int[]) :int
   The length is stored at a[-1] space is reserved when we alloc*)
let length_conversion () =
  let offset_dst = make_wtac_var [ Types.Array{elem_type=Types.Int} ] in
  (* Bool has same size as pointer *)
  let p_name =Unique_ids.make_temp () in
  Symbols.add_local_var p_name Types.Bool;
  let instr_calc =
    W.Binary
      {
        op = Sub;
        src1 = Var p_name;
        src2 = Imm (4, W.I32);
        dst = offset_dst;
        t = W.I32;
      }
  in
  let dst = make_wtac_var [ Types.Int ] in
  (* actually use load here *)
  (* It also does not encode operand size
       Copy trick looking up array type does not work
     on int[] array we still want *)
  (* TODO. Note that length is a 32 bit integer since the size of an array
     i at most 2^32 this then needs to be upcasted to be used in computatoins *)
  let instrs =[ instr_calc; W.Copy { src = mem_of_wtac_var offset_dst; dst; t = W.I64 }; W.Return [dst]] in 
  (* This type is hacky, we would like t use wtac types *)
  
  let f_type = Types.FunType{param_types=[Types.Bool]; ret_type=[Types.Int]}in
  let fun_le = Wtac.Function { name= "length"; body = instrs; params=[p_name]; f_type; locals = !vars }
  in
  reset();
  fun_le

(* Helper to compute offset
   [arr_size] size of the array as a dst of a computation
   [elem_t] elemnt type of the array
   Computes offset and mallocs.
   Returns a pointer and the insturctions*)
let arr_offset_calculation arr_size elem_t =
  let num_bytes = make_wtac_var [ Types.Bool ] in
  let num_bytes_length = make_wtac_var [ Types.Bool ] in
  (* This computes arr_size * size(elem_t) + 8
     +8 because we store th elength at name[-1] *)
  let offset_instr =
    W.Binary
      {
        op = Mult;
        src1 = arr_size;
        src2 = W.Imm (Types.get_size elem_t, W.I32);
        dst = num_bytes;
        t = W.I32;
      }
  in
  (* For offset *)
  let offset_length =
    W.Binary
      {
        op = Add;
        src1 = num_bytes;
        src2 = W.Imm (8, W.I32);
        dst = num_bytes_length;
        t = W.I32;
      }
  in

  let fin_ptr = make_wtac_var [ Types.Array { elem_type = elem_t } ] in
  (* Types does not really matter *)
  let arr_ptr = make_wtac_var [ Types.Array { elem_type = elem_t } ] in

  let instr =
    [
      offset_instr;
      offset_length;
      W.FCall
        { name = "eta_malloc"; args = [ num_bytes_length ]; dst = Some arr_ptr };
      W.Copy { src = arr_size; dst = mem_of_wtac_var arr_ptr; t = W.I64 };
      W.Binary
        {
          op = Add;
          src1 = arr_ptr;
          src2 = W.Imm (8, I32);
          dst = fin_ptr;
          t = I32;
        };
    ]
  in
  (fin_ptr, instr)

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
  | Ast.Int i -> ([], Wtac.Imm (i, W.I64))
  | Bool true -> ([], Wtac.Imm (1, W.I32))
  | Bool false -> ([], Wtac.Imm (0, W.I32))
  | Var x -> (
      match Symbols.get_opt x with
      | None -> failwith "Internal Error: Variable should be in symbol Map"
      | Some _entry -> ([], Wtac.Var x))
  | Unary (op, e) -> gen_unary op e
  | Binary (op, e1, e2) ->
      let e1_instr, v_e1 = expr_gen e1 in
      let e2_instr, v_e2 = expr_gen e2 in
      let w_op = convert_binop op in
      let e_ty = type_and_unfold e1 in
      let res_type = match op with 
      | Eq | Neq | Lt | Leq | Gt | Geq -> Types.Bool 
      | _ -> e_ty
      in
      let dst = make_wtac_var [ res_type ] in
      let w_bin =
        W.Binary
          { op = w_op; src1 = v_e1; src2 = v_e2; dst; t = convert_type e_ty }
      in
      let instrs = e1_instr @ e2_instr @ [ w_bin ] in
      (instrs, dst)
  | FunCall { name; args } ->
      (* TODO : For type need to lookup function call.
         However, if it returns multiple ones then the return type is wrong
           and we need to do additional work *)
         (* Only match with single return *)
        let ret_type = match (Symbols.get name).t with
        | Types.FunType{ret_type=[r]; _} -> r
        | _ -> failwith "Internal Error: Function call should have a function type"
        in

        let dst = make_wtac_var [ ret_type ] in
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
            (elem_type, make_wtac_var [ Types.Array { elem_type } ])
        | [ t ] ->
            failwith
              ("Internal Error: This should have array type. Is:" ^ Types.show t)
        | _ -> failwith "Wrong type"
      in
      (* A lot of offset calculations... *)
      let dst_offset = make_wtac_var [ Types.Bool ] in
      (* TODO Types may be wrong *)
      let offset =
        W.Binary
          {
            op = Mult;
            src1 = v_e2;
            src2 = Imm (Types.get_size elem_t, W.I32);
            dst = dst_offset;
            t = W.I32;
          }
      in
      let offset_instr =
        W.Binary
          {
            op = Add;
            src1 = v_e1;
            src2 = dst_offset;
            dst = fin_addr;
            t = W.I32;
          }
      in
      let mem_dst = mem_of_wtac_var fin_addr in
      (e2_instr @ e1_instr @ [ offset; offset_instr ], mem_dst)
  | ArrayLit es -> 
    let es_ty = List.map Typecheck.typecheck_expr es in
    let elem_type = match es_ty with 
    (* This happens for {} we still mallo to store length *)
    | [] -> Types.Int 
    | [e_ty]::_ -> e_ty 
    | _ -> failwith "Internal Error: Array contains tuple type"
  in  
    let len = List.length es in
    let arr_ptr, arr_instr = arr_offset_calculation (Imm (len, I64)) elem_type in 
    let move_helper i e = 
      let e_instr, e_res = expr_gen e in
      let offset_dst = make_wtac_var [Types.Bool] in
      let offset = W.Binary{op=Mult; src1 = Imm (i, I32); src2= W.Imm(Types.get_size elem_type, I32); dst = offset_dst; t =I32} in
      let offset_ptr_dst = make_wtac_var [Types.Array{elem_type}]
      in 
      let ptr_offset_instr = W.Binary{op=Add; src1= arr_ptr; src2 = offset_dst; dst=offset_ptr_dst; t=I32} in
      let move = W.Copy{src=e_res; dst=mem_of_wtac_var offset_ptr_dst; t= convert_type elem_type} in 
      e_instr @ [offset; ptr_offset_instr; move]
    in
    let instr = List.mapi move_helper es |> List.flatten in

    arr_instr @ instr, arr_ptr

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
  let t = type_and_unfold e in
  match res with
  | Mem _ as src ->
      let dst = make_wtac_var [ t ] in
      (instrs @ [ W.Copy { src; dst; t = convert_type t } ], dst)
  | _ -> (instrs, res)

(* The dims are filtered and do not contain None
   We require that at least 1 dimension is there
   and [elem_type] the element type of the array
   Returns a pointer to the allocated array and the instructions*)
let rec array_init_help elem_t dims =
  match dims with
  | [ e ] ->
      let e_instr, e_res = expr_gen e in
      let arr_ptr, arr_instr = arr_offset_calculation e_res elem_t in
      (arr_ptr, e_instr @ arr_instr)
  | e :: ds ->
      let e_instr, e_res = expr_gen e in
      let break_label = Unique_ids.make_label "malloc_exit" in
      let loop_label = Unique_ids.make_label "malloc_loop" in
      let m_arr_ptr, m_arr_instr = arr_offset_calculation e_res elem_t in
      let ele_t =
        match elem_t with
        | Types.Array { elem_type } -> elem_type
        | _ -> failwith "Internal Error: Mismatch between types and dimensions"
      in
      (* Those instructions will be looped *)
      let inner_ptr, inner_instr = array_init_help ele_t ds in
      (* Manual while loop *)
      let dst_counter = make_wtac_var [ Types.Bool ] in
      let copy_counter =
        W.Copy { src = Imm (0, W.I32); dst = dst_counter; t = W.I32 }
      in
      let dst_guard = make_wtac_var [ Types.Bool ] in
      let guard =
        W.Binary
          {
            op = Lt;
            src1 = dst_counter;
            src2 = e_res;
            dst = dst_guard;
            t = W.I32;
          }
      in
      (* Setup loop code  *)
      let dst_guard_not = make_wtac_var [ Types.Bool ] in
      let guard_neg =
        W.Unary { op = Not; src = dst_guard; dst = dst_guard_not }
      in
      let loop_add =
        W.Binary
          {
            op = Add;
            src1 = dst_counter;
            src2 = Imm (1, W.I32);
            dst = dst_counter;
            t = W.I32;
          }
      in
      let loop_guard = W.BrIf { cond = dst_guard_not; l = break_label } in
      (* Calulate offset *)
      let offset = make_wtac_var [ Types.Bool ] in
      let offset_instr =
        W.Binary
          {
            op = Mult;
            src1 = dst_counter;
            src2 = W.Imm (Types.get_size elem_t, convert_type elem_t);
            dst = offset;
            t = W.I32;
          }
      in
      let ptr_loc = make_wtac_var [ elem_t ] in
      let m_arr_ptr_offset =
        W.Binary
          {
            op = Add;
            src1 = m_arr_ptr;
            src2 = offset;
            dst = ptr_loc;
            t = W.I32;
          }
      in
      let mem_ptr = mem_of_wtac_var ptr_loc in
      let assign_mem =
        W.Copy { src = inner_ptr; dst = mem_ptr; t = convert_type elem_t }
      in
      let loop_malloc =
        W.Loop
          {
            label = loop_label;
            body =
              [ guard; guard_neg; loop_guard ]
              @ inner_instr
              @ [
                  offset_instr;
                  m_arr_ptr_offset;
                  assign_mem;
                  loop_add;
                  W.Br loop_label;
                ];
          }
      in
      let block_m =
        W.Block
          {
            label = break_label;
            body = m_arr_instr @ [ copy_counter; loop_malloc ];
          }
      in
      (m_arr_ptr, e_instr @ [ block_m ])
  | _ ->
      failwith "Internal Error: Array Init helper needs at least one dimension!"

let rec stmt_gen = function
  | Ast.Return es ->
      let instrs_ls, res_ls = List.map expr_gen es |> List.split in
      List.concat instrs_ls @ [ Return res_ls ]
  | Ast.LocalDecl { name; init; var_type } -> (
      (* Ignore delcaration if it does not have an initializer.
         TODO: Wrong it still can be assigned to
         Then it is not useful *)
      update_locals name var_type;
      match init with
      | None -> []
      | Some e ->
          let instrs, res = expr_gen e in
          (* Symbols.add_var ~f_name name var_type; *)
          instrs
          @ [ W.Copy { src = res; dst = Var name; t = convert_type var_type } ])
  | Ast.ArrayDecl { name; init; dims; a_type } -> (
      update_locals name a_type;
      match init with
      | Some e ->
          let instrs, res = expr_gen e in
          instrs
          @ [ W.Copy { src = res; dst = Var name; t = convert_type a_type } ]
      | None ->
          let elem_t =
            match a_type with
            | Types.Array { elem_type } -> elem_type
            | _ -> failwith "Internal Error: Non Array"
          in
          let filter_dims = List.filter_map Fun.id dims in
          let tmp_ptr, instr = array_init_help elem_t filter_dims in
          instr @ [ W.Copy { src = tmp_ptr; dst = Var name; t = I32 } ])
  | Ast.Assign { lhs; rhs } ->
      let instrs_rhs, rhs_res =
        List.map gen_and_convert_expr rhs |> List.split
      in
      let rhs_ty = List.map Typecheck.typecheck_expr rhs in
      let lhs_ty = List.map Typecheck.typecheck_expr lhs in
      (* TODO with arrays this will be trickier This are not converted *)
      let instrs_lhs, lhs_res = List.map expr_gen lhs |> List.split in
      (* generate a copy for each pair
         TODO This does not work. This can be unbalanced
         right now x,y = add() which returns 2 elements
         Function can return a multi value
         Also rhs needs to be copied into temporaries and then transfered*)
      let dst_rhs, rhs_instrs_copies =
        List.fold_left_map
          (fun acc (rhs_v, rhs_t) ->
            match rhs_t with
            | [ t ] ->
                let dst = make_wtac_var [ t ] in
                (acc @ [ dst ], W.Copy { src = rhs_v; dst; t = convert_type t })
            | _ ->
                failwith
                  "Function call assignments with multiple returns not handled \
                   right now")
          []
          (List.combine rhs_res rhs_ty)
      in
      let copies =
        List.combine (List.combine lhs_res lhs_ty) dst_rhs
        |> List.map (fun ((lhs, lhs_t), rhs) ->
               match lhs_t with
               | [ t ] -> W.Copy { src = rhs; dst = lhs; t = convert_type t }
               | _ ->
                   failwith
                     "Internal Error: Lvalues should only containt one type \
                      value")
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
        | Some (Ast.Int i) -> Wtac.Imm (i, W.I64)
        | Some (Bool true) -> Wtac.Imm (1, W.I32)
        | Some (Bool false) -> Wtac.Imm (0, W.I32)
        | Some _ ->
            failwith
              "Internal error. A non literal is used to initialzie a global"
        | None -> Wtac.Imm (-1, W.I32)
        (* like an undefined*)
      in
      Data { name; t = var_type; value }

let gen (Ast.Program decls) : Wtac.program =
  let length_fun = length_conversion () in
  let wtac_decls = List.map gen_top_level decls in
  Wtac.Program (length_fun::wtac_decls)
