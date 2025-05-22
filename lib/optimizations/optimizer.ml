(* Main optimzer *)
open Cfg
open Data_structures

(* See KEEP*.png Our analysis works best if there is only a single entry
     However early returns for example can create cfgs with
   some dead branches. On those dataflow does not really work
   and other analysis wont work as well. So we remove tem first.
   Note that this is different to dead code analysis
   But that would be better to be honest
*)
let reachable_blocks cfg =
  let rec reach_helper b reach =
    let n_reach = IntSetS.add b reach in
    let succs = CFG.succs cfg b in
    List.fold_left (fun acc s -> reach_helper s acc) n_reach succs
  in
  reach_helper (CFG.get_entry cfg) IntSetS.empty

(* Remove dead blocks and all edges *)
let remove_dead_blocks cfg reach_s =
  let remove_edges edges edge_id =
    if not (IntSetS.mem edge_id reach_s) then edges else edge_id :: edges
  in
  let new_blocks =
    List.fold_left
      (fun acc (bl_id, bl) ->
        if not (IntSetS.mem bl_id reach_s) then acc
        else
          (* Check edges *)
          let preds = BLOCK.preds bl in
          let succs = BLOCK.succs bl in
          let new_preds = List.fold_left remove_edges [] preds in
          let new_succs = List.fold_left remove_edges [] succs in
          let new_block =
            BLOCK.make (BLOCK.id bl)
              (BLOCK.lab_instructions bl)
              new_preds new_succs
          in
          acc @ [ (bl_id, new_block) ])
      [] (CFG.blocks cfg)
  in
  (* Fucking annoying *)
  CFG.make new_blocks (CFG.fn_name cfg) (CFG.fn_params cfg) (CFG.fn_type cfg)
    (CFG.fn_locals cfg) (CFG.fn_globals cfg)

let is_loop_header n loops =
  let loop_assoc =
    List.map (fun (l_header, l_body) -> (CFG.block_id l_header, l_body)) loops
  in
  List.assoc_opt n loop_assoc

(* TODO: Postdominator does not really work in my setup *)
(* stop_node_id is the current loop header or the postdominator of ifs
   in the worklist
      Not only that it is also the end for IFS
      So the postdominator*)
let rec of_cfg_helper g loops dom_tree wS labelStack instrs stop_node_id =
  match Stack.is_empty wS with
  | true -> instrs
  | false ->
      let curr_node = Stack.pop wS in
      (* TODO: Should be function in CFG for terminator of a block *)
      (* TODO: This check does not work on entry and exit node since they have no terminator
      Check this first.. *)
      let if_cond, curr_instrs =
        if CFG.get_entry g = curr_node || CFG.get_exit g = curr_node then
          (false, [])
        else
          let curr_block = CFG.get_block g curr_node in
          let curr_instrs = curr_block |> CFG.l_instrs |> List.split |> snd in
          let _, terminator = CFG.l_instrs curr_block |> List.rev |> List.hd in
          let curr_if_label = Stack.top_opt labelStack in
          (* When inside if either early return or check if end of if is found *)

          match curr_if_label with
          | Some lab -> (
              print_endline ("Found if Label: " ^ lab);
              match terminator with
              | Wtac.Br l when l = lab ->
                  print_endline "Found if_end";
                  (true, curr_instrs)
              | Return _ ->
                  print_endline "Found Return";
                  (true, curr_instrs)
              | _ -> (false, []))
          | None -> (false, [])
      in
      if if_cond then (
        (* I need to construct the remaining successors until postdominator is hit *)
        List.iter (fun s -> Stack.push s wS) (CFG.succs g curr_node);
        of_cfg_helper g loops dom_tree wS labelStack (instrs @ curr_instrs)
          stop_node_id)
      else if curr_node <> stop_node_id then
        let curr_block = CFG.get_block g curr_node in
        let curr_instrs = curr_block |> CFG.l_instrs |> List.split |> snd in
        match is_loop_header curr_node loops with
        | Some loop_body ->
            print_endline "Handling Loop";
            let loop_instrs, next_nodes =
              handle_loop g loops dom_tree curr_node loop_body labelStack
            in
            (* This is probably always just one node *)
            List.iter (fun e -> Stack.push e wS) next_nodes;
            of_cfg_helper g loops dom_tree wS labelStack (instrs @ loop_instrs)
              stop_node_id
        | None -> (
            (* TODO: While loop is detected as If *)
            (* Check the number of successors *)
            let curr_succs = CFG.succs g curr_node in
            match curr_succs with
            | [ s ] ->
                (* node -> exit or entry -> node *)
                Stack.push s wS;
                of_cfg_helper g loops dom_tree wS labelStack
                  (instrs @ curr_instrs) stop_node_id
            | [ s1; s2 ] ->
                (* If or Short circuting And and Or *)
                (* Get the nodes that are immediatly dominated by curr_node
                   One of those is the one after the if
                     Becasue our control flow is very structured this should lead
                   correct construction and we do not need the postdominator *)
                let dominated_nodes = IdMap.find curr_node dom_tree in
                (* There should be one node after the If 
                TODO: True but can be end with early returns in them
                Keep a stack for the If nodes with its label
                go through the then and else if that label is encountered,
                  stop and if is finished*)
                let postdom_nodes =
                  List.filter
                    (fun el -> if el = s1 || el = s2 then false else true)
                    dominated_nodes
                in
                if List.length postdom_nodes > 1 then
                  failwith
                    "Internal Error: More Postdominators after if than \
                     expected!"
                else
                  let pdom_node = List.hd postdom_nodes in
                  (* Get if_elseNUM label *)
                  let term_id, terminator =
                    CFG.l_instrs curr_block |> List.rev |> List.hd
                  in
                  let curr_instr_no_term =
                    List.remove_assoc term_id (CFG.l_instrs curr_block)
                    |> List.split |> snd
                  in
                  let if_else_label =
                    match terminator with
                    | Wtac.BrIf { l; _ } -> l
                    | _ ->
                        failwith
                          "Internal Error CFG Reconstruction: \n\
                          \  End of loop did not contain BR instruction"
                  in
                  let num_length = String.length if_else_label - 7 in
                  let loop_num = String.sub if_else_label 7 num_length in
                  let if_end_label = "if_end" ^ loop_num in
                  Stack.push if_end_label labelStack;
                  (* Compute both branches *)
                  let else_stack = Stack.create () in
                  Stack.push s1 else_stack;
                  let then_stack = Stack.create () in
                  Stack.push s2 then_stack;
                  let else_br =
                    of_cfg_helper g loops dom_tree else_stack labelStack []
                      pdom_node
                  in
                  let then_br =
                    of_cfg_helper g loops dom_tree then_stack labelStack []
                      pdom_node
                  in
                  (* The terminator of current block has to be put into the top of the else
                     block because of how blocks are structured *)
                  let else_block =
                    Wtac.Block
                      { label = if_else_label; body = [ terminator ] @ else_br }
                  in
                  let end_block =
                    Wtac.Block
                      { label = if_end_label; body = [ else_block ] @ then_br }
                  in
                  (* Printf.printf "If Else Block: \n %s \n" (Wtac.show_instruction else_block); *)
                  Stack.push pdom_node wS;
                  let _ = Stack.pop labelStack in
                  of_cfg_helper g loops dom_tree wS labelStack
                    (curr_instr_no_term @ [ end_block ])
                    stop_node_id
            | [] ->
                (* Reached the Exit! *)
                (* To make this more clear we call the function again
                   Now the worklist will be emety *)
                of_cfg_helper g loops dom_tree wS labelStack instrs stop_node_id
            | _ -> failwith "Internal Error: Node has more than 2 successors")
        (* We hit start of loop or dominator of if *)
      else instrs

(* Work body of loop until header is found
   Also returns the next nodes that need to be worked after loop*)
and handle_loop g loops dom_tree loop_h loop_body labelStack =
  (* add first block of loop body to a new worklist *)
  let loop_stack = Stack.create () in
  let loop_s = CFG.block_id (List.hd loop_body) in
  let header_instrs =
    loop_h |> CFG.get_block g |> CFG.l_instrs |> List.split |> snd
  in
  let next_nodes = CFG.succs g loop_h |> List.filter (fun el -> el <> loop_s) in

  (* Get labels
     Find last block in loop and extract the loop label *)
  let loop_end = loop_body |> List.rev |> List.hd in
  let terminator = CFG.l_instrs loop_end |> List.rev |> List.hd |> snd in
  let loop_label =
    match terminator with
    | Wtac.Br label -> label
    | _ ->
        failwith
          "Internal Error CFG Reconstruction: \n\
          \  End of loop did not contain BR instruction"
  in
  (* Construct block while_end label *)
  let num_length = String.length loop_label - 10 in
  let loop_num = String.sub loop_label 10 num_length in
  let block_label = "while_end" ^ loop_num in
  Stack.push loop_s loop_stack;
  let body_instrs =
    of_cfg_helper g loops dom_tree loop_stack labelStack [] loop_h
  in
  let loop_bl =
    Wtac.Loop { label = loop_label; body = header_instrs @ body_instrs }
  in
  (* Printf.printf "Body of loop %s: \n" (Wtac.show_instruction loop_bl); *)
  let bl = Wtac.Block { label = block_label; body = [ loop_bl ] } in
  ([ bl ], next_nodes)

let of_cfg (g : CFG.t) : Wtac.top_level =
  let dg = Dominator.analyze g in
  let idom_map = Dominator.immediate_doms dg in
  let dom_tree = Dominator.dominator_tree idom_map in
  let loops = Dominator.compute_natural_loops dg in
  let workStack : int Stack.t = Stack.create () in
  let labelStack : string Stack.t = Stack.create () in
  Stack.push (CFG.get_entry g) workStack;
  let instr_body =
    of_cfg_helper g loops dom_tree workStack labelStack [] (-100)
  in
  let fn =
    Wtac.Function
      {
        name = CFG.fn_name g;
        body = instr_body;
        params = CFG.fn_params g;
        f_type = CFG.fn_type g;
        locals = CFG.fn_locals g;
      }
  in
  fn
(* Recover Function *)

let dce (g : Cfg.CFG.t) =
  let live_g = Liveness.analyze g in
  Printf.printf "Liveness Result:\n";
  Liveness.Graph.pp (Format.formatter_of_out_channel stdout) live_g;
  Liveness.run_dce live_g g

let cp (g : Cfg.CFG.t) = Reaching.run_constant_fold g

let pass _n (g : CFG.t) =
  (* Dead Code elimination removes assignment to globals right now
  Either filter them out fromt he uses and defs and later not use them
  Or seed the liveness analysis with the globals used in that function
  Similarly for function arguments I think
  Function arguments can contain pointers that are written to. So they have a side
  effect. So we cannot remove them
  *)
  (* For stack pointer stuff I need copy propagation
  and constant propagation *)
  dce g |> dce |> dce |> dce |> dce

(* List of all globals
TODO: Restrict to globals used in the function that is optimized *)
let optimize_top_level globals decl =
  match decl with
  | Wtac.Data _ -> failwith "Internal Error: Not optimizing Globals."
  | Wtac.Function { name; _ } as f ->
      Printf.printf "Optimizing %s....\n" name;
      let g = CFG.of_function f globals in
      let cfg_name = Printf.sprintf "cfg_fun_%s.dot" name in
      (* Remove unreachable blocks to make cfg well formed *)
      let reach_bl = reachable_blocks g in
      (* Printf.printf "%s\n" (IntSetS.to_string reach_bl); *)
      let g' = remove_dead_blocks g reach_bl in
      Printf.printf "Created CFG! \n";
      Cfg.CFG.print_cfg cfg_name g';

      (* Early returns are pretty annoying
      If in one branch of an if a return is found
        then the other branch will contain all code until the exit
      which is wrong *)
      let opt_g = pass 2 g' in
      Printf.printf "Converting CFG back to Code....\n";
      let new_cfg = of_cfg opt_g in
      (* List.iter (fun b ->
         Cfg.BLOCK.pp (Format.formatter_of_out_channel stdout) b)
         CFG.blocks cfg *)
      (* Printf.printf "%s" (Wtac.show_top_level new_cfg); *)
      new_cfg

let optimize (Wtac.Program decls) =
  (* get all globals *)
  let globals_data, functions =
    List.partition
      (fun decl ->
        match decl with
        | Wtac.Data _ -> true
        | _ -> false)
      decls
  in
  let globals =
    List.map
      (fun g ->
        match g with
        | Wtac.Data g' -> g'.name
        | _ -> failwith "Will not happen")
      globals_data
  in
  let globals_set = VarSetS.of_list (globals @ [ "__stack_pointer" ]) in
  let opt_funs = List.map (optimize_top_level globals_set) functions in
  Wtac.Program (globals_data @ opt_funs)

let optimize_once g (f : CFG.t -> CFG.t) = f g
