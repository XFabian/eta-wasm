(* Generates debug information for a program
Includes:
Dominance tree
Postdominace Tree
Natural Loops
Cfg tree
These are all available as dot files and their png files
Next, it generates this information in one file
using maps from node to childs

A folder debug_filename is created that includes all these files
*)
let run_command cmd args = 
  if Sys.command (Filename.quote_command cmd args) <> 0 then
    failwith ("Command failed: " ^ cmd)

let perm = 0o755
let create_newdir path perm =
  if not (Sys.file_exists path) then Sys.mkdir path perm
let gen_debug_info filename cfg = 
  
  let file_dir = Filename.dirname filename in
  let f_name = Filename.basename filename |> Filename.chop_extension in
  let curr_dir = Sys.getcwd () in 
  let dirname = file_dir ^ "/" ^"debug_" ^ f_name in 
  let cfg_name = f_name ^ "_cfg.dot" in 
  let dom_name = f_name ^ "_dom.dot" in
  create_newdir dirname perm;
  (* Change into that folder *)
  Sys.chdir dirname;
  Cfg.print_cfg cfg_name cfg;
  Dominator.gen_debug dom_name cfg;
  run_command "dot" ["-Tpng"; cfg_name; "-o"; Filename.chop_extension cfg_name ^ ".png"];
  run_command "dot" ["-Tpng"; dom_name; "-o"; Filename.chop_extension dom_name ^ ".png"];
  (* Todo may add loop info *)
  (* Get back *)
  Sys.chdir curr_dir