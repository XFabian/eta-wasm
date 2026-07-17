open Wetac
open Cmdliner



(* TODO: I would like to have a small repl as well *)
let driver stage src_f =
  let _ = Compile.compile stage src_f in
  exit 0





let src_file =
  Arg.(required & pos 0 (some non_dir_file) None & info [] ~docv:"file")

let stage = 
  let parse =
    let doc = "Run the Parser" in
    (Setting.Parse, Arg.info ["parse"] ~doc)
  in
  let validate  =
    let doc = "Run the Semantic Analysis" in
    (Setting.Validate, Arg.info ["validate"] ~doc)
  in
  let wtac =
      let doc = "Run the WTac Generation" in
      (Setting.Wtac, Arg.info ["wtac"] ~doc)
  in
  let optimize =
    let doc = "Run the Optimizer and dump the optimized WTac" in
    (Setting.Optimize, Arg.info ["optimize"] ~doc)
  in
  let codegen =
    let doc = "Run the Code Generation to WASM" in
    (Setting.Codegen, Arg.info ["codegen"] ~doc)
in
  Arg.(value & Arg.vflag Setting.Codegen [parse; validate; wtac; optimize; codegen])


(* $ applies evaluation of args onto evaluation of function driver
   So it provides the arguments to driver (a -> b)t -> a t -> b t is the type
   You can see the monaidc structure. That is bind *)
let cmd =
  let doc = "WEtac : A Eta compiler to WASM" in
  let info = Cmd.info "wetac" ~doc in
  Cmd.v info Term.(const driver $ stage $ src_file)

let main () = Stdlib.exit (Cmd.eval cmd)
let () = main ()

  

