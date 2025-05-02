(* This is a hack because of dependency cycles between
Cfg -> Dataflow -> Dominator
The clean solution would be to defe a CFG interface
I started with this
But I think stuff inside Cfg is still leaking.
We should only expose Cfg but not the block

So add_edge and all that stuff should live in Cfg module
and all other sutff as well for construction
*)

let test cfg =
  let g =Dominator.init cfg in 
  g