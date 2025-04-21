type wetac_error =
{
  kind: error_kind 
  span: span option 
}
type error_kind = 
| ParseError 
| TypeError 
| WTacError
| InternalError 

exception Wetac_error of wetac_error
(* See https://borretti.me/article/lessons-writing-compiler#test *)