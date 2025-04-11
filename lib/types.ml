open Sexplib.Std

type t =
  | Int
  | Bool
  (* Used for _ and {} so where the type depends on the context *)
  | Unknown
  | Array of { elem_type : t }
  | FunType of { param_types : t list; ret_type : t list option }
[@@deriving show, sexp]

let comma_sep out () = Format.fprintf out ", "

let rec pp (fmt : Format.formatter) = function
  | Int -> Format.fprintf fmt "int"
  | Bool -> Format.fprintf fmt "bool"
  | Unknown -> Format.fprintf fmt "unknown"
  | Array { elem_type } -> Format.fprintf fmt "%a[]" pp elem_type
  | FunType { param_types; ret_type } -> (
      let list_fmt =
        match param_types with
        | [] -> Format.sprintf "()"
        | pts ->
            Format.asprintf "(%a)"
              (Format.pp_print_list ~pp_sep:comma_sep pp)
              pts
      in
      (* Ugly but works for now *)
      match ret_type with
      | None -> Format.fprintf fmt "%s -> ()" list_fmt
      | Some ts ->
          Format.fprintf fmt "%s -> %a" list_fmt
            (Format.pp_print_list ~pp_sep:comma_sep pp)
            ts)
