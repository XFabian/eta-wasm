open Sexplib.Std

type t =
  | Int
  | Bool
  (* Used for _ and {} so where the type depends on the context *)
  | Unknown
  | Array of { elem_type : t }
  | FunType of { param_types : t list; ret_type : t list }
  | Record of string
[@@deriving show, sexp]

let comma_sep out () = Format.fprintf out ", "

let rec pp (fmt : Format.formatter) = function
  | Int -> Format.fprintf fmt "int"
  | Bool -> Format.fprintf fmt "bool"
  | Unknown -> Format.fprintf fmt "unknown"
  | Array { elem_type } -> Format.fprintf fmt "%a[]" pp elem_type
  | Record s -> Format.print_string s
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
      | [] -> Format.fprintf fmt "%s -> ()" list_fmt
      | ts ->
          Format.fprintf fmt "%s -> %a" list_fmt
            (Format.pp_print_list ~pp_sep:comma_sep pp)
            ts)

let is_arr = function
  | Array _ -> true
  | _ -> false

let get_size = function
  | Int -> 8
  | Bool -> 4
  (* Arrays are stored as a pointer in the heap *)
  | Array _ -> 4
  (* Records are stored on stack or heap as pointer to first element *)
  | Record _ -> 4
  | FunType _
  | Unknown ->
      failwith "Internal Error: Function Types and Unknown do not have a size"
