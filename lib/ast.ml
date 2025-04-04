type bin_op = | Add | Sub | Mult[@@ deriving show]

type expr = | Int of int | Binary of bin_op * expr * expr [@@deriving show]