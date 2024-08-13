type literal = 
    | Str of string
    | Int of int
    | Bool of bool
    [@@deriving show];;

type t =
    | Let
    | Lambda
    | Dot
    | Assign
    | In
    | Id of string
    | Literal of literal
    [@@deriving show];;

