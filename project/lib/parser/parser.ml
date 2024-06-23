open Token;;

type literal =
    | Str of string
    | Id of string
    | Int of int
    | Bool of bool
[@@deriving show]

type expr =
    | Function of string * string list * expr
    | List of expr list
    | Atom of literal
[@@deriving show]

type ast = expr list
[@@deriving show]

(* Return type of most of the functions making up the parser *)
type traversal = {
    env: Types.env;
    expr: expr;
    rest: Token.t list;
    scheme: Types.scheme;
}

(* Parsing expressions beginning with the character `(` *)
let rec parse_lists (program: Token.t list): expr * Token.t list =
    match program with
    | LParen :: t -> begin
        (* Enters the parsing list "state" *)
        let (expr, rest) = parse_list_content t in 
        match rest with
        (* Deleting the trailing parenthese *)
        | RParen :: t -> (expr, t)
        | _ -> failwith "Missing closing parenthesis"
    end
    | _ -> failwith "Unexpected Token, expected LParen." 

and parse_list_content (program: Token.t list): expr * Token.t list =
    match program with
    (* Handling the case of an empty list `()` *)
    | RParen :: t -> (List [], RParen :: t)
    (* Is it a function? *)
    | Id "defun" :: t -> parse_functions t
    | _ -> begin
        let (atom, rest) = parse_atoms program in
        let (expr, rest') = parse_list_content rest in
        match expr with
        (* If we got a list *)
        | List l -> (List (atom :: l), rest')
        | Function _ | Atom _ -> failwith "Expected to find a list"
    end

and parse_functions (program: Token.t list): expr * Token.t list = 
    match program with
    | Id i :: t -> 
            let (args, rest) = parse_args_list t in
            let (body, rest') = parse_atoms rest in 
            (Function (i, args, body), rest')
    | _ -> failwith "Expected to find an ID as function identifier."

and parse_args_list (program: Token.t list): string list * Token.t list = 
    match program with
    | LParen :: t -> begin
            let (args, rest) = parse_arg_list_content t in
            match rest with
            (* Delete the trailing parenthesis *)
            | RParen :: t -> args, t
            | _ -> failwith "Expected a closing parenthesis"
    end
    | _ -> failwith "Expected a list of arguments"

and parse_arg_list_content (program: Token.t list): string list * Token.t list = 
    match program with
    (* No arguments *)
    | RParen :: t -> ([], RParen :: t)
    | _ ->
        let (arg, rest) = parse_arg program in
        let (arg_tail, rest') = parse_arg_list_content rest in
        arg::arg_tail, rest'

(* Parse a single function argument *)
and parse_arg (program: Token.t list): string * Token.t list =
    match program with
    | Id i :: t -> (i, t)
    | _ -> failwith "Unexpected token, expected an identifier"

and parse_atoms (program: Token.t list): expr * Token.t list = 
    match program with
    | LParen :: _ -> parse_lists program
    | Id i :: t -> (Atom (Id i), t)
    | Literal l :: t -> begin
        match l with
        | Int i -> (Atom (Int i), t)
        | Str s -> (Atom (Str s), t)
        | Bool b -> (Atom (Bool b), t)
    end
    | _ -> failwith "Orphan closing parenthese `RPAREN`"
;;

let rec parse (program: Token.t list): ast =
    let (expr, rest) = parse_atoms program in
    match rest with
    | [] -> [expr]
    | l -> expr :: (parse l)
;;

