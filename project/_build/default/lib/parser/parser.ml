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
    
    (* Type environment *)
    env: Types.env;

    (* Current subtree built and about to be propagated to higher nodes *)
    expr: expr; 

    (* The tokens so far left *)
    rest: Token.t list;

    (* Type of the currently parsed expr (node) *)
    (* WARNING: Should this field be a type scheme ? *)
    t: Types.scheme;
} 

(* Parsing expressions beginning with the character `(` *)
let rec parse_lists (program: Token.t list) (env: Types.env): traversal =
    match program with
    | LParen :: tail -> begin
        (* Enters the parsing list "state" *)
        let { expr; rest; env; t; } = parse_list_content tail env in 

        match rest with
        (* Deleting the trailing parenthese *)
        | RParen :: tail -> { expr = expr; rest = tail; t; env; }

        | _ -> failwith "Missing closing parenthesis"
    end
    | _ -> failwith "Unexpected Token, expected LParen." 

and parse_list_content (program: Token.t list) (env: Types.env): traversal =
    match program with

    (* Handling the case of an empty list `()` *)
    (* Here, empty list has type int which is not ok... should introduce nil type *)
    | RParen :: t -> { expr = List []; rest = RParen :: t; t = (* TODO *) Types.Forall ([], Types.Int); env = Types.empty_type_env; } 

    (* Is it a function? *)
    | Id "defun" :: t -> parse_functions t env
    | _ -> begin
        let { expr = atom; rest; t; env; } = parse_atoms program env in
        let { expr; rest = rest'; t = _; env = env'; } = parse_list_content rest env in
        match expr with
        (* If we got a list *)
        (* This env seems ok but I wonder if I don't need to intersect it with the atom one *)
        | List l -> { expr = List (atom :: l); rest = rest'; t; env = env'; }
        | Function _ | Atom _ -> failwith "Expected to find a list"
    end

and parse_functions (program: Token.t list) (env: Types.env): traversal = 
    match program with
    | Id i :: t -> 
            let (args, rest) = parse_args_list t in
            let { expr = body; rest = rest'; t = _; env = env'; } = parse_atoms rest env in 

            (* Not sure about that, the env is not resolved and intersected *)
            { expr = Function (i, args, body);
              rest = rest';
              (* WARNING: Dummy values *)
              t = Types.Forall ([], Types.Func ([Types.Int], Types.Bool));
              env = env';
            } 

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
(* WARNING: Shouldn't we add context and typing??? *)
and parse_arg (program: Token.t list): string * Token.t list =
    match program with
    | Id i :: t -> (i, t)
    | _ -> failwith "Unexpected token, expected an identifier"

(* OK *)
and parse_atoms (program: Token.t list) (env: Types.env): traversal = 
    match program with
    | LParen :: _ -> parse_lists program env
    | Id i :: t -> begin

        (* Will throw an error if i is not defined (has no type) *)
        let scheme = Types.lookup env i in
        { expr = Atom (Id i); 
          rest = t; 
          env; 
          t = scheme
        }
    end
    | Literal l :: tail -> begin
        match (l: Token.literal) with
        | Int i -> { expr = Atom (Int i); 
                     rest = tail; 
                     env = env; 
                     t = Forall ([], Types.Int); 
                   }
        | Str s -> { expr = Atom (Str s); 
                     rest = tail; 
                     env = env; 
                     t = Forall ([], Types.Str); 
                   }
        | Bool b -> { expr = Atom (Bool b); 
                      rest = tail; 
                      env = env; 
                      t = Forall ([], Types.Bool); 
                    }
    end
    | _ -> failwith "Orphan closing parenthese `RPAREN`"
;;

let rec parse (ctx: Types.env) (program: Token.t list) =
    let { expr; rest; t; env; } = parse_atoms program ctx in
    match rest with
    | [] -> [(expr, t)]
    | l -> (expr, t) :: (parse env l)
;;

