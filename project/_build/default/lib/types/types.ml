type t =
    | TVar of string
    | Bool
    | Str
    | Int
    | List of t
    | Func of t * t
    | Nil
    [@@deriving show];;

type scheme = Forall of string list * t;;

exception TypeError of string

(***********)
(* ENV MAP *)
(***********)
module EnvMap = Map.Make(String);;
(* String : Type *)
type env = t EnvMap.t;;

let extend env k v = EnvMap.add k v env;;
let empty_type_env = EnvMap.empty;;

let lookup env k =
    try EnvMap.find k env 
    with Not_found -> failwith "the variable is not in scope: " ^ k
;;

(* subst = mapping from String (tvar) to type *)

module SubstMap = Map.Make (String);;
(* String : Type *)
type subst_set = t SubstMap.t;;

