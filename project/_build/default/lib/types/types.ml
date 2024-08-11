type t =
    | TVar of string
    (* Primitives *)
    | Bool
    | Str
    | Int
    (* Builtins *)
    | List of t
    | Func of t list * t
    [@@deriving show];;

(* Here the list shows the bound variables of the expr *)
type scheme = Forall of string list * t
[@@deriving show]

exception TypeError of string

(* Impossible to pretty-print? *)
module TypeMap = Map.Make(String)

(* String : Type *)
type env = scheme TypeMap.t

let empty_type_env = TypeMap.empty;;

let extend env k v = TypeMap.add k v env;;

let lookup (env: env) (k: string) =
    try TypeMap.find k env 
    with Not_found -> failwith ("the variable is not in scope: " ^ k)
;;

(* String : Type *)
type subst_set = t TypeMap.t;;
let empty_subst_set = TypeMap.empty;;

let rec substitute s t =
    match t with
    | TVar v -> begin 
        match TypeMap.find_opt v s with
        | Some x -> x
        | None -> TVar v
    end
    | Func (args, ret) -> Func (List.map (substitute s) args, substitute s ret)
    | t -> t
;;

let compose_subst s1 s2 =
    TypeMap.union (fun _ t _ -> Some t) (TypeMap.map (substitute s1) s2) s1
;;

let rec unify t1 t2 =
    match (t1, t2) with
    | (Func (args1, ret1), Func (args2, ret2)) when List.length args1 = List.length args2 ->
        let s1 = List.fold_left2 (fun s arg1 arg2 -> 
            compose_subst s (unify (substitute s arg1) (substitute s arg2))
        ) empty_subst_set args1 args2 in
        let s2 = unify (substitute s1 ret1) (substitute s1 ret2) in
        compose_subst s2 s1
    | TVar v, t | t, TVar v -> v |-> t
    | Int, Int | Bool, Bool | Str, Str -> empty_subst_set
    | _ -> raise (TypeError "Cannot unify types")

and (|->) v t =
    (* Binding operation from a type variable to a type (or type variable) *)
    if t = TVar v then empty_subst_set
    else if occurs_check v t then raise (TypeError "Occurs check failed")
    else TypeMap.singleton v t

and occurs_check v t =
    (* Check if the type variable is used anywhere in a function type to prevent infinite function type *)
    match t with
    | TVar u -> u = v
    | Func (args, ret) -> List.exists (occurs_check v) args || occurs_check v ret
    | _ -> false
;;

(* Instantiation is like the final substitution to replace all monotypes type variables *)

let rec ftv_ty = function
  | TVar n -> [n]
  | Int | Str | Bool | List _ -> []
  | Func (args, ret) -> List.flatten (List.map ftv_ty args) @ ftv_ty(ret)

let ftv_scheme (Forall (vars, t)) =
  List.filter (fun x -> not (List.mem x vars)) (ftv_ty t)

let ftv_env env =
  TypeMap.fold (fun _ s acc -> List.append (ftv_scheme s) acc) env []

let generalize env t =
  let vars = List.filter (fun v -> not (List.mem v (ftv_env env))) (ftv_ty t) in
  Forall (vars, t)

let fresh_tyvar =
  let counter = ref 0 in
  fun () -> incr counter; TVar ("t" ^ string_of_int !counter)

(* "Given the context, this expression should be narrowed into:" -> polytype to monotype *)
let instantiate (Forall (vars, t)) =
  let subst = List.fold_left (fun s v -> TypeMap.add v (fresh_tyvar ()) s) TypeMap.empty vars in
  substitute subst t
