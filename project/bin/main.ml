open Token;;

let () =
    (* [LParen; Id "defun"; Id "add"; LParen; Id "x"; Id "y"; RParen; Id "x"; RParen] *)
    let parse_res = [Literal (Int 2); Literal (Bool true)] 
        |> Parser.parse Types.empty_type_env in
    List.iter (fun (x, y) -> 
        print_string (Types.show_scheme y);
        print_newline ();
        print_string (Parser.show_expr x);
        print_newline ()
    ) parse_res
;;

