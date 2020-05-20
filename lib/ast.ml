type num = Q.t

let sexp_of_num q = Q.to_string q |> Sexplib0.Sexp_conv.sexp_of_string
let compare_num a b = Q.compare a b

type children =
  { left : expr
  ; right : expr
  }

and expr =
  [ `Add of children
  | `Multiply of children
  | `Number of num
  ]
[@@deriving sexp_of, compare]

let int x = `Number (Q.of_int x)

module BigStep = struct
  let rec eval : expr -> expr = function
    | `Number num -> `Number num
    | `Add { left = `Number left; right = `Number right } -> `Number Q.(left + right)
    | `Add { left; right } -> eval @@ `Add { left = eval left; right = eval right }
    | `Multiply { left = `Number left; right = `Number right } -> `Number Q.(left * right)
    | `Multiply { left; right } ->
      eval @@ `Multiply { left = eval left; right = eval right }
  ;;

  let%test_unit "test eval" =
    [%test_result: expr] ~expect:(int 15)
    @@ eval (`Multiply { left = int 3; right = int 5 });
    [%test_result: expr] ~expect:(int 20)
    @@ eval
         (`Multiply { left = int 2; right = `Multiply { left = int 2; right = int 5 } })
  ;;
end

module SmallStep = struct
  let reducible = function
    | `Number _ -> false
    | _ -> true
  ;;

  let rec reduce' : expr -> expr = function
    | `Number num -> `Number num
    | `Add { left; right } when reducible left -> `Add { left = reduce' left; right }
    | `Add { left; right } when reducible right -> `Add { left; right = reduce' right }
    | `Add _ as expr -> BigStep.eval expr
    | `Multiply { left; right } when reducible left ->
      `Multiply { left = reduce' left; right }
    | `Multiply { left; right } when reducible right ->
      `Multiply { left; right = reduce' right }
    | `Multiply _ as expr -> BigStep.eval expr
  ;;

  let rec reduce expr = if reducible expr then reduce @@ reduce' expr else expr

  let%test_unit "test small step reduce" =
    [%test_result: expr] ~expect:(int 15)
    @@ reduce (`Multiply { left = int 3; right = int 5 });
    [%test_result: expr] ~expect:(int 20)
    @@ reduce
         (`Multiply { left = int 2; right = `Multiply { left = int 2; right = int 5 } })
  ;;
end
