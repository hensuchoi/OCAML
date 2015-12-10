exception MLFailure of string

type binop = 
      Plus 
    | Minus 
    | Mul 
    | Div 
    | Eq 
    | Ne 
    | Lt 
    | Le 
    | And 
    | Or          
    | Cons

type expr =   
      Const of int 
    | True   
    | False      
    | NilExpr
    | Var of string    
    | Bin of expr * binop * expr 
    | If  of expr * expr * expr
    | Let of string * expr * expr 
    | App of expr * expr 
    | Fun of string * expr    
    | Letrec of string * expr * expr

type value =  
      Int of int		
    | Bool of bool          
    | Closure of env * string option * string * expr 
    | Nil                    
    | Pair of value * value     

and env = (string * value) list

let binopToString op = 
  match op with
      Plus -> "+" 
    | Minus -> "-" 
    | Mul -> "*" 
    | Div -> "/"
    | Eq -> "="
    | Ne -> "!="
    | Lt -> "<"
    | Le -> "<="
    | And -> "&&"
    | Or -> "||"
    | Cons -> "::"

let rec valueToString v = 
  match v with 
      Int i -> 
        Printf.sprintf "%d" i
    | Bool b -> 
        Printf.sprintf "%b" b
    | Closure (evn,fo,x,e) -> 
        let fs = match fo with None -> "Anon" | Some fs -> fs in
          Printf.sprintf "{%s,%s,%s,%s}" (envToString evn) fs x (exprToString e)
    | Pair (v1,v2) -> 
        Printf.sprintf "(%s::%s)" (valueToString v1) (valueToString v2) 
    | Nil -> 
        "[]"

and envToString evn =
  let xs = List.map (fun (x,v) -> Printf.sprintf "%s:%s" x (valueToString v)) evn in
    "["^(String.concat ";" xs)^"]"

and exprToString e =
  match e with
      Const i ->
        Printf.sprintf "%d" i
    | True -> 
        "true" 
    | False -> 
        "false"
    | Var x -> 
        x
    | Bin (e1,op,e2) -> 
        Printf.sprintf "%s %s %s" 
          (exprToString e1) (binopToString op) (exprToString e2)
    | If (e1,e2,e3) -> 
        Printf.sprintf "if %s then %s else %s" 
          (exprToString e1) (exprToString e2) (exprToString e3)
    | Let (x,e1,e2) -> 
        Printf.sprintf "let %s = %s in \n %s" 
          x (exprToString e1) (exprToString e2) 
    | App (e1,e2) -> 
        Printf.sprintf "(%s %s)" (exprToString e1) (exprToString e2)
    | Fun (x,e) -> 
        Printf.sprintf "fun %s -> %s" x (exprToString e) 
    | Letrec (x,e1,e2) -> 
        Printf.sprintf "let rec %s = %s in \n %s" 
          x (exprToString e1) (exprToString e2) 
    |NilExpr -> failwith "default"


(*********************** Some helpers you might need ***********************)

let rec fold f base args = 
  match args with [] -> base
                | h::t -> fold f (f(base,h)) t

let listAssoc (k,l) = 
  fold (fun (r,(t,v)) -> if r = None && k=t then Some v else r) None l

(*********************** Your code starts here ****************************)



let lookup (x,evn) = match listAssoc (x,evn) with
    Some x -> x
  |_ -> raise (MLFailure("variable not bound: " ^ x ));;



let rec eval (evn,e) = 
  match e with
    |Var v -> lookup(v,evn)
    |Const c-> Int c
    |NilExpr -> Nil
    |True -> Bool true
    |False -> Bool false
    |Bin(e1,op,e2) -> 
        let v1= eval (evn, e1) in
        let v2 = eval (evn,e2) in 
          (match (v1,op,v2) with
              (Int i1,Plus,Int i2) -> let plusR = Int (i1+i2) in plusR
            |(Int i1,Minus,Int i2) -> let minusR = Int (i1 - i2) in minusR
            |(Int i1,Mul,Int i2)   -> let timesR = Int (i1*i2) in timesR
            |(Int i1,Div,Int i2)   -> let divR = Int (i1/i2) in divR
            |(Int i1,Eq,Int i2)    ->  if (i1=i2) = true then Bool true else Bool false 
            |(Bool i1,Eq,Bool i2)    -> if (i1=i2) = true then Bool true else Bool false
            |(Int i1,Ne,Int i2)    -> if (i1=i2) = true then Bool false else Bool true
            |(Bool i1,Ne,Bool i2)    -> if (i1=i2) = true then Bool false else Bool true
            |(Int i1,Lt,Int i2)    -> if i1<i2 = true then Bool true else Bool false 
            |(Int i1,Le,Int i2)    -> if i1<=i2 = true then Bool true else Bool false
            |(Bool i1,And,Bool i2)   -> let andR = Bool(i1&&i2) in andR
            |(Bool i1,Or,Bool i2)    ->  let orR = Bool(i1||i2) in orR
            |_-> raise(MLFailure ("Error, invalid type"))
          )


    |If (exp, if_true,if_false) ->
        (match eval (evn, exp) with
          |Bool x -> if x = true
              then let tr = eval (evn,if_true)
                in tr
              else let fls = eval (evn,if_false)
                in fls
          |_ -> failwith "Error, invalid type")

    |Let (exp, exp1, exp2) -> let v1 = eval (evn,exp1) in
        let new_evn = (exp,v1)::evn
        in let funlet = eval (new_evn,exp2)
        in funlet


    | App (e1,e2) -> 
        (match eval (evn,e1) with
          | Closure (evn2,n,x,e) -> (
              match n with
                | None -> eval (((x, eval (evn,e2))::evn2),e)
                | Some i -> eval (((i, Closure(evn,n,x,e))::((x,eval (evn,e2))::evn)),e))

          | _ -> eval (evn,e1))


    | Fun (x,exp1) -> let funct =
                        Closure (evn, None, x, exp1)
        in funct

    | Letrec (a,e1,e2) -> 
        let evn1= (
          match eval(evn,e1) with
            | Closure (x1,None,x,e) -> Closure (x1, Some a,x,e)
            |_-> eval (evn,e1))
        in
        let evn' = (a,evn1)::evn in eval (evn',e2);;









