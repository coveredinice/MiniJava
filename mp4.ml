open Ast
open List


(*************************************************************************************************)

(* Stack: contains variables, pointers to objects and arrays *)

type val_on_stack = IntVal of int | FloatVal of float | StrVal of string | BoolVal of bool 
| NullVal | PointerObject of int * exp_type | PointerArray of int * exp_type

type stack = (string * val_on_stack * exp_type) list

(* Heap: contains objects and arrays *)

type val_on_heap = Object of string * stack | Array of exp_type * ((string * val_on_stack) list) 

type heap = val_on_heap list

(************************************************************************************************)

(* Binary operators (without And, Or) *)

let bin_operations (val1:val_on_stack) op (val2:val_on_stack) : val_on_stack = 
   match val1, op, val2 with
   (* Plus, Mult, Div, Minus between integers *)
   | IntVal x, Plus, IntVal y -> IntVal (x + y)
   | IntVal x,  Minus, IntVal y  -> IntVal (x - y)
   | IntVal x,  Mult, IntVal y ->  IntVal(x * y)
   | IntVal x,  Div, IntVal y -> if y=0 then failwith("Div on Int: division by zero.") else IntVal (x / y)
   (* Plus, Mult, Div, Minus between floats*)
   | FloatVal x,  Plus, FloatVal y -> FloatVal (x +.  y)
   | FloatVal x,  Minus, FloatVal y  -> FloatVal (x -. y)
   | FloatVal x,  Mult, FloatVal y ->  FloatVal (x *. y)
   | FloatVal x,  Div, FloatVal y ->  if y=0.0 then failwith("Div on Float: division by zero.") else FloatVal (x /. y) 
   (*LessThan between two integers, two floats and mixed-type values *)
   | IntVal x, LessThan, IntVal y -> BoolVal (x < y)
   | FloatVal x, LessThan, FloatVal y -> BoolVal (x < y)
   | FloatVal x, LessThan, IntVal y -> BoolVal (x < float_of_int(y))
   | IntVal x, LessThan, FloatVal y -> BoolVal (float_of_int(x) < y)
   (* Mixed cases with floats and integers *)
   | IntVal x, Plus, FloatVal y ->  FloatVal (float_of_int(x) +. y)
   | IntVal x, Minus, FloatVal y ->  FloatVal (float_of_int(x) -. y)
   | IntVal x, Mult, FloatVal y -> FloatVal (float_of_int(x) *. y)
   | IntVal x, Div, FloatVal y -> if y=0.0 then failwith("Div on Float: division by zero.") else FloatVal (float_of_int(x) /. y)
   | FloatVal x, Plus, IntVal y -> FloatVal (x +. float_of_int(y))
   | FloatVal x, Minus, IntVal y -> FloatVal (x -. float_of_int(y))
   | FloatVal x, Mult, IntVal y -> FloatVal (x *. float_of_int(y))
   | FloatVal x, Div, IntVal y -> if y=0 then failwith("Div on Float: division by zero.") else FloatVal (x /. float_of_int(y))
   | _ -> failwith("[bin_operation] This is not a valid binary operation")
;;   

(************************************************************************************************)

(* Id 
   auxiliary functions (but might be needed elsewhere) *)

let rec id_is_in_stack (theStack:stack) (id:id) : bool = 
  match theStack with
  | [] ->  false 
  | (name,value,exptype)::xs -> name=id || id_is_in_stack xs id 
;;

let rec get_id_from_stack (theStack:stack) (id:id) : val_on_stack =
  match theStack with
  | [] -> failwith ("[get_id_from_stack] id " ^ id ^ " is unbound")
  | (name,value,exptype)::xs -> if name=id then value else get_id_from_stack xs id
;;

let get_id_from_fields (theHeap:heap) (pos:int) (id:id): val_on_stack =
  let resulting_object = List.nth theHeap pos in 
    match resulting_object with
     | Object(class_name,private_stack) -> get_id_from_stack private_stack id
     | _ -> failwith("[get_id_from_fields] This is not an object but an array")
;;

(************************************************************************************************)

(* NewId
   auxiliary functions (but might be needed elsewhere) *)

let rec scan_classes (list_of_classes: class_decl list) (targetClass:id) : class_decl = 
  match list_of_classes with
    | [] -> failwith("[get_class] Class " ^ targetClass ^ " cannot be found")
    | (Class(className, _, _, _) as firstclass)::rest -> if targetClass=className then firstclass else scan_classes rest targetClass
;;

let get_class (targetClass:id) (Program lst_classes) : class_decl =
  scan_classes lst_classes targetClass
;;

let rec cancel_Var theFields : (exp_type * string) list = 
  match theFields with
    | [] -> []
    | Var(exptype,id)::xs -> (exptype,id) :: cancel_Var xs
;;

let rec dump_all_fields (theClass:id) (prog:program) : (exp_type * string) list =
  let (Class(name,extends,fields,methods)) = get_class theClass prog in
  (match extends with
    | "Object" -> cancel_Var fields 
    | _ -> cancel_Var fields @ dump_all_fields extends prog)
;;

let rec add_null (obtained_fields: (exp_type * string) list) : (string * val_on_stack * exp_type) list =
  match obtained_fields with
    | [] -> []
    | (exptype,id)::xs -> (id,NullVal,exptype) :: add_null xs
;; 

(************************************************************************************************)

(* NewArray
   auxiliary functions (but might be needed elsewhere) *)

let return_length_as_int (theLength:val_on_stack) : int = 
   match theLength with
    | IntVal l when l > 0 -> l 
    | _ -> failwith("[return_length_as_int] The length must be a positive integer")
;;

let rec create_sequential_list (length:int) (count:int) : string list = 
   match length with
   | 0 -> []
   | x when x > 0 -> [string_of_int(count)] @ create_sequential_list (length-1) (count+1)
   | _ -> failwith("[create_sequential_list] Length must be positive")
;;

let create_list (length:val_on_stack) (count:int) : string list = 
   let intlength = return_length_as_int length in create_sequential_list intlength count
;;

let assign_null (id:id) : id * val_on_stack =
   (id, NullVal)
;;

let map_null (length:val_on_stack) (count:int) : (id * val_on_stack) list = 
   let s = create_list length count in map assign_null s
;;

(************************************************************************************************)

(* Methods dei metodi:
   auxiliary functions (but might be needed elsewhere) *)


let rec method_exists (id:id) (list_of_methods: meth_decl list) : bool =
   if list_of_methods = [] then false
   else (match list_of_methods with
      | Method(_,name,_,_,_,_)::xs -> id=name || method_exists id xs
      | _ -> failwith("[method_exists] This is not a correct list of methods"))
;;

let rec get_method_from_list (id:id) (list_of_methods:meth_decl list) : meth_decl = 
   match list_of_methods with
   | [] -> failwith("[get_method_from_list] The method " ^ id ^ " does not exist")
   | (Method(_, name, _, _, _, _) as theMethod)::xs ->  if id=name then theMethod 
                                                        else get_method_from_list id xs
;;

let get_method_from_class (id:id) (Class(_, _, _, list_of_methods)) : meth_decl =
   get_method_from_list id list_of_methods
;;

let rec get_method (id:id) (theClass:id) (prog:program) : meth_decl =
   let resulting_class = get_class theClass prog in
      (match resulting_class with
        | Class(_,name,_,list_of_methods) ->  if method_exists id list_of_methods 
                                              then get_method_from_class id resulting_class
                                              else (if name <> "Object" then get_method id name prog
                                                    else failwith("[get_method] The method " ^ id ^ " does not exist"))) 
;;

(***********************************************************************************************)

(* Type checking *)

 let rec dump_all_classes (theClass:id) (prog:program) : exp_type list =
  let (Class(name,extends,fields,methods)) = get_class theClass prog in
  (match extends with
    | "Object" -> [ObjectType(name)]
    | _ -> ObjectType(name) :: dump_all_classes extends prog)
;;

let rec is_in_list (theClass:id) (lst:exp_type list) : bool = 
  match lst with
   | [] -> false
   | (ObjectType(x))::rest -> theClass=x || is_in_list theClass rest
   | _ -> failwith("[is_in_list] Probably not working with an object")
;;

let check_inheritance (theClass) (good_list:exp_type list) : bool =
  is_in_list theClass good_list 
;; 

let rec type_control (vos:val_on_stack) (exptype:exp_type) (prog:program) : bool = 
  match vos, exptype with
    | IntVal x, IntType -> true
    | StrVal x, StringType -> true
    | BoolVal x, BoolType -> true
    | FloatVal x, FloatType -> true
    | PointerArray (x,zz), next -> (match next with
                                    | IntType -> zz=IntType
                                    | FloatType -> zz=FloatType
                                    | BoolType -> zz=BoolType
                                    | StringType -> zz=StringType 
                                    | ObjectType(p) -> false 
                                    | ArrayType(yy) -> zz=yy)
    | PointerObject (x,ObjectType(z)), ObjectType(y) -> let good_list = dump_all_classes y prog in check_inheritance z good_list
    | _ -> false
;;

(* Assignments and replacements *)

let rec assign_to_variable (id:id) (v:val_on_stack) (theStack:stack) (prog:program) : stack =
  match theStack with
    | [] -> failwith("[assign_to_variable] The variable " ^ id ^ " is unbound")
    | (h_id,h_vos,h_exptype)::rest -> if h_id<>id 
                                    then (h_id,h_vos,h_exptype):: assign_to_variable id v rest prog
                                    else (if type_control v h_exptype prog 
                                          then (id,v,h_exptype)::rest
                                          else failwith("[assign_to_variable] The types are not the same")) 
;;

let assign_to_fields (theObject:val_on_heap) (id:string) (vos:val_on_stack) (prog:program) : val_on_heap =
  match theObject with
    | Object(theClass,theFields) -> Object(theClass, assign_to_variable id vos theFields prog)
    | _ -> failwith("[assign_to_fields] This is not an object")
;;

let rec replace_in_list (i:int) replaced lst = 
  match i,lst with
  | v, [] -> failwith("[replace_in_list] The list is empty, nothing to replace")
  | 0, x::xs -> replaced::xs
  | v, x::xs -> x :: replace_in_list (v-1) replaced xs
;;

(*  Combine the arguments and add NullVal to locals. Remember: 

    - args: (exp_type * string) list
    - obtained_args: val_on_stack list
    - locals: Var(exp_type * string) list
*)

let rec combine_args (args:(exp_type * string) list) (obtained_args:val_on_stack list) : stack = 
  match args with
    | [] -> []
    | (exptype,id)::rest -> (id,(hd obtained_args),exptype) :: combine_args rest (tl obtained_args)
;; 
  
let rec add_null_locals (locals: var_decl list) : stack =
  match locals with
    | [] -> []
    | Var(exptype,id)::rest -> (id,NullVal,exptype) :: add_null_locals rest
;;  
 
let create_new_stack (this:val_on_stack) (obtained_args: val_on_stack list) (args:(exp_type * string) list) locals theClass : (string * val_on_stack * exp_type) list = 
  let first = combine_args args obtained_args and second = add_null_locals locals  
    in ("this", this, ObjectType(theClass)) :: (first @ second)
;;

(* Replacing lists in arrays *)

let replace_array_list (theArray:val_on_heap) (newlist: (string * val_on_stack) list) =
  match theArray with
    | Array(theType,oldlist) -> Array(theType,newlist)
    | _ -> failwith("[replace_array_list] This is not a valid array")
;;

(***********************************************************************************************)

(* Conversions between different types *)

let int_to_valonstack (v:int) : val_on_stack = 
  match v with
  | p -> IntVal p
;;

let valonstack_to_int (v:val_on_stack) : int = 
  match v with
  | IntVal p -> p
  |_ -> failwith("[valonstack_to_int] Cannot convert something not IntVal")
;;


(************************************************************************************************)

 (* Big evaluation method *)

let rec eval (theExpr:exp) (theStack,theHeap) (prog:program) : val_on_stack * heap = 
   match theExpr with
   (* Integer literal, float literal, string literal, null, booleani *)
   | Integer v ->  (IntVal v, theHeap)
   | Float v ->  (FloatVal v, theHeap)         
   | String v ->  (StrVal v, theHeap)
   | Null ->  (NullVal, theHeap)
   | True -> (BoolVal true, theHeap)
   | False -> (BoolVal false, theHeap)
   (* Variable access *)
   | This -> (get_id_from_stack theStack "this", theHeap)
   (* Id *)
   | Id v ->  eval_id v (theStack,theHeap) prog
   (* Not *)
   | Not v -> eval_not v (theStack,theHeap) prog
   (* And, Or *) 
   | Operation(e1, And, e2) -> eval_bool e1 And e2 (theStack,theHeap) prog
   | Operation(e1, Or, e2) -> eval_bool e1 Or e2 (theStack,theHeap) prog
   (* Operazioni binarie *)
   | Operation(e1, op, e2) -> eval_binary e1 op e2 (theStack,theHeap) prog
   (*NewId*)
   | NewId c -> eval_newid c (theStack,theHeap) prog
   (*NewArray*)
   | NewArray (ty,len) -> eval_newarray (ty,len) (theStack,theHeap) prog 
   (* Length *)
   | Length e -> eval_length e (theStack,theHeap) prog
   (* Array *)
   | Array(e1,e2) -> eval_array e1 e2 (theStack,theHeap) prog
   (* Metodi *)
   | MethCall(e, id, args) ->  eval_method e id args (theStack,theHeap) prog 

(* Id *)

and eval_id (id:id) (theStack,theHeap) (prog:program): val_on_stack * heap =
   if id_is_in_stack theStack id 
   then let obtained_id = get_id_from_stack theStack id in (obtained_id, theHeap)
   else let obtained_this = get_id_from_stack theStack "this" in
      (match obtained_this with
         | PointerObject (pointer,_) -> let obtained_fields = get_id_from_fields theHeap pointer id in (obtained_fields, theHeap)
         | _ -> failwith("[eval_id] variable " ^ id ^ " is not defined"))
                         
(* Not, And e Or *)

and eval_not (e:exp) (theStack,theHeap) (prog:program) : val_on_stack * heap = 
   let (result_value, theHeap1) = eval e (theStack,theHeap) prog in
   (match result_value with
      | BoolVal v -> (BoolVal (not v), theHeap1)
      | _ -> failwith("[eval_not] Not cannot be applied to a non-boolean"))

and eval_bool (e1:exp) op (e2:exp) (theStack,theHeap) (prog:program): val_on_stack * heap = 
  let (result_e1,theHeap1) = eval e1 (theStack,theHeap) prog in
  (let (result_e2,theHeap2) = eval e2 (theStack,theHeap1) prog in
      (match op with
          | And -> (match result_e1 with
                    | BoolVal true -> (result_e2, theHeap2)
                    | BoolVal false -> (BoolVal false, theHeap1)
                    | _ -> failwith("[eval_bool] And cannot be applied to a non-boolean"))
          | Or ->  (match result_e1 with
                    | BoolVal false -> (result_e2, theHeap2)
                    | BoolVal true -> (BoolVal true, theHeap1)
                    | _ -> failwith("[eval_bool] Or cannot be applied to a non-boolean."))
          | _ -> failwith("[eval_bool] This boolean operator is not supported")))  

(* Binary operations *)

and eval_binary (e1:exp) op (e2:exp) (theStack,theHeap) (prog:program) : val_on_stack * heap =
  let (result_e1,theHeap1) = eval e1 (theStack,theHeap) prog in
    let (result_e2,theHeap2) = eval e2 (theStack,theHeap1) prog in
      (bin_operations result_e1 op result_e2, theHeap2)  

(* NewId *)

and eval_newid (theClass:id) (theStack,theHeap) (prog:program) : val_on_stack * heap =
  let obtained_fields = dump_all_fields theClass prog (*obtained_fields: (exp_type * id) list *)
  and new_pos = List.length theHeap
  in (let new_object = Object(theClass, add_null obtained_fields) in
        (PointerObject (new_pos, ObjectType(theClass)), theHeap @ [new_object]))

(* NewArray *)  

and eval_newarray (theType,theLength) (theStack,theHeap) (prog:program) : val_on_stack * heap =
  let new_pos = List.length theHeap 
  and (l,theHeap) = eval theLength (theStack,theHeap) prog in 
    match l with
      | IntVal len -> (if (len > 0) 
                      then (let new_array = Array(theType, (map_null l 0)) 
                        in (PointerArray (new_pos,theType), theHeap @ [new_array]))  
                      else failwith("[eval_newarray] Size must be positive"))
      | _ -> failwith("[evan_newarray] Size must be a valid integer")          

(* Length *)

and eval_length (e:exp) (theStack,theHeap) (prog:program) : val_on_stack * heap =
  let (pointer,theHeap1) = eval e (theStack,theHeap) prog 
    in (match pointer with
          | PointerArray (pos,theType) -> let result = List.nth theHeap1 pos in 
                              (match result with
                                | Array(theType,locations) -> (int_to_valonstack (List.length locations), theHeap1)
                                | _ -> failwith("[eval_length] The pointer does not point to an array"))
          | _ -> failwith("[eval_length] This is not a correct pointer"))                  

(* Array *)

and eval_array (e1:exp) (e2:exp) (theStack,theHeap) (prog:program) : val_on_stack * heap =
  let (pointer,theHeap) = eval e1 (theStack,theHeap) prog in 
    (match pointer with
      | PointerArray (p,theType) -> (let result = List.nth theHeap p in 
                          (match result with 
                          | Array(theType,lista)-> let (position,theHeap) = eval e2 (theStack,theHeap) prog 
                                                in let value = List.nth lista (valonstack_to_int(position)) 
                                                in (snd(value),theHeap)
                          | _ -> failwith("[eval_array] Not pointing to an array")))
      | _ -> failwith("[eval_array] This is not a pointer. Perhaps the array is not constructed properly?")) 

(* Arguments *)

and eval_args (args:exp list) (theStack,theHeap) (prog:program) : val_on_stack list * heap = 
  if args = [] then ([],theHeap)
  else let (head_obtained,theHeap1) = eval (hd args) (theStack,theHeap) prog in
          let (tail_obtained,theHeap2) = eval_args (tl args) (theStack,theHeap1) prog in
            (head_obtained::tail_obtained, theHeap2)   

and eval_method e id args (theStack,theHeap) (prog:program) =
  let (pointer,theHeap1) = eval e (theStack,theHeap) prog in 
    (match pointer with
      | (PointerObject (loc,ObjectType(_))) as this -> let retrieved_object = List.nth theHeap1 loc in 
                                    (match retrieved_object with
                                     | Object(theClass,theFields) -> (let Method(declType, _, args, locals, list_of_statements, return_val) = get_method id theClass prog and
                                                                      (obtained_args,theHeap2) = eval_args args (theStack,theHeap1) prog in 
                                                                      let theStack1 = create_new_stack this obtained_args args locals theClass in 
                                                                      eval_correctly list_of_statements return_val (theStack1,theHeap2) prog declType) 
                                     | _ -> failwith("[eval_method] Not pointing to an object"))
      | _ -> failwith("[eval_method] Not a valid pointer. Perhaps initialize the object?"))

(* Statements *)

and run_statements (stat:stm) (theStack,theHeap) (prog:program) : (stack * heap) =
  match stat with
    | Assignment(id, e) -> let (res_expr,theHeap1) = eval e (theStack,theHeap) prog in 
                              let (res_id,theHeap1) = eval_id id (theStack,theHeap1) prog in 
                                (if id_is_in_stack theStack id 
                                       then (assign_to_variable id res_expr theStack prog,theHeap1)     
                                       else (match get_id_from_stack theStack "this" with
                                               |  PointerObject (loc, ObjectType(_)) ->
                                                   let obj = List.nth theHeap1 loc
                                                   in (theStack, replace_in_list loc (assign_to_fields obj id res_expr prog) theHeap1 )
                                               | _ -> failwith("Assignment to undefined variable: "^id)))

    | ArrayAssignment(id, e1, e2) -> (
                      if id_is_in_stack theStack id
                      then (let (pointer,theHeap) = eval_id id (theStack,theHeap) prog in 
                               ( let (res_e2,theHeap2) = eval e2 (theStack,theHeap) prog in 
                                  ( let (res_e1,theHeap3) = eval e1 (theStack,theHeap2) prog 
                           in(match pointer with
                                | PointerArray (p, tipo) -> (let result = List.nth theHeap3 p in 
                                        (match result with
                                            | (Array(tipo,lista) as theArray) -> (let (locations,values) = List.split lista in
                                                                                  (if (res_e1 < int_to_valonstack(List.length lista) && res_e1 >=int_to_valonstack(0))
                                                                                  then 
                                                                                      (if type_control res_e2 tipo prog 
                                                                                      then let newlist = replace_in_list (valonstack_to_int(res_e1)) res_e2 values in
                                                                                            let newArray = replace_array_list theArray (List.combine locations newlist) in
                                                                                            (theStack, replace_in_list p newArray theHeap3)
                                                                                      else failwith("[ArrayAssignment] Types are mismatched"))
                                                                                  else failwith("[ArrayAssignment] Assignment is out of bound")))
                                            | _ -> failwith("[ArrayAssignment] This is not working with an array")))
                                | _ -> failwith("[ArrayAssignment] Variable " ^id ^ " is not an array")))))
                      else failwith("[ArrayAssignment] Variable " ^ id ^ " is undefined"))

      | If(e,s1,s2) -> let (condition,theHeap1) = eval e (theStack,theHeap) prog in
                       (match condition with
                          | BoolVal true -> run_statements s1 (theStack,theHeap1) prog
                          | BoolVal false -> run_statements s2 (theStack,theHeap1) prog
                          | _ -> failwith("[run_statements] This is not a boolean"))
      
      | While(e,s) -> let (condition,theHeap1) = eval e (theStack,theHeap) prog in
                      (match condition with 
                        | BoolVal true -> let (theStack1,theHeap2) = run_statements s (theStack,theHeap1) prog in run_statements (While(e,s)) (theStack1,theHeap2) prog
                        | BoolVal false -> (theStack,theHeap)
                        | _ -> failwith("[run_statements] This is not a boolean"))
      
      | Block sl -> run_statements_lis sl (theStack,theHeap) prog

(* List of statements *)

and run_statements_lis (list_of_statements:stm list) (theStack,theHeap) (prog:program) : (stack * heap) =
    match list_of_statements with
      | [] -> (theStack,theHeap)
      | x::xs -> let (theStack1,theHeap1) = run_statements x (theStack,theHeap) prog
                in run_statements_lis xs (theStack1,theHeap1) prog

(* Complete method evaluation *)

and eval_method_final (list_of_statements:stm list) (return_val:exp) (theStack,theHeap) (prog:program) : val_on_stack * heap =
    let (theStack1,theHeap1) = run_statements_lis list_of_statements (theStack,theHeap) prog
    in eval return_val (theStack1,theHeap1) prog

and eval_correctly (list_of_statements:stm list) (return_val:exp) (theStack,theHeap) (prog:program) declType : val_on_stack * heap =
  let (return_evaluated,theHeap) = eval_method_final list_of_statements return_val (theStack,theHeap) prog
  in 
    (if (type_control return_evaluated declType prog) 
    then (return_evaluated,theHeap) 
    else failwith("[eval_correctly] Return method and declared method does not work")) 
;;

(****************************************************************************************************)

(* Running the code: conversions and rearrangement of the parsed code *)

let string_of_val_on_stack (v:val_on_stack) : string =
  match v with
   | IntVal i -> (string_of_int i)
   | StrVal s -> s
   | FloatVal f -> (string_of_float f)
   | BoolVal b ->  (string_of_bool b)
   | PointerObject (p,_) -> (string_of_int p)
   | PointerArray (p,_) -> (string_of_int p)
   | NullVal -> "null"
;;

let rec find_position_in_list (list_of_classes: class_decl list) (count:int) =
  match list_of_classes with
    | [] -> failwith("[find_position_in_list] Classes not available")
    | Class(cname,_,_,_) :: rest -> if cname="Main" then count else find_position_in_list rest (count+1)
;;

let find_main_position (prog:program) : int =
  match prog with
    | Program[] -> failwith("[find_main_position] A program must be parsed first")
    | Program((Class(cname,_,_,_)::rest) as theClasses) -> find_position_in_list theClasses 0
;;

let rec get_class_from_list (list_of_classes: class_decl list) (pos:int) : class_decl =
  match list_of_classes with
    | [] -> failwith("[get_class_from_list] Classes not available")
    | Class(cname,_,_,_)::rest -> List.nth list_of_classes pos
;;

let get_the_main (prog:program) : class_decl = 
  match prog with
    | Program[] -> failwith("[get_the_main] A program must be parsed first")
    | Program((Class(cname,_,_,_) :: rest) as theClasses) -> let pos = find_main_position prog 
                                                              in get_class_from_list theClasses pos
;;

let rec filter_main_out (list_of_classes:class_decl list) : class_decl list =
  match list_of_classes with
  | [] -> []
  | (Class(cname,_,_,_) as firstclass)::xs -> if cname="Main" then (filter_main_out xs) else firstclass :: (filter_main_out xs)
;;

let rearrange_program (prog:program) : program =
  let theMain = get_the_main prog in 
  (match prog with
  | Program[] -> failwith("[remove_double_main] Program has yet to be parsed")
  | Program(list_of_classes) -> Program(theMain :: (filter_main_out list_of_classes)))
;;

(* Actual execution of the interpreter *)

let run (prog:program) : string =
  match prog with
  | Program[] -> failwith("[run] A program must be parsed first")
  | _ -> (let Method(theType, _, _, _, _, return_val) = get_method "main" "Main" prog 
            and rearranged = rearrange_program prog in
              (match rearranged with
                | Program(Class(idclass,_,_,_)::rest) -> (let theStack = [("this", PointerObject (0, ObjectType(idclass)), ObjectType(idclass))]
                                                              and theHeap = [Object(idclass, [])] in 
                                                                  let (theResult,_) = eval (MethCall(This, "main", [])) (theStack,theHeap) prog in 
                                                                    (if type_control theResult theType prog
                                                                      then string_of_val_on_stack theResult 
                                                                      else failwith("[run] Type mismatch between result and return type of the function")))                        
                | _-> failwith("[run] What you parsed is not a valid program")))
;;