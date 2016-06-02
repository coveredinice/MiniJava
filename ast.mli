type program      = Program of (class_decl list)

and class_decl    = Class of id * id 
                      * (var_decl list) * (meth_decl list)
                  
and meth_decl     = Method of exp_type 
                      * id 
                      * ((exp_type * id) list)
                      * (var_decl list)
                      * (stm list)
                      * exp
                      
and var_decl      = Var of exp_type * id

and stm           = Block of (stm list)
                  | If of exp * stm * stm
                  | While of exp * stm
                  | Assignment of id * exp
                  | ArrayAssignment of id * exp * exp

and exp           = Operation of exp * binop * exp
                  | Array of exp * exp
                  | Length of exp
                  | MethCall of exp * id * (exp list)
                  | Id of id
                  | This
                  | NewArray of exp_type * exp
                  | NewId of id
                  | Not of exp
                  | Null
                  | True
                  | False
                  | Integer of int
                  | String of string
                  | Float of float

and binop         = And | Or | LessThan | Plus | Minus | Mult | Div

and exp_type      = ArrayType of exp_type
                  | BoolType
                  | IntType
                  | ObjectType of id
                  | StringType
                  | FloatType

and id = string
