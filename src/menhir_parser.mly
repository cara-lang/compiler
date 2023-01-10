%{ 
open Syntax
%}

%token <int> INT
%token <float> FLOAT
%token <string> CHAR
%token <string> STRING
%token <string> GETTER
%token <string> QUALIFIER
%token <string> LOWER_NAME
%token <string> UPPER_NAME
%token <int> HOLE
%token PLUS MINUS TIMES DIV
%token IF THEN ELSE
%token TRUE FALSE
%token TYPE ALIAS
%token BACKSLASH ARROW UNDERSCORE LHOLE
%token LPAREN RPAREN     (* ( ) *)
%token LBRACE RBRACE     (* { } *)
%token LBRACKET RBRACKET (* [ ] *)
%token COMMA COLON BANG EQUALS PIPE
%token SHEBANG EOL EOF

(* TODO after relaxing `identifier` to `expr` in BCall and ECall, make CALL left-associative: 

f(1)(2)
  should parse to
[f(1)](2)
  instead of
f([1(2)])

*)

(* TODO there's a conflict about `foo = x!` vs `foo!` and `foo!(...)` *)

(* lowest precedence *)
%right ARROW (* \x -> (x + 1) rather than (\x -> x) + 1 *)
%left PLUS MINUS
%left TIMES DIV
%left GETTER
%nonassoc UMINUS
(* highest precedence *)

%start <Syntax.decl list> main

%%

main: 
    | SHEBANG? EOL* decl_with_eols+ EOF { $3 }
    ;

decl_with_eols:
    | decl EOL+ { $1 }
    ;

decl:
    | TYPE ALIAS UPPER_NAME                                                 EQUALS type_ { DTypeAlias ($3, [], $5) }
    | TYPE ALIAS UPPER_NAME LBRACKET separated_list(COMMA,typevar) RBRACKET EQUALS type_ { DTypeAlias ($3, $5, $8) }
      (* type alias Foo    = Int *)
      (* type alias Foo[a] = Dict[Int,a] *)
      (* TODO right hand side is not just a string *)
    | TYPE UPPER_NAME                                                 EQUALS constructor_list { DType ($2, [], $4) }
    | TYPE UPPER_NAME LBRACKET separated_list(COMMA,typevar) RBRACKET EQUALS constructor_list { DType ($2, $4, $7) }
      (* type Foo     = Bar                     *)
      (* type Foo     = Bar | Baz               *)
      (* type List[a] = Empty | Cons(a,List[a]) *)
      (* type Foo =
           | Bar 
           | Baz
      *)
    | LOWER_NAME           decl_after_lower_name { $2($1) }
    | qualified_identifier decl_after_qualified  { $2($1) }
    ;

decl_after_lower_name:
    | LPAREN separated_list(COMMA,LOWER_NAME) RPAREN EQUALS expr { fun name -> DFunction (name, $2, $5) }
      (* f(x,y) = e *)
      (* TODO types *)
      (* TODO patterns instead of arguments *)
    | BANG                                          { fun name -> DStatement (SBang (BValue (EIdentifier ([],name)))) }
    | BANG LPAREN separated_list(COMMA,expr) RPAREN { fun name -> DStatement (SBang (BCall  (EIdentifier ([],name), $3))) }
    | EQUALS bang                                   { fun name -> DStatement (SLetBang (name, $2)) }
    | EQUALS expr                                   { fun name -> DStatement (SLet     (name, $2)) }
    ;

decl_after_qualified:
    | BANG                                          { fun name -> DStatement (SBang (BValue name)) }
    | BANG LPAREN separated_list(COMMA,expr) RPAREN { fun name -> DStatement (SBang (BCall (name, $3))) }
    ;

constructor_list:
    | PIPE? separated_nonempty_list(PIPE,constructor) { $2 }
    ;

constructor:
    | UPPER_NAME LPAREN separated_nonempty_list(COMMA,type_) RPAREN { { name = $1; arguments = $3; } }
    | UPPER_NAME                                                    { { name = $1; arguments = []; } }
    ;

typevar:
    | LOWER_NAME { Typevar $1 }
    ;

type_:
    | UPPER_NAME { Type $1 }
    | LOWER_NAME { Type $1 } (* TODO typevar *)
    ;

bang:
    | identifier BANG LPAREN separated_list(COMMA,expr) RPAREN { BCall ($1, $4) }  (* IO.println!(123) *)
    | identifier BANG                                          { BValue $1 }       (* Id.getAndInc! *)
    ;

%inline common_expr(e):
    | INT    { EInt $1 }
    | FLOAT  { EFloat $1 }
    | CHAR   { EChar $1 } (* TODO we're guaranteed by lexer it's >0 bytes, but we still need to validate it's just one Unicode "extended grapheme cluster"! *)
    | STRING { EString $1 }
    | TRUE   { EBool true }
    | FALSE  { EBool false }
    | LPAREN separated_list(COMMA,e) RPAREN { 
        match $2 with
            | [] -> EUnit     (* () *)
            | [e] -> e        (* (1+2) *)
            | _ -> ETuple $2  (* (1,2), (1,2,3,4,5,6,7,8) *)
    }
    | IF e THEN e ELSE e { EIf ($2,$4,$6) }
    | identifier { $1 }
    | e LPAREN separated_list(COMMA,e) RPAREN { ECall ($1, $3) }  (* f(1,2,3) *)
    | LBRACKET separated_list(COMMA,e) RBRACKET { EList $2 }      (* [1,2,3] *)
    | LBRACE separated_list(COMMA,field) RBRACE { record($2) }    (* {x:1,y:True} *)
    | e PLUS e  { EBinOp ($1, OpPlus, $3) }      (* 1 + 3 *)
    | e MINUS e { EBinOp ($1, OpMinus, $3) }     (* 1 - 3 *)
    | e TIMES e { EBinOp ($1, OpTimes, $3) }     (* 1 * 3 *)
    | e DIV e   { EBinOp ($1, OpDiv, $3) }       (* 1 / 3 *)
    | MINUS e %prec UMINUS { EUnOp (OpNeg, $2) } (* -123 *)
    | GETTER { ERecordGetter $1 }                (* .foo *)
    | e GETTER  { ERecordGet ($1, $2) } (* abc.foo *)

    (* TODO replace `LOWER_NAME` with `pattern` *)
    | BACKSLASH separated_list(COMMA,LOWER_NAME) ARROW e { ELambda ($2, $4) }  (* \x -> 123, \x,y -> 123 *)
    | LHOLE holey_expr RPAREN { lambda_with_holes($2) }                        (* #(1 + _) *)
    ;

expr:
    | common_expr(expr) { $1 }

holey_expr:
    | UNDERSCORE { EIdentifier ([],"_") }
    | HOLE       { EIdentifier ([],"_" ^ Int.to_string $1) }
    | common_expr(holey_expr) { $1 }
    ;

identifier:
    | QUALIFIER* name { EIdentifier ($1, $2) }
    ;

qualified_identifier:
     (* TODO I believe we're using this in bang patterns; UPPER_NAMES don't make
     sense as monadic actions though. *)
    | QUALIFIER+ name { EIdentifier ($1, $2) }
    ;

name:
    | UPPER_NAME { $1 }
    | LOWER_NAME { $1 }
    ;

field:
    | LOWER_NAME COLON expr { ($1, $3) }         (* {x:1,y:2} *)
    | LOWER_NAME { ($1, EIdentifier ([], $1)) }  (* {x,y} record punning -> {x:x,y:y} *)
    ;
