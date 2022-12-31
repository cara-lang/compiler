%{ open Syntax %}

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
%token BACKSLASH ARROW UNDERSCORE LHOLE
%token LPAREN RPAREN
%token LBRACE RBRACE
%token LBRACKET RBRACKET
%token COMMA COLON BANG EQUALS 
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

%start <Syntax.stmt_list> main

%%

main: 
    | EOL* submain EOF { $2 }
    ;

submain:
    | SHEBANG EOL* stmt* { StmtList ($3, None) } (* shebang already has one implicit EOL at the end *)
    | stmt*              { StmtList ($1, None) }
    (* toplevel doesn't do returned expr ^ *)
    ;

stmt:
    (* TODO replace `LOWER_NAME` with `pattern` *)
    | LOWER_NAME EQUALS bang EOL+ { SLetBang ($1, $3) }  (* x = IO.ask!("Your name: ") *)
    | LOWER_NAME EQUALS expr EOL+ { SLet     ($1, $3) }  (* x = 123 *)
    | bang                   EOL+ { SBang $1 }           (* IO.println!("Hello") *)
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
    | LPAREN separated_list(COMMA,e) RPAREN { 
        match $2 with
            | [] -> EUnit     (* () *)
            | [e] -> e        (* (1+2) *)
            | _ -> ETuple $2  (* (1,2), (1,2,3,4,5,6,7,8) *)
    }
    | identifier { $1 }
    | e LPAREN separated_list(COMMA,e) RPAREN { ECall ($1, $3) }  (* f(1,2,3) *)
    | LBRACKET separated_list(COMMA,e) RBRACKET { EList $2 }      (* [1,2,3] *)
    | LBRACE separated_list(COMMA,field) RBRACE { record($2) }    (* {x:1,y:True} *)
    | e PLUS e  { EBinOp ($1, OpPlus, $3) }      (* 1 + 3 *)
    | e MINUS e { EBinOp ($1, OpMinus, $3) }     (* 1 - 3 *)
    | e TIMES e { EBinOp ($1, OpTimes, $3) }     (* 1 * 3 *)
    | e DIV e   { EBinOp ($1, OpDiv, $3) }       (* 1 / 3 *)
    | MINUS e %prec UMINUS { EUnOp (OpNeg, $2) } (* -123 *)
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

name:
    | UPPER_NAME { $1 }
    | LOWER_NAME { $1 }
    ;

field:
    | LOWER_NAME COLON expr { ($1, $3) }         (* {x:1,y:2} *)
    | LOWER_NAME { ($1, EIdentifier ([], $1)) }  (* {x,y} record punning -> {x:x,y:y} *)
    ;