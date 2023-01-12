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
%token PLUS MINUS TIMES DIV POWER PERCENT CARET TILDE AND SHL SHR
%token LTE LT EQ NEQ GT GTE OROR ANDAND PLUSPLUS
%token RANGE_I RANGE_E
%token IF THEN ELSE
%token TRUE FALSE
%token TYPE ALIAS
%token MODULE PRIVATE OPAQUE EXTEND
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

(* lowest precedence *)
%right ARROW (* \x -> (x + 1) rather than (\x -> x) + 1 *)
%left ANDAND
%left OROR
%left RANGE_I RANGE_E
%left PIPE
%left CARET
%left AND
%left EQ NEQ
%left LTE LT GT GTE
%left SHL SHR
%left PLUS MINUS
%left TIMES DIV PERCENT
%left POWER
%nonassoc UMINUS UNEGAT
%left GETTER
(* highest precedence *)

%start <Syntax.decl list> main

%%

main: 
    | SHEBANG? EOL* decl_with_eols* EOF { $3 }
    ;

decl_with_eols:
    | decl EOL+ { $1 }
    ;

decl:
    | PRIVATE private_decl       { $2 }
    | OPAQUE  opaque_type_decl   { $2 }
    | type_decl_without_modifier { $1 }
    | MODULE        UPPER_NAME LBRACE EOL* decl_with_eols+ RBRACE { DModule (MNoModifier, $2,$5) }
    | EXTEND MODULE identifier LBRACE EOL* decl_with_eols+ RBRACE { DExtendModule ($3,$6) }
    | UNDERSCORE EQUALS { failwith "E0013: Assignment to underscore" }
    | LOWER_NAME           decl_after_lower_name { $2($1) }
    | qualified_identifier decl_after_qualified  { $2($1) }
    ;


decl_after_lower_name:
    | LPAREN separated_list(COMMA,LOWER_NAME) RPAREN EQUALS expr { fun name -> DFunction (name, $2, $5) }
    | BANG                                          { fun name -> DStatement (SBang (BValue (EIdentifier ([],name)))) }
    | BANG LPAREN separated_list(COMMA,expr) RPAREN { fun name -> DStatement (SBang (BCall  (EIdentifier ([],name), $3))) }
    | EQUALS bang                                   { fun name -> DStatement (SLetBang (             name, $2)) }
    | EQUALS expr                                   { fun name -> DStatement (SLet     (LNoModifier, name, $2)) }
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

private_decl:
    | type_alias_decl        { $1(TAPrivate) } 
    | type_decl              { $1(TPrivate)  } 
    | LOWER_NAME EQUALS expr { DStatement (SLet (LPrivate, $1, $3)) }
    | MODULE UPPER_NAME LBRACE EOL* decl_with_eols+ RBRACE { DModule (MPrivate, $2, $5) }
    ;

opaque_type_decl:
    | type_decl { $1(TOpaque) }
    ;

type_decl_without_modifier:
    | type_alias_decl { $1(TANoModifier) }
    | type_decl       { $1(TNoModifier)  }
    ;

type_decl:
    | TYPE UPPER_NAME                                                 EQUALS constructor_list { fun modifier -> DType (modifier, $2, [], $4) }
    | TYPE UPPER_NAME LBRACKET separated_list(COMMA,typevar) RBRACKET EQUALS constructor_list { fun modifier -> DType (modifier, $2, $4, $7) }
    ;

type_alias_decl:
    | TYPE ALIAS UPPER_NAME                                                 EQUALS type_ { fun modifier -> DTypeAlias (modifier, $3, [], $5) }
    | TYPE ALIAS UPPER_NAME LBRACKET separated_list(COMMA,typevar) RBRACKET EQUALS type_ { fun modifier -> DTypeAlias (modifier, $3, $5, $8) }
    ;

typevar:
    | LOWER_NAME { Typevar $1 }
    ;

type_:
    (* TODO qualified/namespaced types as well! *)
    | UPPER_NAME        { TNamed $1    }
    | LOWER_NAME        { TVar $1      }
    | type_ ARROW type_ { TFn ($1, $3) }
    | UPPER_NAME LBRACKET separated_nonempty_list(COMMA,type_) RBRACKET { TCall ($1, $3) }
    | LPAREN separated_list(COMMA,type_) RPAREN {
        match $2 with
          | [] -> TUnit     (* ()          *)
          | [t] -> t        (* (List[a])   *)
          | _ -> TTuple $2  (* (Int, Bool) *)
    }
    (* TODO other types *)
    ;

bang:
    | eidentifier BANG LPAREN separated_list(COMMA,expr) RPAREN { BCall ($1, $4) }  (* IO.println!(123) *)
    | eidentifier BANG                                          { BValue $1 }       (* Id.getAndInc! *)
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
    | IF e THEN e        { failwith "E0021: If expression without an else branch" }
    | eidentifier { $1 }
    | e LPAREN separated_list(COMMA,e) RPAREN { ECall ($1, $3) }  (* f(1,2,3) *)
    | LBRACKET separated_list(COMMA,e) RBRACKET { EList $2 }      (* [1,2,3] *)
    | LBRACE separated_list(COMMA,field) RBRACE { record($2) }    (* {x:1,y:True} *)
    | e PLUS e     { EBinOp ($1, OpPlus, $3) }     (* 1 + 3  *)
    | e MINUS e    { EBinOp ($1, OpMinus, $3) }    (* 1 - 3  *)
    | e TIMES e    { EBinOp ($1, OpTimes, $3) }    (* 1 * 3  *)
    | e DIV e      { EBinOp ($1, OpDiv, $3) }      (* 1 / 3  *)
    | e POWER e    { EBinOp ($1, OpPow, $3) }      (* 1 ** 3 *)
    | e PERCENT e  { EBinOp ($1, OpMod, $3) }      (* 1 % 3  *)
    | e PLUSPLUS e { EBinOp ($1, OpAppend, $3) }   (* 1 ++ 3 *)
    | e LTE e      { EBinOp ($1, OpLte, $3) }      (* 1 <= 3 *)
    | e LT  e      { EBinOp ($1, OpLt,  $3) }      (* 1 < 3  *)
    | e EQ  e      { EBinOp ($1, OpEq,  $3) }      (* 1 == 3 *)
    | e NEQ e      { EBinOp ($1, OpNeq, $3) }      (* 1 != 3 *)
    | e GT  e      { EBinOp ($1, OpGt,  $3) }      (* 1 > 3  *)
    | e GTE e      { EBinOp ($1, OpGte, $3) }      (* 1 >= 3 *)
    | e ANDAND e   { EBinOp ($1, OpAndBool, $3) }  (* 1 && 3 *)
    | e OROR e     { EBinOp ($1, OpOrBool,  $3) }  (* 1 || 3 *)
    | e AND e      { EBinOp ($1, OpAndBin,  $3) }  (* 1 & 3  *)
    | e PIPE e     { EBinOp ($1, OpOrBin,   $3) }  (* 1 | 3  *)
    | e CARET e    { EBinOp ($1, OpXorBin,  $3) }  (* 1 ^ 3  *)
    | e SHL e      { EBinOp ($1, OpShiftL,  $3) }  (* 1 << 3 *)
    | e SHR e      { EBinOp ($1, OpShiftR,  $3) }  (* 1 >> 3 *)
    | e RANGE_I e  { EBinOp ($1, OpRangeInclusive, $3) } (* 1..3  *)
    | e RANGE_E e  { EBinOp ($1, OpRangeExclusive, $3) } (* 1...3 *)
    | MINUS e %prec UMINUS { EUnOp (OpNegateNum,  $2) } (* -123 *)
    | BANG  e %prec UNEGAT { EUnOp (OpNegateBool, $2) } (* !foo *)
    | TILDE e %prec UNEGAT { EUnOp (OpNegateBin,  $2) } (* ~123 *)
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

eidentifier:
    | identifier { EIdentifier $1 }
    ;

identifier:
    | QUALIFIER* name { ($1, $2) }
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
