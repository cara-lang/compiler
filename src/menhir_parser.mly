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
%token PLUS MINUS TIMES DIV POWER PERCENT CARET TILDE AND SHL SHR SHRU
%token LTE LT EQ NEQ GT GTE OROR ANDAND PLUSPLUS
%token RANGE_I RANGE_E
%token IF THEN ELSE
%token TRUE FALSE
%token TYPE ALIAS
%token MODULE PRIVATE OPAQUE EXTEND
%token BACKSLASH ARROW UNDERSCORE LHOLE
%token PIPELINE
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
%left ELSE
%left ANDAND
%left OROR
%left PLUSPLUS
%left PIPELINE
%left RANGE_I RANGE_E
%left PIPE
%left CARET
%left AND
%left EQ NEQ
%left LTE LT GT GTE
%left SHL SHR SHRU
%left PLUS MINUS
%left TIMES DIV PERCENT
%left POWER
%nonassoc UMINUS UNEGAT
%left LPAREN
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

stmt_with_eols:
    | stmt EOL+ { $1 }

decl:
    | PRIVATE private_decl       { $2 }
    | OPAQUE  opaque_type_decl   { $2 }
    | type_decl_without_modifier { $1 }
    | MODULE        UPPER_NAME LBRACE EOL* decl_with_eols+ RBRACE { DModule (MNoModifier, $2,$5) }
    | EXTEND MODULE identifier LBRACE EOL* decl_with_eols+ RBRACE { DExtendModule ($3,$6) }
    | stmt { DStatement $1 }
    | LOWER_NAME decl_after_lower_name { $2($1) }
    ;

decl_after_lower_name:
    | COLON type_            { fun name -> DValueAnnotation (name, $2) }
    | fun_parens EQUALS expr { fun name -> DFunction (name, $1, $3) }

    (* blocks and effect blocks *)
    |            EQUALS             block { fun name -> DBlock               (name,         $2) }
    | fun_parens EQUALS             block { fun name -> DBlockFunction       (name, $1,     $3) }
    |            EQUALS module_name block { fun name -> DEffectBlock         (name,     $2, $3) }
    | fun_parens EQUALS module_name block { fun name -> DEffectBlockFunction (name, $1, $3, $4) }
    ;

stmt:
    | LOWER_NAME           stmt_after_lower_name { $2($1) }
    | qualified_identifier stmt_after_qualified  { $2($1) }
    ;

%inline stmt_after_lower_name:
    | BANG                                          { fun name -> SBang (BValue (EIdentifier ([],name))) }
    | BANG LPAREN separated_list(COMMA,expr) RPAREN { fun name -> SBang (BCall  (EIdentifier ([],name), $3)) }
    | EQUALS bang                                   { fun name -> SLetBang (             name, $2) }
    | EQUALS expr                                   { fun name -> SLet     (LNoModifier, name, $2) }
    ;

%inline stmt_after_qualified:
    | BANG                                          { fun name -> SBang (BValue name) }
    | BANG LPAREN separated_list(COMMA,expr) RPAREN { fun name -> SBang (BCall (name, $3)) }
    ;

fun_parens:
    | LPAREN separated_list(COMMA,LOWER_NAME) RPAREN { $2 }
    ;

block:
    | LBRACE EOL* block_body { $3([]) }
    ;

block_body:
    | identifier block_item_after_identifier { $2($1) }
    | non_identifier_expr {}
    (* TODO START HERE --- we need to make this kinda tail-recursive; if block_item_after_identifier is an expr, end and wrap up; otherwise add the new stmt to the list and go for another item! We might also need to split `expr` to identifier stuff and non-identifier stuff. *)
    | EOL* RBRACE { fun acc -> acc }
    ;

constructor_list:
    | PIPE? separated_nonempty_list(PIPE,constructor) { $2 }
    ;

constructor:
    | UPPER_NAME LPAREN separated_nonempty_list(COMMA,constructor_arg) RPAREN { { name = $1; arguments = $3; } }
    | UPPER_NAME                                                              { { name = $1; arguments = []; } }
    ;

constructor_arg:
    | type_                  { (None,    $1) }
    | LOWER_NAME COLON type_ { (Some $1, $3) }
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
    | non_identifier_expr(e) and_then_expr(e) { $2($1) }
    | eidentifier            and_then_expr(e) { $2($1) }
    ;

%inline non_identifier_expr(e):
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
    | LBRACKET separated_list(COMMA,e) RBRACKET { EList $2 }      (* [1,2,3] *)
    | LBRACE separated_list(COMMA,field) RBRACE { record($2) }    (* {x:1,y:True} *)
    | MINUS e %prec UMINUS { EUnOp (OpNegateNum,  $2) } (* -123 *)
    | BANG  e %prec UNEGAT { EUnOp (OpNegateBool, $2) } (* !foo *)
    | TILDE e %prec UNEGAT { EUnOp (OpNegateBin,  $2) } (* ~123 *)
    | GETTER { ERecordGetter $1 }                (* .foo *)

    (* TODO replace `LOWER_NAME` with `pattern` *)
    | BACKSLASH separated_list(COMMA,LOWER_NAME) ARROW e { ELambda ($2, $4) }  (* \x -> 123, \x,y -> 123 *)
    | LHOLE holey_expr RPAREN { lambda_with_holes($2) }                        (* #(1 + _) *)
    ;

%inline and_then_expr(e):
    | { fun l -> l }
    | LPAREN separated_list(COMMA,e) RPAREN { fun l -> ECall (l, $2) }  (* f(1,2)  *)
    | PLUS e     { fun l -> EBinOp (l, OpPlus, $2) }                    (* 1 + 3   *)
    | MINUS e    { fun l -> EBinOp (l, OpMinus, $2) }                   (* 1 - 3   *)
    | TIMES e    { fun l -> EBinOp (l, OpTimes, $2) }                   (* 1 * 3   *)
    | DIV e      { fun l -> EBinOp (l, OpDiv, $2) }                     (* 1 / 3   *)
    | POWER e    { fun l -> EBinOp (l, OpPow, $2) }                     (* 1 ** 3  *)
    | PERCENT e  { fun l -> EBinOp (l, OpMod, $2) }                     (* 1 % 3   *)
    | PLUSPLUS e { fun l -> EBinOp (l, OpAppend, $2) }                  (* 1 ++ 3  *)
    | LTE e      { fun l -> EBinOp (l, OpLte, $2) }                     (* 1 <= 3  *)
    | LT  e      { fun l -> EBinOp (l, OpLt,  $2) }                     (* 1 < 3   *)
    | EQ  e      { fun l -> EBinOp (l, OpEq,  $2) }                     (* 1 == 3  *)
    | NEQ e      { fun l -> EBinOp (l, OpNeq, $2) }                     (* 1 != 3  *)
    | GT  e      { fun l -> EBinOp (l, OpGt,  $2) }                     (* 1 > 3   *)
    | GTE e      { fun l -> EBinOp (l, OpGte, $2) }                     (* 1 >= 3  *)
    | ANDAND e   { fun l -> EBinOp (l, OpAndBool, $2) }                 (* 1 && 3  *)
    | OROR e     { fun l -> EBinOp (l, OpOrBool,  $2) }                 (* 1 || 3  *)
    | AND e      { fun l -> EBinOp (l, OpAndBin,  $2) }                 (* 1 & 3   *)
    | PIPE e     { fun l -> EBinOp (l, OpOrBin,   $2) }                 (* 1 | 3   *)
    | CARET e    { fun l -> EBinOp (l, OpXorBin,  $2) }                 (* 1 ^ 3   *)
    | SHL e      { fun l -> EBinOp (l, OpShiftL,  $2) }                 (* 1 << 3  *)
    | SHR e      { fun l -> EBinOp (l, OpShiftR,  $2) }                 (* 1 >> 3  *)
    | SHRU e     { fun l -> EBinOp (l, OpShiftRU, $2) }                 (* 1 >>> 3 *)
    | RANGE_I e  { fun l -> EBinOp (l, OpRangeInclusive, $2) }          (* 1..3    *)
    | RANGE_E e  { fun l -> EBinOp (l, OpRangeExclusive, $2) }          (* 1...3   *)
    | PIPELINE e { fun l -> EPipeline (l, $2) }                         (* a |> b  *)
    | GETTER     { fun l -> ERecordGet (l, $1) }                        (* abc.foo *)
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

module_name:
    | QUALIFIER* UPPER_NAME { List.append $1 [$2] }

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
