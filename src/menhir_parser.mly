%{ open Syntax %}

%token <int> INT
%token <float> FLOAT
%token <string> CHAR
%token <string> STRING
%token <string> QUALIFIER
%token <string> LOWER_NAME
%token <string> UPPER_NAME
%token PLUS MINUS TIMES DIV
%token BACKSLASH ARROW
%token LPAREN RPAREN
%token LBRACKET RBRACKET
%token COMMA BANG EQUALS
%token SHEBANG EOL EOF

%left PLUS MINUS /* lowest precedence */
%left TIMES DIV  /* medium precedence */
%nonassoc UMINUS /* highest precedence */

%start <Syntax.stmt_list> main

%%

main: 
    | EOL* SHEBANG? EOL* stmt* EOL* EOF { StmtList ($4, None) }
    (* in top-level we don't do the trailing returned expr *)
    ;

stmt:
    (* TODO replace `LOWER_NAME` with `pattern` *)
    | LOWER_NAME EQUALS bang EOL+ { SLetBang ($1, $3) }
    | LOWER_NAME EQUALS expr EOL+ { SLet     ($1, $3) }
    | bang                   EOL+ { SBang $1 }
    ;

bang:
    | expr BANG LPAREN separated_list(COMMA,expr) RPAREN { BCall ($1, $4) }
    | expr BANG                                          { BValue $1 }
    ;

expr:
    | INT    { EInt $1 }
    | FLOAT  { EFloat $1 }
    | CHAR   { EChar $1 } (* TODO we're guaranteed by lexer it's >0 bytes, but we still need to validate it's just one Unicode "extended grapheme cluster"! *)
    | STRING { EString $1 }
    | QUALIFIER* name { EIdentifier ($1, $2) }
    | LPAREN separated_list(COMMA,expr) RPAREN { 
        match $2 with
            | [] -> EUnit
            | [e] -> e
            | _ -> ETuple $2
    }
    | LBRACKET separated_list(COMMA,expr) RBRACKET { EList $2 }
    | expr PLUS expr  { EBinOp ($1, OpPlus, $3) }
    | expr MINUS expr { EBinOp ($1, OpMinus, $3) }
    | expr TIMES expr { EBinOp ($1, OpTimes, $3) }
    | expr DIV expr   { EBinOp ($1, OpDiv, $3) }
    | MINUS expr %prec UMINUS { EUnOp (OpNeg, $2) }
    | expr LPAREN separated_list(COMMA,expr) RPAREN { ECall ($1, $3) }

    (* TODO replace `LOWER_NAME` with `pattern` *)
    | BACKSLASH                             LOWER_NAME         ARROW expr { ELambda ([$2], $4) }
    | BACKSLASH LPAREN separated_list(COMMA,LOWER_NAME) RPAREN ARROW expr { ELambda ($3, $6) }
    ;

name:
    | UPPER_NAME { $1 }
    | LOWER_NAME { $1 }
    ;