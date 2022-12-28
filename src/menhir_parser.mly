%{ open Syntax %}

%token <int> INT
%token <float> FLOAT
%token <string> STRING
%token <string> QUALIFIER
%token <string> LOWER_NAME
%token <string> UPPER_NAME
%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN
%token COMMA BANG EQUALS
%token EOL EOF

%left PLUS MINUS /* lowest precedence */
%left TIMES DIV  /* medium precedence */
%nonassoc UMINUS /* highest precedence */

%start <Syntax.stmt_list> main

%%

main: 
    | stmt* EOL* EOF { StmtList ($1, None) }
    (* in top-level we don't do the trailing returned expr *)
    ;

stmt:
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
    | STRING { EString $1 }
    | QUALIFIER* name { EIdentifier ($1, $2) }
    | LPAREN separated_list(COMMA,expr) RPAREN { 
        match $2 with
            | [] -> EUnit
            | [e] -> e
            | _ -> ETuple $2
    }
    | expr PLUS expr  { EBinOp ($1, OpPlus, $3) }
    | expr MINUS expr { EBinOp ($1, OpMinus, $3) }
    | expr TIMES expr { EBinOp ($1, OpTimes, $3) }
    | expr DIV expr   { EBinOp ($1, OpDiv, $3) }
    | MINUS expr %prec UMINUS { EUnOp (OpNeg, $2) }
    | expr LPAREN separated_list(COMMA,expr) RPAREN { ECall ($1, $3) }
    ;

name:
    | UPPER_NAME { $1 }
    | LOWER_NAME { $1 }
    ;