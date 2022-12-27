%{ open Syntax %}

%token <int> INT
%token <string> STRING
%token IO_PRINTLN
%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN
%token COMMA
%token EQUALS
%token BANG
%token EOL EOF

%left PLUS MINUS /* lowest precedence */
%left TIMES DIV  /* medium precedence */
%nonassoc UMINUS /* highest precedence */

%start <Syntax.expr> main

%%

main: 
    | expr list(EOL) EOF { $1 }
    ;

expr:
    | IO_PRINTLN { EIoPrintln }
    | INT    { EInt $1 }
    | STRING { EString $1 }
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
