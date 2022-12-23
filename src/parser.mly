%{ open Syntax %}

%token <int> INT
%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN
%token EOL

%left PLUS MINUS /* lowest precedence */
%left TIMES DIV  /* medium precedence */
%nonassoc UMINUS /* highest precedence */

%start <Syntax.expr> main

%%

main: expr EOL { $1 };
expr:
    | INT { ELiteral $1 }
    | LPAREN expr RPAREN { $2 }
    | expr PLUS expr  { EBinOp ($1, OpPlus, $3) }
    | expr MINUS expr { EBinOp ($1, OpMinus, $3) }
    | expr TIMES expr { EBinOp ($1, OpTimes, $3) }
    | expr DIV expr   { EBinOp ($1, OpDiv, $3) }
    | MINUS expr %prec UMINUS { EUnOp (OpNeg, $2) }