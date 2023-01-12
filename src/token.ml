open Base

type token =
  | INT of int
  | FLOAT of float
  | CHAR of string
  | STRING of string
  | GETTER of string
  | QUALIFIER of string
  | LOWER_NAME of string
  | UPPER_NAME of string
  | HOLE of int
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | PERCENT
  | POWER
  | SHL
  | SHR
  | CARET
  | ANDAND
  | AND
  | OROR
  | LTE
  | LT
  | EQ
  | NEQ
  | GT
  | GTE
  | RANGE_I
  | RANGE_E
  | TILDE
  | IF
  | THEN
  | ELSE
  | TRUE
  | FALSE
  | TYPE
  | ALIAS
  | MODULE
  | PRIVATE
  | OPAQUE
  | EXTEND
  | BACKSLASH
  | ARROW
  | UNDERSCORE
  | LHOLE
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | LBRACKET
  | RBRACKET
  | COMMA
  | COLON
  | BANG
  | EQUALS
  | PIPE
  | SHEBANG
  | EOL
  | EOF
  [@@deriving show, compare]
