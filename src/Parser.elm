module Parser exposing (parse)

import AST exposing (AST(..))
import Error exposing (ParserError(..))
import Token exposing (Token)
import Tree exposing (Tree)


parse : List Token -> Result ParserError (Tree AST)
parse _ =
    Debug.todo "parse"
