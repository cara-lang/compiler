module Lexer exposing (lex)

import Error exposing (LexerError(..))
import Token exposing (Token)


lex : String -> Result LexerError (List Token)
lex source =
    Debug.todo "lex"
