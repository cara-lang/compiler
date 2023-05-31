module Parser exposing (parse)

import AST exposing (..)
import Error exposing (ParserError(..))
import List.Zipper as Zipper exposing (Zipper)
import Token exposing (Token, Type(..))
import Tree exposing (Tree)


type alias Parser a =
    Zipper Token -> Result ParserError ( a, Zipper Token )


parse : List Token -> Result ParserError AST.Program
parse tokensList =
    case Zipper.fromList tokensList of
        Nothing ->
            Err ExpectedNonemptyTokens

        Just tokens ->
            case program tokens of
                Err err ->
                    Err err

                Ok ( decls, tokens_ ) ->
                    if isAtEnd tokens_ then
                        Ok decls

                    else
                        Err ExpectedEOF


program : Parser AST.Program
program =
    \tokens ->
    many declaration (tokens |> skipEol)


{-| Consumes 0+ children.
Never raises an error.
In case a child raises an error, stops looping.
-}
many : Parser a -> Parser (List a)
many childParser =
    \tokens ->
    let
        go : List a -> Parser (List a)
        go acc tokens_ =
            case childParser tokens_ of
                Err _ ->
                    Ok ( List.reverse acc, tokens_ )

                Ok ( child, tokens__ ) ->
                    go (child :: acc) tokens__
    in
    go [] tokens


skipEol : Zipper Token -> Zipper Token
skipEol tokens =
    tokens
        |> Zipper.find (\t -> t.type_ /= EOL)
        -- TODO maybe return an error instead?
        |> Maybe.withDefault tokens


isAtEnd : Zipper Token -> Bool
isAtEnd tokens =
    (Zipper.current tokens).type_ == EOF


{-| Commited: if the prefix agrees, the parser will be tried as the only option.
If the parser fails, the error of that parser will be re-raised.

Noncommited: these will be tried one after another until one succeeds.
If the oneOf list runs out, an error will be raised.

-}
oneOf :
    { commited : List ( List Token.Type, Parser a )
    , noncommited : List (Parser a)
    }
    -> Parser a
oneOf _ =
    Debug.todo "oneOf"


declaration : Parser Decl
declaration =
    oneOf
        { commited =
            [{- ( [ Private, Type, Alias ], typeAliasDecl )
                , ( [ Type, Alias ], typeAliasDecl )
                , ( [ Private, Type ], typeDecl )
                , ( [ Opaque, Type ], typeDecl )
                , ( [ Type ], typeDecl )
                , ( [ Extend, TModule ], extendModuleDecl )
                , ( [ Private, TModule ], moduleDecl )
                , ( [ TModule ], moduleDecl )
             -}
            ]
        , noncommited =
            [ -- x = 123
              -- x = foo!(123)
              -- foo!(123)
              -- private x = 123
              -- x: Int = 123
              -- This needs to be before the valueAnnotationDecl to parse `x: Int = 123`
              statementDecl

            {- , -- x : Int
                 {- can't prefix it because `x:Int = 123` is also possible and needs
                    to be handled inside statementDecl (because of `private`!)
                 -}
                 valueAnnotationDecl
               , -- f(a,b) = expr
                 -- private f(a,b) = expr
                 -- `-`(a,b) = expr
                 -- `-`(a) = expr
                 functionDecl
               , binaryOperatorDecl
               , unaryOperatorDecl
               , -- f(a:Int, b:Int): Bool
                 -- private f(a:Int, b:Int): Bool
                 functionAnnotationDecl
            -}
            ]
        }


statementDecl : Parser Decl
statementDecl =
    Debug.todo "Parser: statementDecl"


moduleDecl : Parser Decl
moduleDecl =
    Debug.todo "moduleDecl"
