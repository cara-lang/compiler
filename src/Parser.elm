module Parser exposing (parse)

import AST exposing (AST(..))
import Error exposing (ParserError(..))
import List.Zipper as Zipper exposing (Zipper)
import Token exposing (Token, Type(..))
import Tree exposing (Tree)


type alias Parser =
    Zipper Token -> Result ParserError ( Tree AST, Zipper Token )


parse : List Token -> Result ParserError (Tree AST)
parse tokensList =
    case Zipper.fromList tokensList of
        Nothing ->
            Err ExpectedNonemptyTokens

        Just tokens ->
            case program tokens of
                Err err ->
                    Err err

                Ok ( programAST, tokens_ ) ->
                    if isAtEnd tokens_ then
                        Ok programAST

                    else
                        Err ExpectedEOF


program : Parser
program tokens =
    many
        Program
        declaration
        (tokens |> skipEol)


{-| Consumes 0+ children.
Never raises an error.
Returns `Tree AST` with parent as root and containing the parsed children.
In case a child raises an error, stops looping.
-}
many : AST -> Parser -> Parser
many parent childParser tokens =
    let
        go : List (Tree AST) -> Parser
        go acc tokens_ =
            case childParser tokens_ of
                Err _ ->
                    Ok ( Tree.tree parent (List.reverse acc), tokens_ )

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
    { commited : List ( List Token.Type, Parser )
    , noncommited : List Parser
    }
    -> Parser
oneOf _ =
    Debug.todo "oneOf"


declaration : Parser
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


statementDecl : Parser
statementDecl =
    Debug.todo "Parser: statementDecl"



{-
   valueAnnotationDecl : Parser
   valueAnnotationDecl =
       Debug.todo "valueAnnotationDecl"


   functionDecl : Parser
   functionDecl =
       Debug.todo "functionDecl"


   binaryOperatorDecl : Parser
   binaryOperatorDecl =
       Debug.todo "binaryOperatorDecl"


   unaryOperatorDecl : Parser
   unaryOperatorDecl =
       Debug.todo "unaryOperatorDecl"


   functionAnnotationDecl : Parser
   functionAnnotationDecl =
       Debug.todo "functionAnnotationDecl"


   typeAliasDecl : Parser
   typeAliasDecl =
       Debug.todo "typeAliasDecl"


   typeDecl : Parser
   typeDecl =
       Debug.todo "typeDecl"


   extendModuleDecl : Parser
   extendModuleDecl =
       Debug.todo "extendModuleDecl"

-}


moduleDecl : Parser
moduleDecl =
    Debug.todo "moduleDecl"
