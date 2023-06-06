module Parser exposing (parse)

import AST exposing (..)
import Error exposing (ParserError(..))
import Id exposing (Id)
import List.Zipper as Zipper exposing (Zipper)
import Loc exposing (Loc)
import Parser.Internal as Parser exposing (InfixParser, InfixParserTable, Parser, TokenPred(..))
import Token exposing (Token, Type(..))
import Tree exposing (Tree)


parse : List Token -> Result ( Loc, ParserError ) AST.Program
parse tokensList =
    case Zipper.fromList tokensList of
        Nothing ->
            Err ( Loc.zero, ExpectedNonemptyTokens )

        Just tokens ->
            program tokens
                |> Result.map Tuple.first


program : Parser AST.Program
program =
    Parser.skipEol
        |> Parser.andThen
            (\_ ->
                Parser.manyUntilEOF
                    (Parser.succeed identity
                        |> Parser.keep declaration
                        |> Parser.skip Parser.skipEol
                    )
            )


declaration : Parser Decl
declaration =
    Parser.oneOf
        { commited =
            [ {- ( [ T Private, T Type, T Alias ], typeAliasDecl )
                 , ( [ T Type, T Alias ], typeAliasDecl )
                 , ( [ T Private,T Type ], typeDecl )
                 , ( [ T Opaque,T Type ], typeDecl )
                 , ( [ T Type ], typeDecl )
                 , ( [ T Extend,T Module ], extendModuleDecl )
                 , ( [ T Private,T Module ], moduleDecl )
                 ,
              -}
              ( [ T Module ], moduleDecl )
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
    statement
        |> Parser.map DStatement


{-|

    : PRIVATE? MODULE UPPER_NAME LBRACE (EOL+ declaration)+ EOL+ RBRACE

-}
moduleDecl : Parser Decl
moduleDecl =
    Parser.succeed
        (\mod name decls ->
            DModule
                { mod = mod
                , name = name
                , decls = decls
                }
        )
        |> Parser.keep
            -- PRIVATE?
            (Parser.maybe (Parser.token Private)
                |> Parser.map
                    (\maybePrivate ->
                        case maybePrivate of
                            Nothing ->
                                ModuleNoModifier

                            Just () ->
                                ModulePrivate
                    )
            )
        |> Parser.skip (Parser.token Module)
        |> Parser.keep upperName
        |> Parser.skip (Parser.token LBrace)
        |> Parser.keep
            -- (EOL+ declaration)+
            (Parser.many
                (Parser.succeed identity
                    |> Parser.skip Parser.skipEol
                    |> Parser.keep (Parser.lazy (\() -> declaration))
                )
            )
        |> Parser.skip Parser.skipEol
        |> Parser.skip (Parser.token RBrace)


statement : Parser Stmt
statement =
    Parser.oneOf
        { commited = []
        , noncommited =
            [ {- letBangStatement
                 ,
              -}
              letStatement
            , bangStatement
            ]
        }


{-|

    : PRIVATE? pattern (COLON type)? EQ expr
    = x = 123
    = x: Int = 123
    = private x = 123

We're disallowing `_` in the pattern, as it doesn't make sense
since exprs can never have effects.

-}
letStatement : Parser Stmt
letStatement =
    Parser.succeed
        (\mod lhs t expr_ ->
            SLet
                { mod = mod
                , lhs = lhs
                , type_ = t
                , expr = expr_
                }
        )
        |> Parser.keep
            -- PRIVATE?
            (Parser.maybe (Parser.token Private)
                |> Parser.map
                    (\maybePrivate ->
                        case maybePrivate of
                            Nothing ->
                                LetNoModifier

                            Just () ->
                                LetPrivate
                    )
            )
        |> Parser.keep
            (pattern
                -- TODO do we need to support a "stop the world" type of error? This would be one of those
                |> Parser.butNot PWildcard AssignmentOfExprToUnderscore
            )
        |> Parser.keep
            -- (COLON type)?
            (Parser.ifNextIs Colon
                (Parser.succeed identity
                    |> Parser.skip (Parser.token Colon)
                    |> Parser.keep type_
                )
            )
        |> Parser.skip (Parser.token Eq)
        |> Parser.skip Parser.skipEolBeforeIndented
        |> Parser.keep expr


pattern : Parser Pattern
pattern =
    Parser.oneOf
        { commited =
            [ {- ( [ T LParen, T RParen ], unitPattern )
                 ,
              -}
              ( [ P Token.isLowerName ], varPattern )

            {-
               , ( [ P Token.isUpperName ], constructorPattern )
               , ( [ P Token.isQualifier ], constructorPattern )
               , ( [ T Int_ ], intPattern )
               , ( [ T Float_ ], floatPattern )
               , ( [ T LParen ], tuplePattern )
               , ( [ T LBracket ], listPattern )
               , ( [ T Minus ], negatedPattern )
               , ( [ T Underscore ], wildcardPattern )
               , ( [ T DotDotDot ], spreadPattern )
               , ( [ T LBrace, DotDot ], recordSpreadPattern )
               , ( [ T LBrace, P Token.isLowerName ], recordFieldsPattern )
            -}
            -- TODO other patterns
            ]
        , noncommited = []
        }


{-|

    : LOWER_NAME
    = abc

-}
varPattern : Parser Pattern
varPattern =
    lowerName
        |> Parser.map PVar


type_ : Parser AST.Type
type_ =
    typeAux 0 False


typeAux : Int -> Bool -> Parser AST.Type
typeAux precedence isRight =
    Parser.pratt
        { skipEolBeforeIndented = False
        , isRight = isRight
        , precedence = precedence
        , prefix = prefixType
        , infix = infixType
        }


prefixType : Parser AST.Type
prefixType =
    Parser.oneOf
        { commited =
            [ ( [ T LParen, T RParen ], unitType )
            , ( [ T LParen ], tupleOrParenthesizedType )
            , ( [ T LBrace ], recordType )
            , ( [ P Token.isUpperName ], namedType )
            , ( [ P Token.isQualifier ], namedType )
            , ( [ P Token.isLowerName ], varType )
            ]
        , noncommited = []
        }


infixType : InfixParserTable AST.Type
infixType =
    \token ->
        case token of
            Arrow ->
                Just { precedence = 1, isRight = True, parser = fnType }

            LBracket ->
                Just { precedence = 2, isRight = True, parser = applicationType }

            _ ->
                Nothing


varType : Parser AST.Type
varType =
    \_ -> Debug.todo "varType"


{-|

    : QUALIFIER* UPPER_NAME
    = Int
    = Base.Maybe
    = List.Internal.Step

-}
namedType : Parser AST.Type
namedType =
    upperIdentifier
        |> Parser.map TNamed


{-|

    : LBRACE (recordTypeField (COMMA recordTypeField)*)? RBRACE
    = {a:Int,b:Bool}

-}
recordType : Parser AST.Type
recordType =
    Parser.separatedList
        { left = LBrace
        , right = RBrace
        , sep = Comma
        , item = recordTypeField
        , skipEol = True
        , allowTrailingSep = True
        }
        |> Parser.map TRecord


{-|

    : LOWER_NAME COLON type
    = a: Int

-}
recordTypeField : Parser RecordTypeField
recordTypeField =
    Parser.succeed RecordTypeField
        |> Parser.keep lowerName
        |> Parser.skip (Parser.token Colon)
        |> Parser.keep (Parser.lazy (\() -> type_))


{-|

    : LPAREN type (COMMA type)* RPAREN
    = (Int)
    = (Int,Float,String)

-}
tupleOrParenthesizedType : Parser AST.Type
tupleOrParenthesizedType =
    Parser.separatedNonemptyList
        { left = Token.LParen
        , right = Token.RParen
        , sep = Token.Comma
        , item = Parser.lazy (\() -> type_)
        , skipEol = True
        , allowTrailingSep = False
        }
        |> Parser.map
            (\( t, ts ) ->
                case ts of
                    [] ->
                        t

                    _ ->
                        TTuple (t :: ts)
            )


unitType : Parser AST.Type
unitType =
    Parser.succeed TUnit
        |> Parser.skip (Parser.token LParen)
        |> Parser.skip (Parser.token RParen)


{-|

    : type ARROW type
      ^^^^^^^^^^ already parsed
    = x -> y

-}
fnType : InfixParser AST.Type
fnType =
    \config ->
        typeAux config.precedence config.isRight
            |> Parser.map (\right -> TFn { from = config.left, to = right })


applicationType : InfixParser AST.Type
applicationType =
    \_ -> Debug.todo "applicationType"


{-|

    : bang
    = Foo.bar!(1,False)
    = 1 |> IO.println!

-}
bangStatement : Parser Stmt
bangStatement =
    bang
        |> Parser.map SBang


{-|

    : expr BANG (LPAREN expr (COMMA expr)* RPAREN)?
    = foo!
    = foo!()
    = Bar.foo!(123,456)
    = x |> IO.println!
    = x |> f(1) |> Foo.bar!(1,2,3)

-}
bang : Parser Bang
bang =
    Parser.succeed
        (\expr_ args ->
            case args of
                Nothing ->
                    BValue expr_

                Just args_ ->
                    BCall { fn = expr_, args = args_ }
        )
        |> Parser.keep expr
        |> Parser.skip (Parser.token Bang)
        |> Parser.keep
            (Parser.ifNextIs LParen
                (Parser.separatedList
                    { left = Token.LParen
                    , right = Token.RParen
                    , sep = Token.Comma
                    , item = expr
                    , skipEol = False
                    , allowTrailingSep = False
                    }
                )
            )


expr : Parser Expr
expr =
    exprAux 0 False


exprAux : Int -> Bool -> Parser Expr
exprAux precedence isRight =
    Parser.pratt
        { skipEolBeforeIndented = True
        , isRight = isRight
        , precedence = precedence
        , prefix = prefixExpr
        , infix = infixExpr
        }


prefixExpr : Parser Expr
prefixExpr =
    Parser.oneOf
        { commited =
            [ ( [ P Token.isInt ], intExpr )
            , ( [ P Token.isFloat ], floatExpr )

            {- , ( [ P Token.isChar ], charExpr ) -}
            , ( [ P Token.isString ], stringExpr )

            {-
               , ( [ P Token.isBacktickString ], backtickStringExpr )
               , ( [ P Token.isGetter ], recordGetterExpr )
               , ( [ T True_ ], boolExpr )
               , ( [ T False_ ], boolExpr )
               , ( [ T LParen, T RParen ], unitExpr )
               , ( [ T LParen ], tupleOrParenthesizedExpr )
            -}
            , ( [ T LBracket ], listExpr )

            {-
               , ( [ T If ], ifExpr )
               , ( [ T Case ], caseExpr )
               , ( [ T Backslash ], lambdaExpr )
               , ( [ T LHole ], holeLambdaExpr )
               , ( [ T Underscore ], holeExpr )
               , ( [ T Hole ], holeExpr )
            -}
            , ( [ T ColonColon ], rootIdentifierExpr )
            , ( [ T Minus ], unaryOpExpr Minus NegateNum )

            {-
               , ( [ T Bang ], unaryOpExpr Bang NegateBool )
               , ( [ T Tilde ], unaryOpExpr Tilde NegateBin )
            -}
            ]
        , noncommited =
            [ {- blockExpr
                 , constructorExpr
                 ,
              -}
              identifierExpr

            --, recordExpr
            ]
        }


{-|

    = "abc"
    : STRING

-}
stringExpr : Parser Expr
stringExpr =
    Parser.tokenData Token.getString
        |> Parser.map AST.String


{-|

    : LBRACKET expr (COMMA expr)* RBRACKET
    = []
    = [1]
    = [1,2]

-}
listExpr : Parser Expr
listExpr =
    -- TODO should we allow trailing sep?
    Parser.separatedList
        { left = LBracket
        , right = RBracket
        , sep = Comma
        , item = Parser.lazy (\() -> expr)
        , skipEol = True
        , allowTrailingSep = False
        }
        |> Parser.map List


{-|

    : INT
    = 123

-}
intExpr : Parser Expr
intExpr =
    Parser.tokenData Token.getInt
        |> Parser.map AST.Int


{-|

    : FLOAT
    = 123.45

-}
floatExpr : Parser Expr
floatExpr =
    Parser.tokenData Token.getFloat
        |> Parser.map AST.Float


infixExpr : InfixParserTable Expr
infixExpr =
    \token ->
        case token of
            {-
               AndAnd ->
                   Just { precedence = 1, isRight = False, parser = binaryOpExpr AndBool }

               OrOr ->
                   Just { precedence = 2, isRight = False, parser = binaryOpExpr OrBool }

               PlusPlus ->
                   Just { precedence = 3, isRight = False, parser = binaryOpExpr Append }

               Pipeline ->
                   Just { precedence = 4, isRight = False, parser = pipelineExpr }

               DotDot ->
                   Just { precedence = 5, isRight = False, parser = rangeInclusiveExpr }

               DotDotDot ->
                   Just { precedence = 5, isRight = False, parser = binaryOpExpr RangeExclusive }

               Pipe ->
                   Just { precedence = 6, isRight = False, parser = binaryOpExpr OrBin }

               Caret ->
                   Just { precedence = 7, isRight = False, parser = binaryOpExpr XorBin }

               And ->
                   Just { precedence = 8, isRight = False, parser = binaryOpExpr AndBin }

               EqEq ->
                   Just { precedence = 9, isRight = False, parser = binaryOpExpr Eq }

               Neq ->
                   Just { precedence = 9, isRight = False, parser = binaryOpExpr Neq }

               Lte ->
                   Just { precedence = 10, isRight = False, parser = binaryOpExpr Lte }

               Lt ->
                   Just { precedence = 10, isRight = False, parser = binaryOpExpr Lt }

               Gt ->
                   Just { precedence = 10, isRight = False, parser = binaryOpExpr Gt }

               Gte ->
                   Just { precedence = 10, isRight = False, parser = binaryOpExpr Gte }

               Shl ->
                   Just { precedence = 11, isRight = False, parser = binaryOpExpr ShiftL }

               Shr ->
                   Just { precedence = 11, isRight = False, parser = binaryOpExpr ShiftR }

               Shru ->
                   Just { precedence = 11, isRight = False, parser = binaryOpExpr ShiftRU }

               Plus ->
                   Just { precedence = 12, isRight = False, parser = binaryOpExpr Plus }

               Minus ->
                   Just { precedence = 12, isRight = False, parser = binaryOpExpr Minus }

               Times ->
                   Just { precedence = 13, isRight = False, parser = binaryOpExpr Times }

               Div ->
                   Just { precedence = 13, isRight = False, parser = binaryOpExpr Div }

               Percent ->
                   Just { precedence = 13, isRight = False, parser = binaryOpExpr Mod }

               Power ->
                   Just { precedence = 14, isRight = True, parser = binaryOpExpr Pow }

               LParen ->
                   Just { precedence = 15, isRight = True, parser = callExpr }

               Getter _ ->
                   Just { precedence = 16, isRight = False, parser = recordGetExpr }
            -}
            _ ->
                Nothing


{-|

    : tokenType expr

-}
unaryOpExpr : Token.Type -> UnaryOp -> Parser Expr
unaryOpExpr tokenType op =
    Parser.succeed (\e -> UnaryOp op e)
        |> Parser.skip (Parser.token tokenType)
        |> Parser.keep (Parser.lazy (\() -> expr))


{-|

    : QUALIFIER* LOWER_NAME
    = foo
    = Foo.bar
    = Foo.Bar.baz

-}
identifierExpr : Parser Expr
identifierExpr =
    lowerIdentifier
        |> Parser.map Identifier


{-|

    : COLONCOLON lowerIdentifier
    = ::foo
    = ::Foo.bar
    = ::Foo.Bar.baz

-}
rootIdentifierExpr : Parser Expr
rootIdentifierExpr =
    Parser.succeed RootIdentifier
        |> Parser.skip (Parser.token ColonColon)
        |> Parser.keep lowerIdentifier


{-|

    : QUALIFIER* LOWER_NAME
    = foo
    = Foo.bar
    = Foo.Bar.baz

-}
lowerIdentifier : Parser Id
lowerIdentifier =
    Parser.map2 Id
        (Parser.many qualifier)
        lowerName


{-|

    : QUALIFIER* UPPER_NAME
    = Foo
    = Foo.Bar
    = Foo.Bar.Baz

-}
upperIdentifier : Parser Id
upperIdentifier =
    Parser.map2 Id
        (Parser.many qualifier)
        upperName


qualifier : Parser String
qualifier =
    Parser.tokenData Token.getQualifier


lowerName : Parser String
lowerName =
    Parser.tokenData Token.getLowerName


upperName : Parser String
upperName =
    Parser.tokenData Token.getUpperName
