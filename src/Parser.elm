module Parser exposing (parse)

import AST.Frontend as AST exposing (..)
import Console
import Error exposing (Error(..), ParserContext(..), ParserError(..))
import Id exposing (Id)
import Intrinsic
import Lexer
import List.Zipper as Zipper
import Loc exposing (Loc)
import Maybe.Extra
import NonemptyList exposing (NonemptyList)
import Operator exposing (BinaryOp(..), UnaryOp(..))
import Parser.HoleAnalysis as HoleAnalysis
import Parser.Internal as Parser exposing (InfixParser, InfixParserTable, Parser, TokenPred(..))
import Token exposing (Token, Type(..))


parse : List Token -> Result ( Loc, ParserError, List ParserContext ) AST.Program
parse tokensList =
    parseWith program tokensList []


parseWith : Parser a -> List Token -> List ParserContext -> Result ( Loc, ParserError, List ParserContext ) a
parseWith parser tokensList context =
    case Zipper.fromList tokensList of
        Nothing ->
            Err ( Loc.zero, ExpectedNonemptyTokens, context )

        Just tokens ->
            parser tokens context
                |> Result.map (\( a, _, _ ) -> a)
                |> Result.mapError (\err -> ( err.loc, err.error, err.context ))


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
            [ ( [ T Private, T Type, T Alias ], typeAliasDecl )
            , ( [ T Type, T Alias ], typeAliasDecl )
            , ( [ T Intrinsic, T Type ], intrinsicTypeDecl )
            , ( [ T Private, T Type ], typeDecl )
            , ( [ T Opaque, T Type ], typeDecl )
            , ( [ T Type ], typeDecl )
            , ( [ T Extend, T Module ], extendModuleDecl )
            , ( [ T Private, T Module ], moduleDecl )
            , ( [ T Module ], moduleDecl )
            , ( [ T Test ], testDecl )
            ]
        , noncommited =
            [ statementDecl
            ]
        }


{-|

    : functionBranch (EOL+ functionBranch)*
    = f(1,b) = b * 2
      f(a,b) = a + b

    (It eats multiple of them if they have the same name.)

-}
functionDefStmt : Parser Stmt
functionDefStmt =
    functionBranch Nothing
        |> Parser.andThen
            (\first ->
                let
                    ( firstMod, firstName, firstBranch ) =
                        first
                in
                Parser.many
                    (Parser.succeed identity
                        |> Parser.skip Parser.skipEol
                        |> Parser.keep (functionBranch (Just firstName))
                    )
                    |> Parser.map
                        (\rest ->
                            let
                                allBranches : NonemptyList FunctionBranch
                                allBranches =
                                    NonemptyList.fromCons
                                        firstBranch
                                        (List.map (\( _, _, branch ) -> branch) rest)

                                smush : LetModifier -> LetModifier -> LetModifier
                                smush mod1 mod2 =
                                    case ( mod1, mod2 ) of
                                        ( _, LetNoModifier ) ->
                                            mod1

                                        ( LetNoModifier, _ ) ->
                                            mod2

                                        ( _, LetPrivate ) ->
                                            mod1

                                        ( LetPrivate, _ ) ->
                                            mod2

                                        ( LetIntrinsic, LetIntrinsic ) ->
                                            LetIntrinsic

                                finalMod =
                                    List.foldl
                                        (\( otherMod, _, _ ) accMod -> smush accMod otherMod)
                                        firstMod
                                        rest
                            in
                            SFunctionDef
                                { mod = finalMod
                                , name = firstName
                                , branches = allBranches
                                }
                        )
            )
        |> Parser.inContext InFunctionDefStmt


{-|

    : PRIVATE? LOWER_NAME LPAREN (pattern (COMMA pattern)*)? RPAREN EQ EOL* expr
    = f(a,b) = a + b

-}
functionBranch : Maybe String -> Parser ( LetModifier, String, FunctionBranch )
functionBranch neededName =
    Parser.succeed (\mod name args body -> ( mod, name, { args = args, body = body } ))
        |> Parser.keep letModifierNonIntrinsic
        |> Parser.keep
            (lowerName
                |> (case neededName of
                        Nothing ->
                            identity

                        Just neededName_ ->
                            Parser.butNot_
                                (\gotName -> gotName /= neededName_)
                                ExpectedSpecificFunctionName
                   )
            )
        |> Parser.keep
            (Parser.separatedList
                { left = LParen
                , right = RParen
                , sep = Comma
                , item = patternWithType
                , skipEol = True
                , allowTrailingSep = False
                }
            )
        |> Parser.skip (Parser.token Token.Eq)
        |> Parser.skip Parser.skipEol
        |> Parser.keep expr


{-|

    : (PRIVATE|INTRINSIC)?

-}
letModifier : Parser LetModifier
letModifier =
    Parser.oneOf
        { commited =
            [ ( [ T Private ], Parser.succeed LetPrivate |> Parser.skip (Parser.token Private) )
            , ( [ T Intrinsic ], Parser.succeed LetIntrinsic |> Parser.skip (Parser.token Intrinsic) )
            ]
        , noncommited = [ Parser.succeed LetNoModifier ]
        }


letModifierNonIntrinsic : Parser LetModifier
letModifierNonIntrinsic =
    letModifier
        |> Parser.butNot LetIntrinsic UnexpectedIntrinsic


{-|

    : BACKTICK_STRING LPAREN pattern COMMA pattern RPAREN EQ EOL* expr
      ^^^^^^^^^^^^^^^ needs to be BinaryOp
    = `-`(a,b) = a * 2 + b
    = `-`(a,b): Int = a * 2 + b
    = `-`(a: Int, b: Int) = a * 2 + b
    = private `-`(a: Int, b: Int) = a * 2 + b

-}
binaryOperatorDefStmt : Parser Stmt
binaryOperatorDefStmt =
    Parser.succeed (\op left right body -> SBinaryOperatorDef { op = op, left = left, right = right, body = body })
        |> Parser.keep binaryOperatorName
        |> Parser.skip (Parser.token LParen)
        |> Parser.keep patternWithType
        |> Parser.skip (Parser.token Comma)
        |> Parser.keep patternWithType
        |> Parser.skip (Parser.token RParen)
        |> Parser.skip (Parser.token Token.Eq)
        |> Parser.skip Parser.skipEolBeforeIndented
        |> Parser.keep expr
        |> Parser.inContext InBinaryOperatorDefStmt


patternWithType : Parser ( Pattern, Maybe AST.Type )
patternWithType =
    Parser.succeed Tuple.pair
        |> Parser.keep pattern
        |> Parser.keep
            (Parser.maybe
                (Parser.succeed identity
                    |> Parser.skip (Parser.token Token.Colon)
                    |> Parser.keep type_
                )
            )


{-|

    : BACKTICK_STRING LPAREN pattern RPAREN EQ EOL* expr
      ^^^^^^^^^^^^^^^ needs to be UnaryOp
    = `-`(a) = a + 5
    = private `-`(a) = a + 5

-}
unaryOperatorDefStmt : Parser Stmt
unaryOperatorDefStmt =
    Parser.succeed (\op arg body -> SUnaryOperatorDef { op = op, arg = arg, body = body })
        |> Parser.keep unaryOperatorName
        |> Parser.skip (Parser.token LParen)
        |> Parser.keep patternWithType
        |> Parser.skip (Parser.token RParen)
        |> Parser.skip (Parser.token Token.Eq)
        |> Parser.skip Parser.skipEolBeforeIndented
        |> Parser.keep expr
        |> Parser.inContext InUnaryOperatorDefStmt


{-|

    : INTRINSIC TYPE UPPER_NAME (LBRACKET typevar (COMMA typevar)* RBRACKET)?
    = intrinsic type Unit
    = intrinsic type IO[a]

-}
intrinsicTypeDecl : Parser Decl
intrinsicTypeDecl =
    Parser.succeed
        (\name vars -> DIntrinsicType { name = name, vars = vars })
        |> Parser.skip (Parser.token Intrinsic)
        |> Parser.skip (Parser.token Type)
        |> Parser.keep upperName
        |> Parser.keep
            (Parser.maybe
                (Parser.separatedNonemptyList
                    { left = Token.LBracket
                    , right = Token.RBracket
                    , sep = Token.Comma
                    , item = typevar
                    , skipEol = False
                    , allowTrailingSep = False
                    }
                )
                |> Parser.map
                    (\maybeTypevars ->
                        case maybeTypevars of
                            Nothing ->
                                []

                            Just ( t, ts ) ->
                                t :: ts
                    )
            )
        |> Parser.inContext InIntrinsicTypeDecl


{-|

    : (PRIVATE | OPAQUE)? TYPE UPPER_NAME (LBRACKET typevar (COMMA typevar)* RBRACKET)? EQ constructorList
    = type Unit = Unit
    = private type MyList[a] = Empty | Cons(a,MyList[a])
    = opaque type Html[msg] = Inert(InertHtml) | Eventful(EventfulHtml[msg])

    Note intrinsic types do have their own declaration

-}
typeDecl : Parser Decl
typeDecl =
    Parser.succeed (\mod name vars constructors -> DType { mod = mod, name = name, vars = vars, constructors = constructors })
        |> Parser.keep typeModifierNonIntrinsic
        |> Parser.skip (Parser.token Type)
        |> Parser.keep upperName
        |> Parser.keep
            (Parser.maybe
                (Parser.separatedNonemptyList
                    { left = Token.LBracket
                    , right = Token.RBracket
                    , sep = Token.Comma
                    , item = typevar
                    , skipEol = False
                    , allowTrailingSep = False
                    }
                )
                |> Parser.map
                    (\maybeTypevars ->
                        case maybeTypevars of
                            Nothing ->
                                []

                            Just ( t, ts ) ->
                                t :: ts
                    )
            )
        |> Parser.skip (Parser.token Token.Eq)
        |> Parser.keep constructorList
        |> Parser.inContext InTypeDecl


{-|

    : PRIVATE? TYPE ALIAS UPPER_NAME (LBRACKET typevar (COMMA typevar)* RBRACKET)? EQ EOL* type
    = type alias Foo = Int
    = private type alias Bar[a,b] = Result[a,b]

-}
typeAliasDecl : Parser Decl
typeAliasDecl =
    Parser.succeed (\mod name vars body -> DTypeAlias { mod = mod, name = name, vars = vars, body = body })
        |> Parser.keep
            (Parser.oneOf
                { commited = []
                , noncommited =
                    [ Parser.succeed TypeAliasPrivate
                        |> Parser.skip (Parser.token Token.Private)
                    , Parser.succeed TypeAliasNoModifier
                    ]
                }
            )
        |> Parser.skip (Parser.token Type)
        |> Parser.skip (Parser.token Alias)
        |> Parser.keep upperName
        |> Parser.keep
            (Parser.maybe
                (Parser.separatedNonemptyList
                    { left = Token.LBracket
                    , right = Token.RBracket
                    , sep = Token.Comma
                    , item = typevar
                    , skipEol = True
                    , allowTrailingSep = False
                    }
                )
                |> Parser.map
                    (\maybeVars ->
                        case maybeVars of
                            Nothing ->
                                []

                            Just ( var, vars ) ->
                                var :: vars
                    )
            )
        |> Parser.skip (Parser.token Token.Eq)
        |> Parser.skip Parser.skipEolBeforeIndented
        |> Parser.keep type_
        |> Parser.inContext InTypeAliasDecl


{-|

    : LOWER_NAME
    = a
    = comparable123

-}
typevar : Parser String
typevar =
    lowerName


{-|

    : (EOL+ PIPE)? EOL* constructor (EOL* PIPE EOL* constructor)*
    = Foo | Bar(Bool) | Baz(Int,String)
    = | Foo | Bar(Bool) | Baz(Int,String)

-}
constructorList : Parser (List TypeConstructor)
constructorList =
    Parser.succeed (::)
        |> Parser.skip
            (Parser.maybe
                (Parser.succeed identity
                    |> Parser.skip Parser.skipEol
                    |> Parser.skip (Parser.token Pipe)
                )
            )
        |> Parser.skip Parser.skipEol
        |> Parser.keep constructor
        |> Parser.keep
            (Parser.many
                (Parser.succeed identity
                    |> Parser.skip Parser.skipEol
                    |> Parser.skip (Parser.token Pipe)
                    |> Parser.skip Parser.skipEol
                    |> Parser.keep constructor
                )
            )


{-|

    : UPPER_NAME (LPAREN constructorArg (COMMA constructorArg)* RPAREN)?
    = Foo
    = Bar(Int)
    = Bar(n: Int, verbose: Bool)

-}
constructor : Parser TypeConstructor
constructor =
    Parser.succeed (\name args -> { name = name, args = args })
        |> Parser.keep upperName
        |> Parser.keep
            (Parser.maybe
                (Parser.separatedNonemptyList
                    { left = Token.LParen
                    , right = Token.RParen
                    , sep = Token.Comma
                    , item = constructorArg
                    , skipEol = True
                    , allowTrailingSep = False
                    }
                )
                |> Parser.map
                    (\maybeArgs ->
                        case maybeArgs of
                            Nothing ->
                                []

                            Just ( arg, args ) ->
                                arg :: args
                    )
            )


{-|

    : (LOWER_NAME COLON)? type
    = Int
    = n: Int

-}
constructorArg : Parser TypeConstructorArg
constructorArg =
    Parser.succeed (\name type__ -> { name = name, type_ = type__ })
        |> Parser.keep
            (Parser.maybe
                (Parser.succeed identity
                    |> Parser.keep lowerName
                    |> Parser.skip (Parser.token Colon)
                )
            )
        |> Parser.keep type_


{-|

    : (PRIVATE|OPAQUE|INTRINSIC)?

-}
typeModifier : Parser TypeModifier
typeModifier =
    Parser.oneOf
        { commited =
            [ ( [ T Private ], Parser.succeed TypePrivate |> Parser.skip (Parser.token Private) )
            , ( [ T Opaque ], Parser.succeed TypeOpaque |> Parser.skip (Parser.token Opaque) )
            , ( [ T Intrinsic ], Parser.succeed TypeIntrinsic |> Parser.skip (Parser.token Intrinsic) )
            ]
        , noncommited = [ Parser.succeed TypeNoModifier ]
        }


typeModifierNonIntrinsic : Parser TypeModifier
typeModifierNonIntrinsic =
    typeModifier
        |> Parser.butNot TypeIntrinsic UnexpectedIntrinsic


{-|

    : (PRIVATE|INTRINSIC)? LOWER_NAME COLON type
    = x : Int

-}
valueAnnotationStmt : Parser Stmt
valueAnnotationStmt =
    Parser.succeed (\mod name type__ -> SValueAnnotation { mod = mod, name = name, type_ = type__ })
        |> Parser.keep letModifier
        |> Parser.keep (Parser.tokenData Token.getLowerName)
        |> Parser.skip (Parser.token Colon)
        |> Parser.keep type_
        |> Parser.inContext InValueAnnotationStmt


{-|

    : (PRIVATE|INTRINSIC)? BACKTICK_STRING COLON type
                                     ^^^^ needs to be `x -> y -> z`
               ^^^^^^^^^^^^^^^ needs to be BinaryOp
    = `-` : Int -> Int -> Int

-}
binaryOperatorAnnotationStmt : Parser Stmt
binaryOperatorAnnotationStmt =
    Parser.succeed
        (\mod op type__ ->
            case type__ of
                TFn a ->
                    case a.to of
                        TFn b ->
                            Parser.succeed
                                (SBinaryOperatorAnnotation
                                    { mod = mod
                                    , op = op
                                    , left = a.from
                                    , right = b.from
                                    , ret = b.to
                                    }
                                )

                        _ ->
                            Parser.fail BinaryOpAnnotationNotFn2

                _ ->
                    Parser.fail BinaryOpAnnotationNotFn2
        )
        |> Parser.keep letModifier
        |> Parser.keep binaryOperatorName
        |> Parser.skip (Parser.token Colon)
        |> Parser.keep type_
        |> Parser.andThen identity
        |> Parser.inContext InBinaryOperatorAnnotationStmt


{-|

    : (PRIVATE|INTRINSIC)? BACKTICK_STRING COLON type ARROW type
               ^^^^^^^^^^^^^^^ needs to be UnaryOp
    = `-` : Int -> Int

-}
unaryOperatorAnnotationStmt : Parser Stmt
unaryOperatorAnnotationStmt =
    Parser.succeed
        (\mod op type__ ->
            case type__ of
                TFn a ->
                    Parser.succeed
                        (SUnaryOperatorAnnotation
                            { mod = mod
                            , op = op
                            , arg = a.from
                            , ret = a.to
                            }
                        )

                _ ->
                    Parser.fail UnaryOpAnnotationNotFn1
        )
        |> Parser.keep letModifier
        |> Parser.keep unaryOperatorName
        |> Parser.skip (Parser.token Colon)
        |> Parser.keep type_
        |> Parser.andThen identity
        |> Parser.inContext InUnaryOperatorAnnotationStmt


binaryOperatorName : Parser BinaryOp
binaryOperatorName =
    Parser.tokenData Token.getBacktickString
        |> Parser.andThen
            (\name ->
                case name of
                    "+" ->
                        Parser.succeed Operator.Plus

                    "-" ->
                        Parser.succeed Operator.Minus

                    "*" ->
                        Parser.succeed Operator.Times

                    "/" ->
                        Parser.succeed Operator.Div

                    "%" ->
                        Parser.succeed Modulo

                    "**" ->
                        Parser.succeed Operator.Power

                    "|" ->
                        Parser.succeed OrBin

                    "&" ->
                        Parser.succeed AndBin

                    "^" ->
                        Parser.succeed XorBin

                    "<<" ->
                        Parser.succeed ShiftL

                    ">>" ->
                        Parser.succeed ShiftR

                    ">>>" ->
                        Parser.succeed ShiftRU

                    "<=" ->
                        Parser.succeed Operator.Lte

                    "<" ->
                        Parser.succeed Operator.Lt

                    "==" ->
                        Parser.succeed Operator.Eq

                    "!=" ->
                        Parser.succeed Operator.Neq

                    ">" ->
                        Parser.succeed Operator.Gt

                    ">=" ->
                        Parser.succeed Operator.Gte

                    "||" ->
                        Parser.succeed OrBool

                    "&&" ->
                        Parser.succeed AndBool

                    "++" ->
                        Parser.succeed Append

                    ".." ->
                        Parser.succeed RangeInclusive

                    "..." ->
                        Parser.succeed RangeExclusive

                    _ ->
                        Parser.fail <| UnknownBinaryOp name
            )


unaryOperatorName : Parser UnaryOp
unaryOperatorName =
    Parser.tokenData Token.getBacktickString
        |> Parser.andThen
            (\name ->
                case name of
                    "-" ->
                        Parser.succeed NegateNum

                    "!" ->
                        Parser.succeed NegateBool

                    "~" ->
                        Parser.succeed NegateBin

                    ".." ->
                        Parser.succeed InfiniteRange

                    _ ->
                        Parser.fail <| UnknownUnaryOp name
            )


statementDecl : Parser Decl
statementDecl =
    statement
        |> Parser.map DStatement
        |> Parser.inContext InStmtDecl


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
        |> Parser.inContext InModuleDecl


{-|

    : EXTEND MODULE moduleName LBRACE (EOL+ declaration)* EOL+ RBRACE
    = extend module Foo.Bar { x = 1 }

-}
extendModuleDecl : Parser Decl
extendModuleDecl =
    Parser.succeed
        (\moduleId decls ->
            DExtendModule
                { module_ = moduleId.qualifiers ++ [ moduleId.name ]
                , decls = decls
                }
        )
        |> Parser.skip (Parser.token Extend)
        |> Parser.skip (Parser.token Module)
        |> Parser.keep upperIdentifier
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
        |> Parser.inContext InExtendModuleDecl


testDecl : Parser Decl
testDecl =
    Parser.oneOf
        { commited =
            [ ( [ T Test, T Colon ], unitTestDecl )
            , ( [ T Test, P Token.isString, T Colon ], unitTestDecl )
            ]
        , noncommited =
            [ parameterizedTestDecl
            , propertyTypeTestDecl
            , propertyGenTestDecl
            ]
        }
        |> Parser.inContext InTestDecl


{-|

    : TEST STRING? COLON expr
    = test: 1 == 1
    = test "Example": 1 == 2
    = test: IO { 1 == 1 }

-}
unitTestDecl : Parser Decl
unitTestDecl =
    Parser.succeed (\name expr_ -> DUnitTest { name = name, expr = expr_ })
        |> Parser.skip (Parser.token Test)
        |> Parser.keep (Parser.maybe (Parser.tokenData Token.getString))
        |> Parser.skip (Parser.token Colon)
        |> Parser.keep expr


{-|

    : TEST STRING? WITH list COLON EOL* lambda
    = test with [(1,2), (2,3)]: \a,b -> a+1 == b
    = test "Example" with [(1,2)]: \a,b -> a+1 == b

-}
parameterizedTestDecl : Parser Decl
parameterizedTestDecl =
    Parser.succeed (\name table ( args, expr_ ) -> DParameterizedTest { name = name, table = table, args = args, expr = expr_ })
        |> Parser.skip (Parser.token Test)
        |> Parser.keep (Parser.maybe (Parser.tokenData Token.getString))
        |> Parser.skip (Parser.token With)
        |> Parser.keep listExprRaw
        |> Parser.skip (Parser.token Colon)
        |> Parser.skip Parser.skipEol
        |> Parser.keep lambdaExprRaw


{-|

    : TEST STRING? WITH type COLON lambda
    = test with List[Int]: \xs -> !List.isEmpty(xs)
    = test "Example" with (Int,String): \i,s -> i != String.length(s)

-}
propertyTypeTestDecl : Parser Decl
propertyTypeTestDecl =
    Parser.succeed (\name types ( args, expr_ ) -> DPropertyTypeTest { name = name, types = types, args = args, expr = expr_ })
        |> Parser.skip (Parser.token Test)
        |> Parser.keep (Parser.maybe (Parser.tokenData Token.getString))
        |> Parser.skip (Parser.token With)
        |> Parser.keep type_
        |> Parser.skip (Parser.token Colon)
        |> Parser.skip Parser.skipEol
        |> Parser.keep lambdaExprRaw


{-|

    : TEST STRING? WITH expr COLON lambda
    = test with Gen.list(Gen.int): \xs -> !List.isEmpty(xs)
    = test "Example" with (Gen.int,Gen.int): \a,b -> a != b

-}
propertyGenTestDecl : Parser Decl
propertyGenTestDecl =
    Parser.succeed (\name gens ( args, expr_ ) -> DPropertyGenTest { name = name, gens = gens, args = args, expr = expr_ })
        |> Parser.skip (Parser.token Test)
        |> Parser.keep (Parser.maybe (Parser.tokenData Token.getString))
        |> Parser.skip (Parser.token With)
        |> Parser.keep expr
        |> Parser.skip (Parser.token Colon)
        |> Parser.skip Parser.skipEol
        |> Parser.keep lambdaExprRaw


statement : Parser Stmt
statement =
    Parser.oneOf
        { commited =
            [ ( [ T Use ], useModuleStatement )
            ]
        , noncommited =
            [ -- These need to be before the valueAnnotationStmt to parse `x: Int = 123`
              letBangStatement
            , letStatement
            , bangStatement
            , functionDefStmt
            , valueAnnotationStmt
            , binaryOperatorDefStmt
            , unaryOperatorDefStmt
            , binaryOperatorAnnotationStmt
            , unaryOperatorAnnotationStmt
            ]
        }
        |> Parser.inContext InStmt


{-|

    : USE upperIdentifier
    = use Foo
    = use Foo.Bar

-}
useModuleStatement : Parser Stmt
useModuleStatement =
    Parser.succeed SUseModule
        |> Parser.skip (Parser.token Use)
        |> Parser.keep upperIdentifier
        |> Parser.inContext InUseModuleStmt


{-|

    : PRIVATE? pattern (COLON type)? EQ bang
    = x = Foo.bar!(1,False)

-}
letBangStatement : Parser Stmt
letBangStatement =
    Parser.succeed
        (\mod lhs t bang_ ->
            SLetBang
                { mod = mod
                , lhs = lhs
                , type_ = t
                , bang = bang_
                }
        )
        |> Parser.keep letModifierNonIntrinsic
        |> Parser.keep pattern
        |> Parser.keep
            -- (COLON type)?
            (Parser.ifNextIs Colon
                (Parser.succeed identity
                    |> Parser.skip (Parser.token Colon)
                    |> Parser.keep type_
                )
            )
        |> Parser.skip (Parser.token Token.Eq)
        |> Parser.skip Parser.skipEolBeforeIndented
        |> Parser.keep bang
        |> Parser.inContext InLetBangStmt


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
        |> Parser.keep letModifierNonIntrinsic
        |> Parser.keep
            (pattern
                |> Parser.butNotU PWildcard AssignmentOfExprToUnderscore
            )
        |> Parser.keep
            -- (COLON type)?
            (Parser.ifNextIs Colon
                (Parser.succeed identity
                    |> Parser.skip (Parser.token Colon)
                    |> Parser.keep type_
                )
            )
        |> Parser.skip (Parser.token Token.Eq)
        |> Parser.skip Parser.skipEolBeforeIndented
        |> Parser.keep expr
        |> Parser.inContext InLetStmt


pattern : Parser Pattern
pattern =
    patternAux 0 False
        |> Parser.inContext InPattern


patternAux : Int -> Bool -> Parser Pattern
patternAux precedence isRight =
    Parser.pratt
        { isRight = isRight
        , precedence = precedence
        , prefix = prefixPattern
        , infix = infixPattern
        }


prefixPattern : Parser Pattern
prefixPattern =
    Parser.oneOf
        { commited =
            [ ( [ T LParen, T RParen ], unitPattern )
            , ( [ P Token.isLowerName ], varPattern )
            , ( [ P Token.isUpperName ], constructorPattern )
            , ( [ P Token.isQualifier ], constructorPattern )
            , ( [ T True_ ], boolPattern )
            , ( [ T False_ ], boolPattern )
            , ( [ P Token.isInt ], intPattern )
            , ( [ P Token.isFloat ], floatPattern )
            , ( [ P Token.isChar ], charPattern )
            , ( [ T LParen ], tuplePattern )
            , ( [ T LBracket ], listPattern )
            , ( [ T Token.Minus, P Token.isInt ], negatedIntPattern )
            , ( [ T Token.Minus, P Token.isFloat ], negatedFloatPattern )
            , ( [ T Underscore ], wildcardPattern )
            , ( [ T DotDotDot ], listSpreadPattern )
            , ( [ T LBrace, T DotDot ], recordSpreadPattern )
            , ( [ T LBrace, P Token.isLowerName ], recordFieldsPattern )
            ]
        , noncommited = []
        }


infixPattern : InfixParserTable Pattern
infixPattern =
    \token { skippedEol } ->
        case token of
            Pipe ->
                Just { precedence = 1, isRight = False, parser = orPattern }

            LowerName "as" ->
                Just { precedence = 2, isRight = False, parser = asPattern }

            _ ->
                Nothing


{-|

    : DOTDOTDOT lowerName?
    = ...a
    = ...

    Only allowed inside PList.
    TODO what about PTuple?
    TODO do we want to shuffle types around to make impossible states impossible?

-}
listSpreadPattern : Parser Pattern
listSpreadPattern =
    Parser.succeed PSpread
        |> Parser.skip (Parser.token DotDotDot)
        |> Parser.keep (Parser.maybe lowerName)


{-|

    : upperIdentifier (LPAREN pattern (COMMA pattern)* RPAREN)?
    = Foo
    = Base.Foo
    = Foo(1)
    = Foo(_)

-}
constructorPattern : Parser Pattern
constructorPattern =
    Parser.succeed (\id args -> PConstructor { id = id, args = args })
        |> Parser.keep upperIdentifier
        |> Parser.keep
            (Parser.maybe
                (Parser.separatedNonemptyList
                    { left = Token.LParen
                    , right = Token.RParen
                    , sep = Token.Comma
                    , item = Parser.lazy (\() -> pattern)
                    , skipEol = False
                    , allowTrailingSep = False
                    }
                )
                |> Parser.map
                    (\maybeArgs ->
                        case maybeArgs of
                            Nothing ->
                                []

                            Just ( arg, args ) ->
                                arg :: args
                    )
            )


{-|

    : (TRUE | FALSE)
    = True
    = False

-}
boolPattern : Parser Pattern
boolPattern =
    Parser.succeed PBool
        |> Parser.keep
            (Parser.oneOf
                { commited =
                    [ ( [ T True_ ]
                      , Parser.succeed True
                            |> Parser.skip (Parser.token True_)
                      )
                    , ( [ T False_ ]
                      , Parser.succeed False
                            |> Parser.skip (Parser.token False_)
                      )
                    ]
                , noncommited = []
                }
            )


{-|

    : INT
    = 123

-}
intPattern : Parser Pattern
intPattern =
    Parser.tokenData Token.getInt
        |> Parser.map PInt


{-|

    : FLOAT
    = 123.45

-}
floatPattern : Parser Pattern
floatPattern =
    Parser.tokenData Token.getFloat
        |> Parser.map PFloat


{-|

    : CHAR
    = 'a'

-}
charPattern : Parser Pattern
charPattern =
    Parser.tokenData Token.getChar
        |> Parser.map PChar


{-|

    : MINUS INT
    = -123

-}
negatedIntPattern : Parser Pattern
negatedIntPattern =
    Parser.succeed (negate >> PInt)
        |> Parser.skip (Parser.token Token.Minus)
        |> Parser.keep (Parser.tokenData Token.getInt)


{-|

    : MINUS FLOAT
    = -123.45

-}
negatedFloatPattern : Parser Pattern
negatedFloatPattern =
    Parser.succeed (negate >> PFloat)
        |> Parser.skip (Parser.token Token.Minus)
        |> Parser.keep (Parser.tokenData Token.getFloat)


{-|

    : LPAREN (pattern (COMMA pattern)*)? RPAREN
    = (a)
    = (1,a)

-}
tuplePattern : Parser Pattern
tuplePattern =
    Parser.separatedNonemptyList
        { left = Token.LParen
        , right = Token.RParen
        , sep = Token.Comma
        , item = Parser.lazy (\() -> pattern)
        , skipEol = False
        , allowTrailingSep = False
        }
        |> Parser.map (\( p, ps ) -> PTuple (p :: ps))


{-|

    : LBRACKET (pattern (COMMA pattern)*)? RBRACKET
    = []
    = [a]
    = [1,a]

-}
listPattern : Parser Pattern
listPattern =
    Parser.separatedList
        { left = Token.LBracket
        , right = Token.RBracket
        , sep = Token.Comma
        , item = Parser.lazy (\() -> pattern)
        , skipEol = False
        , allowTrailingSep = False
        }
        |> Parser.map PList


{-|

    : UNDERSCORE
    = _

-}
wildcardPattern : Parser Pattern
wildcardPattern =
    Parser.token Underscore
        |> Parser.map (\() -> PWildcard)


{-|

    : LPAREN RPAREN
    = ()

-}
unitPattern : Parser Pattern
unitPattern =
    Parser.succeed PUnit
        |> Parser.skip (Parser.token LParen)
        |> Parser.skip (Parser.token RParen)


{-|

    : LOWER_NAME
    = abc

-}
varPattern : Parser Pattern
varPattern =
    lowerName
        |> Parser.map PVar


{-|

    : LBRACE DOTDOT RBRACE
    = {..}

-}
recordSpreadPattern : Parser Pattern
recordSpreadPattern =
    Parser.succeed PRecordSpread
        |> Parser.skip (Parser.token LBrace)
        |> Parser.skip (Parser.token DotDot)
        |> Parser.skip (Parser.token RBrace)


{-|

    : LBRACE (lowerName (COMMA lowerName)*)? RBRACE
    = {}
    = {a}
    = {a,b}

-}
recordFieldsPattern : Parser Pattern
recordFieldsPattern =
    Parser.succeed PRecordFields
        |> Parser.keep
            (Parser.separatedList
                { left = LBrace
                , right = RBrace
                , sep = Comma
                , item = lowerName
                , skipEol = False
                , allowTrailingSep = False
                }
            )


type_ : Parser AST.Type
type_ =
    typeAux 0 False


typeAux : Int -> Bool -> Parser AST.Type
typeAux precedence isRight =
    Parser.pratt
        { isRight = isRight
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
            , ( [ T Arrow ], thunkType )
            ]
        , noncommited = []
        }


infixType : InfixParserTable AST.Type
infixType =
    \token { skippedEol } ->
        case token of
            Arrow ->
                Just { precedence = 1, isRight = True, parser = fnType }

            LBracket ->
                Just { precedence = 2, isRight = True, parser = applicationType }

            _ ->
                Nothing


{-|

    : typevar
    = a
    = comparable123

-}
varType : Parser AST.Type
varType =
    Parser.map TVar typevar


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

    : ARROW type
    = -> x

-}
thunkType : Parser AST.Type
thunkType =
    Parser.succeed (\to -> TThunk { to = to })
        |> Parser.skip (Parser.token Arrow)
        |> Parser.keep (Parser.lazy (\() -> type_))


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


{-|

    : type LBRACKET type (COMMA type)* RBRACKET
      ^^^^^^^^^^^^^ already parsed
    = List[a]

-}
applicationType : InfixParser AST.Type
applicationType { left } =
    Parser.succeed (\( arg, args ) -> TApplication { type_ = left, args = arg :: args })
        |> Parser.skip
            -- to be able to use separatedNonemptyList
            Parser.moveLeft
        |> Parser.keep
            (Parser.separatedNonemptyList
                { left = LBracket
                , right = RBracket
                , sep = Comma
                , item = type_
                , skipEol = True
                , allowTrailingSep = False
                }
            )


{-|

    : bang
    = Foo.bar!(1,False)
    = 1 |> IO.println!

-}
bangStatement : Parser Stmt
bangStatement =
    bang
        |> Parser.map SBang
        |> Parser.inContext InBangStmt


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
        |> Parser.inContext InExpr


exprAux : Int -> Bool -> Parser Expr
exprAux precedence isRight =
    Parser.pratt
        { isRight = isRight
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
            , ( [ P Token.isChar ], charExpr )
            , ( [ P Token.isString ], stringExpr )
            , ( [ P Token.isBacktickString ], backtickStringExpr )
            , ( [ P Token.isGetter ], recordGetterExpr )
            , ( [ T True_ ], boolExpr )
            , ( [ T False_ ], boolExpr )
            , ( [ T LParen, P Token.isOperator, T RParen ], operatorFnExpr )
            , ( [ T LParen, T RParen ], unitExpr )
            , ( [ T LParen ], tupleOrParenthesizedExpr )
            , ( [ T LBracket ], listExpr )
            , ( [ T Token.If ], ifExpr )
            , ( [ T Token.Case ], caseExpr )
            , ( [ T Backslash ], lambdaExpr )
            , ( [ T LHole ], holeLambdaExpr )
            , ( [ T Underscore ], holeExpr )
            , ( [ P Token.isHole ], holeExpr )
            , ( [ T ColonColon ], rootIdentifierExpr )
            , ( [ T Token.Minus ], prefixUnaryOpExpr Token.Minus NegateNum )
            , ( [ T Bang ], prefixUnaryOpExpr Bang NegateBool )
            , ( [ T Tilde ], prefixUnaryOpExpr Tilde NegateBin )
            ]
        , noncommited =
            [ blockExpr
            , effectBlockExpr
            , constructorExpr
            , identifierExpr
            , recordExpr
            ]
        }


operatorFnExpr : Parser Expr
operatorFnExpr =
    Parser.succeed (\op -> OpIdentifier op)
        |> Parser.skip (Parser.token LParen)
        |> Parser.keep (Parser.tokenData Token.getOperator)
        |> Parser.skip (Parser.token RParen)


{-|

    : LBRACE EOL+ (statement EOL+)* expr EOL+ RBRACE
    = {
        x = 1
        y = 1 + x
        (x,y)
      }

-}
blockExpr : Parser Expr
blockExpr =
    Parser.succeed (\stmts ret -> Block { stmts = stmts, ret = ret })
        |> Parser.skip (Parser.token LBrace)
        |> Parser.skip (Parser.token EOL)
        |> Parser.skip Parser.skipEol
        |> Parser.keep
            (Parser.many
                (Parser.succeed identity
                    |> Parser.keep
                        (Parser.lazy (\() -> statement)
                            |> Parser.butNotU_ AST.isEffectfulStmt EffectfulStmtInPureBlock
                        )
                    |> Parser.skip Parser.skipEol
                )
            )
        |> Parser.keep
            (Parser.oneOf
                { commited = []
                , noncommited =
                    [ Parser.lazy (\() -> expr)
                    , Parser.failUnrecoverably BlockExprWithNoReturnExpr
                    ]
                }
            )
        |> Parser.skip (Parser.token EOL)
        |> Parser.skip Parser.skipEol
        |> Parser.skip (Parser.token RBrace)


{-|

    : upperIdentifier LBRACE EOL+ (statement EOL+)* ((expr|bang) EOL+)? RBRACE
    = Maybe {
        head = doc.head!
        title = head.title!
        title != ""
      }

NOTE: because we need to be able to consume a bang as the `ret`,
we're doing a bit of post-processing here.

-}
effectBlockExpr : Parser Expr
effectBlockExpr =
    Parser.succeed
        (\monadId stmts maybeRet ->
            let
                module_ =
                    monadId.qualifiers ++ [ monadId.name ]
            in
            case ( List.reverse stmts, maybeRet ) of
                ( (SBang bang_) :: revRest, Nothing ) ->
                    EffectBlock { monadModule = module_, stmts = List.reverse revRest, ret = AST.B bang_ }

                ( _, Just ret ) ->
                    EffectBlock { monadModule = module_, stmts = stmts, ret = ret }

                ( _, Nothing ) ->
                    EffectBlock { monadModule = module_, stmts = stmts, ret = AST.E Unit }
        )
        |> Parser.keep upperIdentifier
        |> Parser.skip (Parser.token LBrace)
        |> Parser.skip Parser.skipEol
        |> Parser.keep
            (Parser.many
                (Parser.succeed identity
                    |> Parser.keep (Parser.lazy (\() -> statement))
                    |> Parser.skip Parser.skipEol
                )
            )
        |> Parser.keep
            (Parser.maybe
                (Parser.lazy
                    (\() ->
                        Parser.oneOf
                            { commited = []
                            , noncommited =
                                [ bang |> Parser.map AST.B
                                , expr |> Parser.map AST.E
                                ]
                            }
                    )
                )
            )
        |> Parser.skip Parser.skipEol
        |> Parser.skip (Parser.token RBrace)


{-|

    : LBRACE (recordExprContent (COMMA recordExprContent)*)? RBRACE
    = {a:1, b:True}
    = {...a, x:123}

-}
recordExpr : Parser Expr
recordExpr =
    Parser.separatedList
        { left = LBrace
        , right = RBrace
        , sep = Comma
        , item = recordExprContent
        , skipEol = True
        , allowTrailingSep = True
        }
        |> Parser.map Record


{-|

    = foo: 123
    = foo
    = ...foo

-}
recordExprContent : Parser RecordExprContent
recordExprContent =
    Parser.oneOf
        { commited =
            [ ( [ P Token.isLowerName, T Colon ], recordExprFieldContent )
            , ( [ P Token.isLowerName ], recordExprPunContent )
            , ( [ T DotDotDot ], recordExprSpreadContent )
            ]
        , noncommited = []
        }


{-|

    : LOWER_NAME COLON expr
    = a: 1

-}
recordExprFieldContent : Parser RecordExprContent
recordExprFieldContent =
    Parser.succeed (\field expr_ -> Field { field = field, expr = expr_ })
        |> Parser.keep lowerName
        |> Parser.skip (Parser.token Colon)
        |> Parser.keep (Parser.lazy (\() -> expr))


{-|

    : LOWER_NAME
    = a

-}
recordExprPunContent : Parser RecordExprContent
recordExprPunContent =
    Parser.map Pun lowerName


{-|

    : DOTDOTDOT lowerIdentifier
    = ...a, ...Foo.a

-}
recordExprSpreadContent : Parser RecordExprContent
recordExprSpreadContent =
    Parser.succeed Spread
        |> Parser.skip (Parser.token DotDotDot)
        |> Parser.keep lowerIdentifier


{-|

    : CASE expr OF EOL* caseBranch (EOL+ caseBranch)*
    = case foo of
        1 -> "Hello"
        2 -> "World"
        _ -> "!"
    = case foo of
        1 | 2 -> "Hello"
        _ -> "!"

-}
caseExpr : Parser Expr
caseExpr =
    Parser.succeed (\subject b bs -> AST.Case { subject = subject, branches = b :: bs })
        |> Parser.skip (Parser.token Token.Case)
        |> Parser.keep (Parser.lazy (\() -> expr))
        |> Parser.skip (Parser.token Of)
        |> Parser.skip Parser.skipEolBeforeIndented
        |> Parser.keep caseBranch
        |> Parser.keep
            (Parser.many
                (Parser.succeed identity
                    |> Parser.skip (Parser.token EOL)
                    |> Parser.skip Parser.skipEolBeforeIndented
                    |> Parser.keep caseBranch
                )
            )


{-|

    : pattern ARROW expr
    = 1 -> "Hello"

-}
caseBranch : Parser CaseBranch
caseBranch =
    Parser.succeed (\p body -> { pattern = p, body = body })
        |> Parser.keep pattern
        |> Parser.skip (Parser.token Arrow)
        |> Parser.skip Parser.skipEolBeforeIndented
        |> Parser.keep (Parser.lazy (\() -> expr))


{-|

    : IF expr THEN expr ELSE expr
    = if 1 == 2 then foo() else bar()

-}
ifExpr : Parser Expr
ifExpr =
    Parser.succeed Tuple.pair
        |> Parser.skip (Parser.token Token.If)
        |> Parser.skip Parser.skipEolBeforeIndented
        |> Parser.keep (Parser.lazy (\() -> expr))
        |> Parser.skip Parser.skipEolBeforeIndented
        |> Parser.skip (Parser.token Token.Then)
        |> Parser.skip Parser.skipEolBeforeIndented
        |> Parser.keep (Parser.lazy (\() -> expr))
        |> Parser.skip Parser.skipEolBeforeIndented
        |> Parser.andThen
            (\( cond, then_ ) ->
                Parser.oneOf
                    { commited =
                        [ ( [ T Token.Else ]
                          , Parser.succeed (\else_ -> AST.If { cond = cond, then_ = then_, else_ = else_ })
                                |> Parser.skip (Parser.token Token.Else)
                                |> Parser.skip Parser.skipEolBeforeIndented
                                |> Parser.keep (Parser.lazy (\() -> expr))
                          )
                        ]
                    , noncommited =
                        [ Parser.failUnrecoverably
                            (IfWithoutElse { cond = cond, then_ = then_ })
                        ]
                    }
            )


{-|

    : BACKSLASH (pattern (COMMA pattern)*)? ARROW expr
    = \ -> 1
    = \x -> 1
    = \x,y -> x+y

-}
lambdaExpr : Parser Expr
lambdaExpr =
    lambdaExprRaw
        |> Parser.map (\( args, body ) -> Lambda { args = args, body = body })


lambdaExprRaw : Parser ( List Pattern, Expr )
lambdaExprRaw =
    Parser.succeed Tuple.pair
        |> Parser.keep
            (Parser.separatedList
                { left = Backslash
                , right = Arrow
                , sep = Comma
                , item = pattern
                , skipEol = False
                , allowTrailingSep = False
                }
            )
        |> Parser.keep (Parser.lazy (\() -> expr))


{-|

    : LHOLE expr RPAREN
    = #(1 + _)
    = #(1 + _1 - _2)

-}
holeLambdaExpr : Parser Expr
holeLambdaExpr =
    Parser.succeed identity
        |> Parser.skip (Parser.token LHole)
        |> Parser.keep (Parser.lazy (\() -> expr))
        |> Parser.skip (Parser.token RParen)
        |> Parser.andThen lambdaWithHoles


lambdaWithHoles : Expr -> Parser Expr
lambdaWithHoles expr_ =
    case HoleAnalysis.analyzeHoles expr_ of
        HoleAnalysis.NoHoles ->
            -- #(123). Basically a thunk.
            -- We desugar to:      \ -> 123
            -- Note this is _not_: \() -> 123 or \_ -> 123
            Parser.succeed <|
                Lambda
                    { args = []
                    , body = expr_
                    }

        HoleAnalysis.OnlyUnderscore ->
            -- #(_ + 1)
            -- We desugar to: \_ -> _ + 1
            -- (where the _ arg is PVar, not PWildcard)
            Parser.succeed <|
                Lambda
                    { args = [ PVar "_" ]
                    , body = expr_
                    }

        HoleAnalysis.OnlyNumbered { max } ->
            -- #(_1 + _3)
            -- We desugar to: \_1 _2 _3 -> _1 + _3
            -- (where the _N args are PVars)
            Parser.succeed <|
                Lambda
                    { args =
                        List.range 1 max
                            |> List.map (\i -> PVar ("_" ++ String.fromInt i))
                    , body = expr_
                    }

        HoleAnalysis.Error err ->
            Parser.fail err


{-|

    : HOLE|UNDERSCORE
    = _
    = _1

-}
holeExpr : Parser Expr
holeExpr =
    Parser.oneOf
        { commited = []
        , noncommited =
            [ Parser.succeed (Identifier (Id.simple "_"))
                |> Parser.skip (Parser.token Underscore)
            , Parser.succeed (\hole -> Identifier (Id.simple ("_" ++ String.fromInt hole)))
                |> Parser.keep (Parser.tokenData Token.getHole)
            ]
        }


{-|

    : TRUE|FALSE
    = True
    = False

-}
boolExpr : Parser Expr
boolExpr =
    Parser.oneOf
        { commited = []
        , noncommited =
            [ Parser.succeed True
                |> Parser.skip (Parser.token True_)
            , Parser.succeed False
                |> Parser.skip (Parser.token False_)
            ]
        }
        |> Parser.map Bool


{-|

    : LPAREN RPAREN
    = ()

-}
unitExpr : Parser Expr
unitExpr =
    Parser.succeed Unit
        |> Parser.skip (Parser.token LParen)
        |> Parser.skip (Parser.token RParen)


{-|

    : LPAREN expr (COMMA expr)* RPAREN
    = (1)
    = (1,1.25,"Hello")

-}
tupleOrParenthesizedExpr : Parser Expr
tupleOrParenthesizedExpr =
    Parser.separatedNonemptyList
        { left = LParen
        , right = RParen
        , sep = Comma
        , item = Parser.lazy (\() -> expr)
        , skipEol = True
        , allowTrailingSep = False
        }
        |> Parser.map
            (\( x, xs ) ->
                if List.isEmpty xs then
                    x

                else
                    Tuple (x :: xs)
            )


{-|

    : GETTER
    = .abc

-}
recordGetterExpr : Parser Expr
recordGetterExpr =
    Parser.tokenData Token.getGetter
        |> Parser.map AST.RecordGetter


{-|

    = 'a'
    : CHAR

-}
charExpr : Parser Expr
charExpr =
    Parser.tokenData Token.getChar
        |> Parser.map AST.Char


{-|

    = "abc"
    = "abc$def-123"
    = "abc${def}ghi"
    : STRING

-}
stringExpr : Parser Expr
stringExpr =
    Parser.tokenData Token.getString
        |> Parser.andThen parseStringInterpolation


{-|

    = `abc`
    : BACKTICK_STRING

-}
backtickStringExpr : Parser Expr
backtickStringExpr =
    Parser.tokenData Token.getBacktickString
        |> Parser.map removeBacktickStringIndentation
        |> Parser.map removeBacktickStringLeftNewline
        |> Parser.map removeBacktickStringRightNewline
        |> Parser.andThen parseStringInterpolation


removeBacktickStringLeftNewline : String -> String
removeBacktickStringLeftNewline str =
    if String.startsWith "\n" str then
        String.dropLeft 1 str

    else
        str


removeBacktickStringRightNewline : String -> String
removeBacktickStringRightNewline str =
    if String.endsWith "\n" str then
        String.dropRight 1 str

    else
        str


removeBacktickStringIndentation : String -> String
removeBacktickStringIndentation str =
    let
        lastIndentation : Int
        lastIndentation =
            countLastIndentation str

        removeIndentationFromLine : String -> Maybe String
        removeIndentationFromLine s =
            let
                prefix =
                    String.left lastIndentation s
            in
            if String.any (\c -> c /= ' ') prefix then
                Nothing

            else
                Just (String.dropLeft lastIndentation s)
    in
    str
        |> String.lines
        |> Maybe.Extra.traverse removeIndentationFromLine
        |> Maybe.map (String.join "\n")
        |> Maybe.withDefault str


countLastIndentation : String -> Int
countLastIndentation str =
    let
        go : Int -> List Char -> Int
        go acc chars =
            case chars of
                [] ->
                    0

                '\n' :: rest ->
                    {-
                       The last newline! This tells us how much to trim.

                       x =
                           `
                           abcde
                           `    ^
                    -}
                    acc

                ' ' :: rest ->
                    {-
                       Whitespace before the last newline! So far so good. Inc the counter.

                       x =
                           `
                           abcde
                           `
                       ^^^^
                    -}
                    go (acc + 1) rest

                _ :: rest ->
                    {-
                       Strings with stuff on the last line besides the whitespace. Nothing will get trimmed.

                       `abcde
                       x `
                       ^
                    -}
                    0
    in
    str
        |> String.reverse
        |> String.toList
        |> go 0


parseStringInterpolation : String -> Parser Expr
parseStringInterpolation str =
    let
        go : Maybe Expr -> List String -> Parser Expr
        go acc chunks =
            case chunks of
                [] ->
                    -- impossible
                    Parser.succeed (AST.String str)

                [ chunk ] ->
                    -- ${ not found
                    case acc of
                        Nothing ->
                            Parser.succeed (AST.String chunk)

                        Just acc_ ->
                            if String.isEmpty chunk then
                                Parser.succeed acc_

                            else
                                Parser.succeed
                                    (AST.BinaryOp acc_
                                        Operator.Append
                                        (AST.String chunk)
                                    )

                beforeOpening :: afterOpening :: rest ->
                    if String.endsWith "\\" beforeOpening then
                        -- Escaped \${ - let's ignore the \ and return ${...} verbatim
                        let
                            newBeforeOpening =
                                String.dropRight 1 beforeOpening

                            newAroundOpening =
                                newBeforeOpening ++ afterOpening

                            newAcc : Expr
                            newAcc =
                                case acc of
                                    Nothing ->
                                        AST.String newAroundOpening

                                    Just acc_ ->
                                        AST.BinaryOp
                                            acc_
                                            Operator.Append
                                            (AST.String newAroundOpening)
                        in
                        go (Just newAcc) rest

                    else
                        -- Normal case: let's interpolate the contents of the ${...}!
                        case String.split "}" afterOpening of
                            [] ->
                                -- impossible
                                Parser.succeed (AST.String str)

                            [ _ ] ->
                                -- } not found, the ${... is unterminated
                                Parser.fail UnfinishedStringInterpolation

                            insideOpening :: afterClosing :: newChunks ->
                                let
                                    ( debugModeEnabled, exprToParse ) =
                                        if String.endsWith "=" insideOpening then
                                            ( True, String.dropRight 1 insideOpening )

                                        else
                                            ( False, insideOpening )

                                    parseResult : Result ( Loc, ParserError, List ParserContext ) Expr
                                    parseResult =
                                        exprToParse
                                            |> (Lexer.lex
                                                    >> Result.mapError
                                                        (\( loc, lexerError ) ->
                                                            ( loc
                                                            , CouldntLexInsideStringInterpolation ( loc, lexerError )
                                                            , []
                                                            )
                                                        )
                                               )
                                            |> Result.andThen (\tokens -> parseWith expr tokens [])
                                in
                                case parseResult of
                                    Err ( _, err, _ ) ->
                                        Parser.fail err

                                    Ok interpolatedExpr ->
                                        let
                                            added : Expr
                                            added =
                                                if debugModeEnabled then
                                                    {- beforeOpening
                                                       ++ exprToParse
                                                       ++ "="
                                                       ++ toString interpolatedExpr
                                                       ++ afterClosing
                                                    -}
                                                    AST.BinaryOp
                                                        (AST.String beforeOpening)
                                                        Operator.Append
                                                        (AST.BinaryOp
                                                            (AST.String (exprToParse ++ "="))
                                                            Operator.Append
                                                            (AST.BinaryOp
                                                                (AST.Call
                                                                    { fn = AST.Identifier (Intrinsic.id Intrinsic.IoToString)
                                                                    , args = [ interpolatedExpr ]
                                                                    }
                                                                )
                                                                Operator.Append
                                                                (AST.String afterClosing)
                                                            )
                                                        )

                                                else
                                                    {- beforeOpening
                                                       ++ toString interpolatedExpr
                                                       ++ afterClosing
                                                    -}
                                                    AST.BinaryOp
                                                        (AST.String beforeOpening)
                                                        Operator.Append
                                                        (AST.BinaryOp
                                                            (AST.Call
                                                                { fn = AST.Identifier (Intrinsic.id Intrinsic.IoToString)
                                                                , args = [ interpolatedExpr ]
                                                                }
                                                            )
                                                            Operator.Append
                                                            (AST.String afterClosing)
                                                        )

                                            newAcc : Expr
                                            newAcc =
                                                case acc of
                                                    Nothing ->
                                                        added

                                                    Just acc_ ->
                                                        AST.BinaryOp
                                                            acc_
                                                            Operator.Append
                                                            added
                                        in
                                        go
                                            (Just newAcc)
                                            (String.join "}" (afterClosing :: newChunks) :: rest)
    in
    go Nothing (String.split "${" str)


{-|

    : LBRACKET expr (COMMA expr)* RBRACKET
    = []
    = [1]
    = [1,2]

-}
listExpr : Parser Expr
listExpr =
    Parser.map List listExprRaw


listExprRaw : Parser (List Expr)
listExprRaw =
    Parser.separatedList
        { left = LBracket
        , right = RBracket
        , sep = Comma
        , item = Parser.lazy (\() -> expr)
        , skipEol = True
        , allowTrailingSep = True
        }


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
    \token { skippedEol } ->
        case token of
            AndAnd ->
                Just { precedence = 1, isRight = False, parser = binaryOpExpr AndBool }

            OrOr ->
                Just { precedence = 2, isRight = False, parser = binaryOpExpr OrBool }

            PlusPlus ->
                Just { precedence = 3, isRight = False, parser = binaryOpExpr Append }

            Pipeline ->
                Just { precedence = 4, isRight = False, parser = pipelineExpr }

            DotDot ->
                if skippedEol then
                    Nothing

                else
                    Just { precedence = 5, isRight = False, parser = rangeInclusiveExpr }

            DotDotDot ->
                if skippedEol then
                    Nothing

                else
                    Just { precedence = 5, isRight = False, parser = binaryOpExpr RangeExclusive }

            Pipe ->
                Just { precedence = 6, isRight = False, parser = binaryOpExpr OrBin }

            Caret ->
                Just { precedence = 7, isRight = False, parser = binaryOpExpr XorBin }

            And ->
                Just { precedence = 8, isRight = False, parser = binaryOpExpr AndBin }

            EqEq ->
                Just { precedence = 9, isRight = False, parser = binaryOpExpr Operator.Eq }

            Token.Neq ->
                Just { precedence = 9, isRight = False, parser = binaryOpExpr Operator.Neq }

            Token.Lte ->
                Just { precedence = 10, isRight = False, parser = binaryOpExpr Operator.Lte }

            Token.Lt ->
                Just { precedence = 10, isRight = False, parser = binaryOpExpr Operator.Lt }

            Token.Gt ->
                Just { precedence = 10, isRight = False, parser = binaryOpExpr Operator.Gt }

            Token.Gte ->
                Just { precedence = 10, isRight = False, parser = binaryOpExpr Operator.Gte }

            Shl ->
                Just { precedence = 11, isRight = False, parser = binaryOpExpr ShiftL }

            Shr ->
                Just { precedence = 11, isRight = False, parser = binaryOpExpr ShiftR }

            Shru ->
                Just { precedence = 11, isRight = False, parser = binaryOpExpr ShiftRU }

            Token.Plus ->
                Just { precedence = 12, isRight = False, parser = binaryOpExpr Operator.Plus }

            Token.Minus ->
                Just { precedence = 12, isRight = False, parser = binaryOpExpr Operator.Minus }

            Token.Times ->
                Just { precedence = 13, isRight = False, parser = binaryOpExpr Operator.Times }

            Token.Div ->
                Just { precedence = 13, isRight = False, parser = binaryOpExpr Operator.Div }

            Percent ->
                Just { precedence = 13, isRight = False, parser = binaryOpExpr Modulo }

            Token.Power ->
                Just { precedence = 14, isRight = True, parser = binaryOpExpr Operator.Power }

            -- Keeping a gap (precedence = 15) for prefix unary ops
            LParen ->
                if skippedEol then
                    Nothing

                else
                    Just { precedence = 16, isRight = True, parser = callExpr }

            Token.Getter _ ->
                Just { precedence = 17, isRight = False, parser = recordGetExpr }

            _ ->
                Nothing


{-|

    : tokenType expr

-}
prefixUnaryOpExpr : Token.Type -> UnaryOp -> Parser Expr
prefixUnaryOpExpr tokenType op =
    let
        -- we've kept a space in the precedence sequence for this
        -- binary ops are below, fn calls are above
        precedence =
            15

        -- this is _prefix_ unary op expr :)
        isRight =
            False
    in
    Parser.succeed (\e -> UnaryOp op e)
        |> Parser.skip (Parser.token tokenType)
        |> Parser.keep (Parser.lazy (\() -> exprAux precedence isRight))


{-|

    : expr ${tokenTag} EOL* expr
      ^^^^^^^^^^^^^^^^ already parsed

-}
binaryOpExpr : BinaryOp -> InfixParser Expr
binaryOpExpr op { left, precedence, isRight } =
    Parser.succeed (\right -> BinaryOp left op right)
        |> Parser.skip Parser.skipEol
        |> Parser.keep (exprAux precedence isRight)


{-|

    : pattern ${tokenTag} pattern
      ^^^^^^^^^^^^^^^^^^^ already parsed

-}
orPattern : InfixParser Pattern
orPattern { left, precedence, isRight } =
    Parser.succeed (\right -> POr left right)
        |> Parser.keep (patternAux precedence isRight)


{-|

    : pattern "as" LOWER_NAME
      ^^^^^^^^^^^^ already parsed

    TODO should we lex "as" as its own token type, or keep it as LowerName("as")?

-}
asPattern : InfixParser Pattern
asPattern { left, precedence, isRight } =
    Parser.succeed (\name -> PAs name left)
        |> Parser.keep lowerName


{-|

    : expr PIPELINE expr
      ^^^^^^^^^^^^^ already parsed
    = a |> b

-}
pipelineExpr : InfixParser Expr
pipelineExpr { left, isRight, precedence } =
    Parser.succeed
        (\right ->
            case right of
                Call { fn, args } ->
                    -- 3 |> f(1,2) --> f(1,2,3)
                    Call { fn = fn, args = args ++ [ left ] }

                _ ->
                    -- 3 |> f --> f(3)
                    Call { fn = right, args = [ left ] }
        )
        |> Parser.keep (exprAux precedence isRight)


{-|

    : expr DOTDOT expr?
      ^^^^^^^^^^^ already parsed
    = a..b
    = a..

-}
rangeInclusiveExpr : InfixParser Expr
rangeInclusiveExpr { isRight, precedence, left } =
    Parser.oneOf
        { commited = []
        , noncommited =
            [ Parser.succeed (\right -> BinaryOp left RangeInclusive right)
                |> Parser.keep (Parser.lazy (\() -> exprAux precedence isRight))
            , Parser.succeed (UnaryOp InfiniteRange left)
            ]
        }


{-|

    : expr LPAREN (expr (COMMA expr)*)? RPAREN
      ^^^^^^^^^^^ already parsed
    = x()
    = x(1)
    = x(1,2)

-}
callExpr : InfixParser Expr
callExpr { left } =
    Parser.succeed (\args -> Call { fn = left, args = args })
        |> Parser.skip
            -- to let separatedList eat the LParen again
            Parser.moveLeft
        |> Parser.keep
            (Parser.separatedList
                { left = LParen
                , right = RParen
                , sep = Comma
                , item = expr
                , skipEol = True
                , allowTrailingSep = False
                }
            )


{-|

    : expr GETTER
      ^^^^^^^^^^^ already parsed, we need to move left to access the GETTER!
    = foo.abc
    = getRecord(123).abc

-}
recordGetExpr : InfixParser Expr
recordGetExpr { left } =
    Parser.succeed (\field -> RecordGet { record = left, field = field })
        |> Parser.skip Parser.moveLeft
        |> Parser.keep (Parser.tokenData Token.getGetter)


{-|

    : QUALIFIER* UPPER_NAME (LPAREN expr (COMMA expr)* RPAREN)?
    = Bar
    = Foo.Bar(1,2,3)

-}
constructorExpr : Parser Expr
constructorExpr =
    Parser.succeed (\id args -> Constructor_ { id = id, args = args })
        |> Parser.keep upperIdentifier
        |> Parser.keep
            (Parser.maybe
                (Parser.separatedNonemptyList
                    { left = Token.LParen
                    , right = Token.RParen
                    , sep = Token.Comma
                    , item = Parser.lazy (\() -> expr)
                    , skipEol = True
                    , allowTrailingSep = False
                    }
                )
                |> Parser.map
                    (\maybeArgs ->
                        case maybeArgs of
                            Nothing ->
                                []

                            Just ( arg, args ) ->
                                arg :: args
                    )
            )


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
