module Parser exposing (parse)

import AST exposing (..)
import Error exposing (ParserError(..))
import Id exposing (Id)
import List.Zipper as Zipper
import Loc exposing (Loc)
import Parser.Internal as Parser exposing (InfixParser, InfixParserTable, Parser, TokenPred(..))
import Token exposing (Token, Type(..))


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
            [ ( [ T Private, T Type, T Alias ], typeAliasDecl )
            , ( [ T Type, T Alias ], typeAliasDecl )
            , ( [ T Private, T Type ], typeDecl )
            , ( [ T Opaque, T Type ], typeDecl )
            , ( [ T Type ], typeDecl )

            {-
               , ( [ T Extend,T Module ], extendModuleDecl )
               , ( [ T Private,T Module ], moduleDecl )
               ,
            -}
            , ( [ T Module ], moduleDecl )
            , ( [ T Test ], testDecl )
            ]
        , noncommited =
            [ -- This needs to be before the valueAnnotationDecl to parse `x: Int = 123`
              statementDecl
            , {- can't prefix it because `x:Int = 123` is also possible and needs
                 to be handled inside statementDecl (because of `private`!)
              -}
              valueAnnotationDecl
            , functionDecl

            {- , -- f(a,b) = expr
                 -- private f(a,b) = expr
                 -- `-`(a,b) = expr
                 -- `-`(a) = expr
               , binaryOperatorDecl
               , unaryOperatorDecl
               , -- f(a:Int, b:Int): Bool
                 -- private f(a:Int, b:Int): Bool
                 functionAnnotationDecl
            -}
            ]
        }


{-|

    : PRIVATE? LOWER_NAME LPAREN (argument (COMMA argument)*)? RPAREN (COLON type)? EQ EOL* expr
    = f(a,b) = a + b
    = f(a,b): Int = a + b
    = f(a: Int, b: Int) = a + b
    = f(a: Int, b: Int): Int = a + b
    = private f(a,b) = a + b

-}
functionDecl : Parser Decl
functionDecl =
    Parser.succeed (\mod name args retType body -> DFunction { mod = mod, name = name, args = args, retType = retType, body = body })
        |> Parser.keep
            (Parser.maybe (Parser.token Private)
                |> Parser.map
                    (\maybePrivate ->
                        case maybePrivate of
                            Nothing ->
                                FunctionNoModifier

                            Just () ->
                                FunctionPrivate
                    )
            )
        |> Parser.keep lowerName
        |> Parser.keep
            (Parser.separatedList
                { left = LParen
                , right = RParen
                , sep = Comma
                , item = argument
                , skipEol = True
                , allowTrailingSep = False
                }
            )
        |> Parser.keep
            (Parser.maybe
                (Parser.succeed identity
                    |> Parser.skip (Parser.token Colon)
                    |> Parser.keep type_
                )
            )
        |> Parser.skip (Parser.token Token.Eq)
        |> Parser.skip Parser.skipEol
        |> Parser.keep expr


{-|

    : pattern (COLON type)?
    = foo
    = foo: Int

-}
argument : Parser Argument
argument =
    Parser.succeed (\pattern_ type__ -> { pattern = pattern_, type_ = type__ })
        |> Parser.keep pattern
        |> Parser.keep
            (Parser.maybe
                (Parser.succeed identity
                    |> Parser.skip (Parser.token Colon)
                    |> Parser.keep type_
                )
            )


{-|

    : (PRIVATE | OPAQUE)? TYPE UPPER_NAME (LBRACKET typevar (COMMA typevar)* RBRACKET)? EQ constructorList
    = type Unit = Unit
    = private type MyList[a] = Empty | Cons(a,MyList[a])
    = opaque type Html[msg] = Inert(InertHtml) | Eventful(EventfulHtml[msg])

-}
typeDecl : Parser Decl
typeDecl =
    Parser.succeed (\mod name vars constructors -> DType { mod = mod, name = name, vars = vars, constructors = constructors })
        |> Parser.keep typeModifier
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
constructorList : Parser (List Constructor)
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
constructor : Parser Constructor
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
constructorArg : Parser ConstructorArg
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


typeModifier : Parser TypeModifier
typeModifier =
    Parser.oneOf
        { commited =
            [ ( [ T Private ]
              , Parser.succeed TypePrivate
                    |> Parser.skip (Parser.token Private)
              )
            , ( [ T Opaque ]
              , Parser.succeed TypeOpaque
                    |> Parser.skip (Parser.token Opaque)
              )
            ]
        , noncommited = [ Parser.succeed TypeNoModifier ]
        }


{-|

    : LOWER_NAME COLON type
    = x : Int

-}
valueAnnotationDecl : Parser Decl
valueAnnotationDecl =
    Parser.succeed (\name type__ -> DValueAnnotation { name = name, type_ = type__ })
        |> Parser.keep (Parser.tokenData Token.getLowerName)
        |> Parser.skip (Parser.token Colon)
        |> Parser.keep type_


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
        |> Parser.keep lambdaExprRaw


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
    Parser.map SLet letStatementRaw


letStatementRaw : Parser LetStmt
letStatementRaw =
    Parser.succeed
        (\mod lhs t expr_ ->
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
        |> Parser.skip (Parser.token Token.Eq)
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
            , ( [ P Token.isChar ], charExpr )
            , ( [ P Token.isString ], stringExpr )

            {-
               , ( [ P Token.isBacktickString ], backtickStringExpr )
            -}
            , ( [ P Token.isGetter ], recordGetterExpr )
            , ( [ T True_ ], boolExpr )
            , ( [ T False_ ], boolExpr )
            , ( [ T LParen, T RParen ], unitExpr )
            , ( [ T LParen ], tupleOrParenthesizedExpr )
            , ( [ T LBracket ], listExpr )
            , ( [ T Token.If ], ifExpr )

            {-
               , ( [ T Case ], caseExpr )
            -}
            , ( [ T Backslash ], lambdaExpr )

            {-
               , ( [ T LHole ], holeLambdaExpr )
               , ( [ T Underscore ], holeExpr )
               , ( [ T Hole ], holeExpr )
            -}
            , ( [ T ColonColon ], rootIdentifierExpr )
            , ( [ T Token.Minus ], prefixUnaryOpExpr Token.Minus NegateNum )

            {-
               , ( [ T Bang ], prefixUnaryOpExpr Bang NegateBool )
               , ( [ T Tilde ], prefixUnaryOpExpr Tilde NegateBin )
            -}
            ]
        , noncommited =
            [ blockExpr
            , effectBlockExpr
            , constructorExpr
            , identifierExpr
            , recordExpr
            ]
        }


{-|

    : LBRACE EOL+ (letStatement EOL+)* expr EOL+ RBRACE
    = {
        x = 1
        y = 1 + x
        (x,y)
      }

-}
blockExpr : Parser Expr
blockExpr =
    Parser.succeed (\letStmts ret -> Block { letStmts = letStmts, ret = ret })
        |> Parser.skip (Parser.token LBrace)
        |> Parser.skip Parser.skipEol
        |> Parser.keep
            (Parser.many
                (Parser.succeed identity
                    |> Parser.keep (Parser.lazy (\() -> letStatementRaw))
                    |> Parser.skip Parser.skipEol
                )
            )
        |> Parser.keep (Parser.lazy (\() -> expr))
        |> Parser.skip Parser.skipEol
        |> Parser.skip (Parser.token RBrace)


{-| TODO what about foo!(1,2,3) as the last item? instead of just an expr

    : upperIdentifier LBRACE EOL+ (statement EOL+)* (expr EOL+)? RBRACE
    = Maybe {
        head = doc.head!
        title = head.title!
        title != ""
      }

-}
effectBlockExpr : Parser Expr
effectBlockExpr =
    Parser.succeed (\module_ stmts ret -> EffectBlock { monadModule = module_, stmts = stmts, ret = ret })
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
                (Parser.succeed identity
                    |> Parser.keep (Parser.lazy (\() -> expr))
                    |> Parser.skip Parser.skipEol
                )
            )
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

    : IF expr THEN expr ELSE expr
    = if 1 == 2 then foo() else bar()

-}
ifExpr : Parser Expr
ifExpr =
    Parser.succeed (\cond then_ else_ -> AST.If { cond = cond, then_ = then_, else_ = else_ })
        |> Parser.skip (Parser.token Token.If)
        |> Parser.keep (Parser.lazy (\() -> expr))
        |> Parser.skip (Parser.token Token.Then)
        |> Parser.keep (Parser.lazy (\() -> expr))
        |> Parser.skip (Parser.token Token.Else)
        |> Parser.keep (Parser.lazy (\() -> expr))


{-|

    : BACKSLASH argument (COMMA argument)* ARROW expr
    = \x -> 1
    = \x,y -> x+y

-}
lambdaExpr : Parser Expr
lambdaExpr =
    lambdaExprRaw
        |> Parser.map (\( args, body ) -> Lambda { args = args, body = body })


lambdaExprRaw : Parser ( List Argument, Expr )
lambdaExprRaw =
    Parser.succeed (\( arg, args ) body -> ( arg :: args, body ))
        |> Parser.keep
            (Parser.separatedNonemptyList
                { left = Backslash
                , right = Arrow
                , sep = Comma
                , item = argument
                , skipEol = False
                , allowTrailingSep = False
                }
            )
        |> Parser.keep (Parser.lazy (\() -> expr))


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
    Parser.map List listExprRaw


listExprRaw : Parser (List Expr)
listExprRaw =
    -- TODO should we allow trailing sep?
    Parser.separatedList
        { left = LBracket
        , right = RBracket
        , sep = Comma
        , item = Parser.lazy (\() -> expr)
        , skipEol = True
        , allowTrailingSep = False
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
    \token ->
        case token of
            AndAnd ->
                Just { precedence = 1, isRight = False, parser = binaryOpExpr AndBool }

            OrOr ->
                Just { precedence = 2, isRight = False, parser = binaryOpExpr OrBool }

            PlusPlus ->
                Just { precedence = 3, isRight = False, parser = binaryOpExpr Append }

            Pipeline ->
                Just { precedence = 4, isRight = False, parser = pipelineExpr }

            {-
               DotDot ->
                   Just { precedence = 5, isRight = False, parser = rangeInclusiveExpr }
            -}
            DotDotDot ->
                Just { precedence = 5, isRight = False, parser = binaryOpExpr RangeExclusive }

            Pipe ->
                Just { precedence = 6, isRight = False, parser = binaryOpExpr OrBin }

            Caret ->
                Just { precedence = 7, isRight = False, parser = binaryOpExpr XorBin }

            And ->
                Just { precedence = 8, isRight = False, parser = binaryOpExpr AndBin }

            EqEq ->
                Just { precedence = 9, isRight = False, parser = binaryOpExpr AST.Eq }

            Token.Neq ->
                Just { precedence = 9, isRight = False, parser = binaryOpExpr AST.Neq }

            Token.Lte ->
                Just { precedence = 10, isRight = False, parser = binaryOpExpr AST.Lte }

            Token.Lt ->
                Just { precedence = 10, isRight = False, parser = binaryOpExpr AST.Lt }

            Token.Gt ->
                Just { precedence = 10, isRight = False, parser = binaryOpExpr AST.Gt }

            Token.Gte ->
                Just { precedence = 10, isRight = False, parser = binaryOpExpr AST.Gte }

            Shl ->
                Just { precedence = 11, isRight = False, parser = binaryOpExpr ShiftL }

            Shr ->
                Just { precedence = 11, isRight = False, parser = binaryOpExpr ShiftR }

            Shru ->
                Just { precedence = 11, isRight = False, parser = binaryOpExpr ShiftRU }

            Token.Plus ->
                Just { precedence = 12, isRight = False, parser = binaryOpExpr AST.Plus }

            Token.Minus ->
                Just { precedence = 12, isRight = False, parser = binaryOpExpr AST.Minus }

            Token.Times ->
                Just { precedence = 13, isRight = False, parser = binaryOpExpr AST.Times }

            Token.Div ->
                Just { precedence = 13, isRight = False, parser = binaryOpExpr AST.Div }

            Percent ->
                Just { precedence = 13, isRight = False, parser = binaryOpExpr Mod }

            Token.Power ->
                Just { precedence = 14, isRight = True, parser = binaryOpExpr Pow }

            -- Keeping a space (precedence = 15) for prefix unary ops
            LParen ->
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

    : expr ${tokenTag} expr
      ^^^^^^^^^^^^^^^^ already parsed

-}
binaryOpExpr : BinaryOp -> InfixParser Expr
binaryOpExpr op { left, precedence, isRight } =
    Parser.succeed (\right -> BinaryOp left op right)
        |> Parser.keep (exprAux precedence isRight)


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

    : expr LPAREN (expr (COMMA expr)*)? RPAREN
      ^^^^^^^^^^^ already parsed
    = x()
    = x(1)
    = x(1,2)

-}
callExpr : { a | left : Expr } -> Parser Expr
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
recordGetExpr : { a | left : Expr } -> Parser Expr
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
