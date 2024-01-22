module Codegen.HVM exposing (codegenProgram)

{-| This module deals with desugaring (lowering) from `AST.Backend`
to `HVM.AST`.
-}

import AST.Backend as AST
import Basics.Extra
import BiDict
import Common
import Debug.Extra
import Env
import HVM.AST as HVM
import Id.Qualified exposing (QualifiedId)
import Set


codegenProgram : AST.Program -> HVM.File
codegenProgram program =
    let
        maxTupleArity : Maybe Int
        maxTupleArity =
            program
                |> AST.findExprs AST.isTuple
                |> List.filterMap AST.tupleArity
                |> List.maximum

        tupleGettersFile : Maybe HVM.File
        tupleGettersFile =
            maxTupleArity
                |> Maybe.map
                    (\maxArity ->
                        { adts = []
                        , rules =
                            -- TODO: only generate those that are used (found across the program)?
                            List.range 0 (maxArity - 1)
                                |> List.concatMap
                                    (\index ->
                                        tupleGetterDef index (Common.tupleIndexToNumericField index)
                                            :: (BiDict.getReverse index Common.namedTupleFields
                                                    |> Set.toList
                                                    |> List.map (tupleGetterDef index)
                                               )
                                    )
                        }
                    )

        initFile : HVM.File
        initFile =
            [ tupleGettersFile
            ]
                |> List.filterMap identity
                |> List.foldr HVM.concatFiles preambleFile
    in
    program
        |> List.map declToFile
        |> List.foldr HVM.concatFiles initFile


preambleFile : HVM.File
preambleFile =
    { adts =
        [ { name = intrinsics.tupleEndType
          , constructors =
                [ { name = intrinsics.tupleEnd
                  , arity = 0
                  }
                ]
          }
        ]
    , rules = []
    }


{-|

    (Cara.GetRec.el1    (a,*))     = a
    (Cara.GetRec.first  (a,*))     = a
    (Cara.GetRec.el2    (*,(a,*))) = a
    (Cara.GetRec.second (*,(a,*))) = a

-}
tupleGetterDef : Int -> String -> HVM.Rule
tupleGetterDef index field =
    { functionName = intrinsics.recordGetter field
    , args = [ tupleGetterPattern index (HVM.PVar intrinsics.extractedVar) ]
    , body = HVM.Var intrinsics.extractedVar
    }


{-|

    index 0: (a,_)
             PTup (PVar "a", PWildcard)
    index 1: (_,(a,_))
             PTup (PWildcard, PTup (PVar "a", PWildcard))
    index 2: (_,(_,(a,_)))
             PTup (PWildcard, PTup (PWildcard, PTup (PVar "a", PWildcard)))

-}
tupleGetterPattern : Int -> HVM.Pattern -> HVM.Pattern
tupleGetterPattern index childPattern =
    let
        wrap : HVM.Pattern -> HVM.Pattern
        wrap p =
            HVM.PTup ( HVM.PWildcard, p )
    in
    Basics.Extra.doNTimes
        index
        wrap
        (HVM.PTup ( childPattern, HVM.PWildcard ))


todoRule : String -> a -> List HVM.Rule
todoRule message thing =
    [ { functionName = message
      , args = []
      , body = HVM.Str <| Debug.toString thing
      }
    ]


app : String -> List HVM.Term -> HVM.Term
app name args =
    if List.isEmpty args then
        HVM.Var name

    else
        HVM.App
            { function = HVM.Var name
            , args = args
            }


{-| This should convert `(1,2,3,4)` into `(1,(2,(3,(4,Cara.TupleEnd))))`

PERF: would it be better to compile tuples to `data` of the given arity rather
than to nested HVM 2-tuples?

-}
tuple : List HVM.Term -> HVM.Term
tuple terms =
    List.foldr
        (\new acc -> HVM.Tup ( new, acc ))
        (HVM.Var intrinsics.tupleEnd)
        terms


idToString : QualifiedId -> String
idToString id =
    if id.qualifiers == ( Env.rootModule.name, [] ) then
        id.name

    else
        Id.Qualified.toString id


declToFile : AST.Decl -> HVM.File
declToFile decl =
    case decl of
        AST.DType r ->
            { adts =
                [ { name = idToString r.id
                  , constructors =
                        r.constructors
                            |> List.map
                                (\c ->
                                    { name = idToString c.id
                                    , arity = c.arity
                                    }
                                )
                  }
                ]
            , rules = []
            }

        AST.DLetStmt r ->
            { adts = []
            , rules = patternToRules r
            }

        AST.DFunctionDef r ->
            { adts = []
            , rules =
                [ { functionName = idToString r.id
                  , args = List.map pattern r.args
                  , body = exprToTerm r.body
                  }
                ]
            }


patternToRules : { lhs : AST.Pattern, expr : AST.Expr } -> List HVM.Rule
patternToRules r =
    let
        rhsTerm =
            exprToTerm r.expr
    in
    case r.lhs of
        {-
           x = 123
           -->
           x = 123
        -}
        AST.PVar name ->
            [ { functionName = name
              , args = []
              , body = rhsTerm
              }
            ]

        {-
           Bar(...) = someBar
           -->
           ... = match someBar {
               (Bar ...): a
           }

           (potentially multiple rules)
        -}
        AST.PConstructor ctr ->
            {- Here we blindly assume the constructor is a singleton
               one (type Foo = Bar(Int)) and so it should allow
               the `Bar(a,b) = someFoo` destructuring.

               Desugar phase should have caught this if it's not.
               If it's not, the HVM file will not compile as HVM
               also does exhaustive checking.
            -}
            ctr.args
                |> List.indexedMap
                    (constructorArgPatternToRules
                        (idToString ctr.id)
                        rhsTerm
                        (List.length ctr.args)
                    )
                |> List.concat

        _ ->
            Debug.Extra.todo1 "Codegen.HVM.patternToRules unhandled DLetStmt" r


constructorArgPatternToRules : String -> HVM.Term -> Int -> Int -> AST.Pattern -> List HVM.Rule
constructorArgPatternToRules ctrId rhsTerm allArgs argIndex argPattern =
    let
        {-
           Bar(a1,a2,a3,a4,a5,a6) (allArgs: 6)
                     ^^ (argIndex: 2)
           before: 2
           after: 3
        -}
        before =
            argIndex

        after =
            allArgs - before - 1

        {-
           Bar(a, _, _) = someBar
           ------------------------> (index 0)
           a = match someBar {
             (Bar a * *): a
           }

           Bar(_, b, _) = someBar
           ------------------------> (index 1)
           b = match someBar {
             (Bar * a *): a
           }

           Bar(_, _, c) = someBar
           ------------------------> (index 2)
           c = match someBar {
             (Bar * a *): a
           }
        -}
        atCorrectIndex : HVM.Pattern -> HVM.Pattern
        atCorrectIndex childPattern =
            HVM.PCtr ctrId
                (List.concat
                    [ List.repeat before HVM.PWildcard
                    , [ childPattern ]
                    , List.repeat after HVM.PWildcard
                    ]
                )
    in
    case argPattern of
        {-
           Bar(x) = someFoo
           -->
           x = match someFoo {
               (Bar a): a
           }
        -}
        AST.PVar name ->
            [ { functionName = name
              , args = []
              , body =
                    HVM.Match
                        { value = rhsTerm
                        , arms =
                            [ ( atCorrectIndex (HVM.PVar intrinsics.extractedVar)
                              , HVM.Var intrinsics.extractedVar
                              )
                            ]
                        }
              }
            ]

        {-
           B((b1,b2,b3,b4)) = b
           -->
           b1 = match b { (B (a,*)): a }
           b2 = match b { (B (*,(a,*))): a }
           b3 = match b { (B (*,(*,(a,*)))): a }
           b4 = match b { (B (*,(*,(*,(a,*))))): a }

           TODO this needs generalizing with the above - some kind of recursive function
        -}
        AST.PTuple elements ->
            elements
                |> List.indexedMap
                    (\elIndex elPattern ->
                        -- TODO this should get generalized - recurse somehow?
                        case elPattern of
                            AST.PVar name ->
                                { functionName = name
                                , args = []
                                , body =
                                    HVM.Match
                                        { value = rhsTerm
                                        , arms =
                                            [ ( atCorrectIndex
                                                    (tupleGetterPattern
                                                        elIndex
                                                        -- TODO this below should be childPattern, that we get
                                                        -- from calling `pattern` or something on `elPattern`
                                                        (HVM.PVar intrinsics.extractedVar)
                                                    )
                                              , HVM.Var intrinsics.extractedVar
                                              )
                                            ]
                                        }
                                }

                            _ ->
                                Debug.Extra.todo1 "constructorArgPatternToRules PTuple non-PVar" ( argIndex, argPattern )
                    )

        _ ->
            Debug.Extra.todo1 "constructorArgPatternToRules" ( argIndex, argPattern )


pattern : AST.Pattern -> HVM.Pattern
pattern p =
    case p of
        AST.PUnit ->
            HVM.PCtr intrinsics.unit []

        _ ->
            Debug.Extra.todo1 "codegen pattern" p


intrinsics :
    { unit : String
    , char : String
    , recordGetter : String -> String
    , tupleEnd : String
    , tupleEndType : String
    , extractedVar : String
    }
intrinsics =
    { unit = "Cara.unit"
    , char = "Cara.char"
    , recordGetter = \field -> "Cara.RecGet." ++ field
    , tupleEnd = "Cara.TupleEnd"
    , tupleEndType = "Cara.TupleEnd.T"
    , extractedVar =
        -- TODO we need to figure out a safe extracted var by looking at the compiled Program
        -- The hardcoded "x" won't be safe if the user defines a `x` variable
        "x"
    }


exprToTerm : AST.Expr -> HVM.Term
exprToTerm expr =
    case expr of
        AST.Int n ->
            HVM.U60 n

        AST.Float n ->
            -- TODO wait for when HVM-Lang supports F60 again
            Debug.Extra.todo1 "codegen exprToTerm" n

        AST.Char str ->
            -- We can't use HVM chars as Cara chars are really extended grapheme clusters
            app intrinsics.char [ HVM.Str str ]

        AST.String str ->
            HVM.Str str

        AST.Bool bool ->
            if bool then
                HVM.U60 0

            else
                HVM.U60 1

        AST.Unit ->
            HVM.Var intrinsics.unit

        AST.Tuple xs ->
            tuple (List.map exprToTerm xs)

        AST.List xs ->
            HVM.Lst (List.map exprToTerm xs)

        AST.FnCall1 { fn, arg } ->
            HVM.App
                { function = exprToTerm fn
                , args = [ exprToTerm arg ]
                }

        AST.Let1 { name, value, body } ->
            HVM.Let
                { pat = HVM.PVar name
                , value = exprToTerm value
                , next = exprToTerm body
                }

        AST.Constructor_ { id, args } ->
            app
                (idToString id)
                (List.map exprToTerm args)

        AST.RootIdentifier qid ->
            HVM.Var (idToString qid)

        AST.Lambda1 r ->
            Debug.Extra.todo1 "exprToTerm lambda1" r

        AST.If r ->
            -- TODO this one should be easy: Data.U60.if or our own variant of it
            Debug.Extra.todo1 "exprToTerm if-then-else" r

        AST.RecordGetter field ->
            let
                argName =
                    "r"
            in
            HVM.Lam
                { name = argName
                , body =
                    HVM.App
                        { function = HVM.Var (intrinsics.recordGetter field)
                        , args = [ HVM.Var argName ]
                        }
                }
