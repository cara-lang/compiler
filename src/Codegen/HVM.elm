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
import Maybe.Extra
import Set


codegenProgram : AST.Program -> HVM.File
codegenProgram program =
    let
        maxTupleArity : Maybe Int
        maxTupleArity =
            program
                |> AST.programChildren
                |> List.filterMap AST.getTuple
                |> List.map List.length
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
                                |> List.map
                                    (\index ->
                                        tupleGetterDef index (Common.tupleIndexToNumericField index)
                                    )
                        }
                    )

        recordsFile : List HVM.File
        recordsFile =
            program
                |> AST.programChildren
                |> List.filterMap AST.getRecord
                |> List.map
                    (\{ sortedFields } ->
                        let
                            recordName =
                                intrinsics.record { sortedFields = List.map .field sortedFields }

                            fieldsCount =
                                List.length sortedFields
                        in
                        { adts =
                            [ { name = adtName recordName
                              , constructors =
                                    [ { name = recordName
                                      , arity = fieldsCount
                                      }
                                    ]
                              }
                            ]
                        , rules =
                            -- TODO: only generate those that are used (found across the program)?
                            sortedFields
                                |> List.indexedMap
                                    (\i { field } ->
                                        --
                                        {-
                                           (Cara.RecGet.foo (Cara.Rec.bar.foo.quux * x *)) = x
                                                                                     ^ (i: 1)
                                           before: 1
                                           after: 1
                                        -}
                                        let
                                            before =
                                                i

                                            after =
                                                fieldsCount - i - 1
                                        in
                                        { functionName = intrinsics.recordGetter field
                                        , args =
                                            [ HVM.PCtr recordName
                                                (List.concat
                                                    [ List.repeat before HVM.PWildcard
                                                    , [ HVM.PVar intrinsics.extractedVar ]
                                                    , List.repeat after HVM.PWildcard
                                                    ]
                                                )
                                            ]
                                        , body = HVM.Var intrinsics.extractedVar
                                        }
                                    )
                        }
                    )

        initFile : HVM.File
        initFile =
            [ tupleGettersFile |> Maybe.Extra.toList
            , recordsFile
            ]
                |> List.concat
                |> List.foldr HVM.concatFiles preambleFile
    in
    program
        |> List.map declToFile
        |> List.foldr HVM.concatFiles initFile


preambleFile : HVM.File
preambleFile =
    { adts =
        [ { name = adtName intrinsics.tupleEnd
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


app : String -> List HVM.Term -> HVM.Term
app name args =
    if List.isEmpty args then
        HVM.Var name

    else
        HVM.App
            { function = HVM.Var name
            , args = args
            }


{-|

     (1,2,3,4)
     -->
     (1,(2,(3,(4,Cara.TupleEnd))))

Needs an ADT with the `Cara.TupleEnd` constructor to be generated elsewhere (see `preambleFile`).
Needs tuple getters to be generated as well (see `tupleGettersFile`).

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
            , rules = toplevelPatternToRules r
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


toplevelPatternToRules : { lhs : AST.Pattern, expr : AST.Expr } -> List HVM.Rule
toplevelPatternToRules r =
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
                    (toplevelConstructorArgPatternToRules
                        (idToString ctr.id)
                        rhsTerm
                        (List.length ctr.args)
                    )
                |> List.concat

        AST.PRecordFields fields ->
            fields
                |> List.map
                    (\field ->
                        { functionName = field
                        , args = []
                        , body =
                            HVM.App
                                { function = HVM.Var (intrinsics.recordGetter field)
                                , args = [ rhsTerm ]
                                }
                        }
                    )

        _ ->
            Debug.Extra.todo1 "Codegen.HVM.patternToRules unhandled DLetStmt" r


extractedValues : AST.Pattern -> List ( String, HVM.Pattern )
extractedValues p =
    case p of
        {-
           PWildcard simply doesn't result in a rule being emitted!

           Bar(a,_,c) = someBar
           --->
           a = match someBar { ... }
           c = match someBar { ... }
        -}
        AST.PWildcard ->
            []

        {-
           PUnit similarly to PWildcard doesn't result in a rule being emitted!

           It gets typechecked _before_ the codegen phase, and can result in a type error.

           Bar(a,(),c) = someBar
           --->
           a = match someBar { ... }
           c = match someBar { ... }
        -}
        AST.PUnit ->
            []

        {-
           Bar(a) = someBar
           -->
           a = match someFoo {
               (Bar x): x
           }
        -}
        AST.PVar name ->
            [ ( name, HVM.PVar intrinsics.extractedVar ) ]

        {-
           Bar((a,b,c,d)) = someBar
           -->
           a = match someBar { (Bar (x,*)): x }
           b = match someBar { (Bar (*,(x,*))): x }
           c = match someBar { (Bar (*,(*,(x,*)))): x }
           d = match someBar { (Bar (*,(*,(*,(x,*))))): x }
        -}
        AST.PTuple xs ->
            xs
                |> List.indexedMap
                    (\i x ->
                        extractedValues x
                            |> List.map (\( n, p2 ) -> ( n, tupleGetterPattern i p2 ))
                    )
                |> List.concat

        _ ->
            Debug.Extra.todo1 "extractedValues" p


toplevelConstructorArgPatternToRules : String -> HVM.Term -> Int -> Int -> AST.Pattern -> List HVM.Rule
toplevelConstructorArgPatternToRules ctrId rhsTerm allArgs argIndex argPattern =
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
    extractedValues argPattern
        |> List.map
            (\( name, extractedValue ) ->
                { functionName = name
                , args = []
                , body =
                    HVM.Match
                        { value = rhsTerm
                        , arms =
                            [ ( atCorrectIndex extractedValue
                              , HVM.Var intrinsics.extractedVar
                              )
                            ]
                        }
                }
            )


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
    , record : { sortedFields : List String } -> String
    , recordGetter : String -> String
    , tupleEnd : String
    , extractedVar : String
    }
intrinsics =
    { unit = "Cara.unit"
    , char = "Cara.char"
    , record = \{ sortedFields } -> "Cara.Rec." ++ String.join "." sortedFields
    , recordGetter = \field -> "Cara.RecGet." ++ field
    , tupleEnd = "Cara.TupleEnd"
    , extractedVar =
        {-
           When converting patterns to top-level rules, we're guaranteed we're
           only dealing with a single part of the overall pattern.

           Thus we only need a single `x` for the `match term { (... x ...): x }`,
           the rest are going to be `*`s.

           TODO we need to figure out a safe string by looking at the compiled Program.
           The hardcoded "x" won't be safe if the user defines a `x` variable.
        -}
        "x"
    }


{-| Cara.TupleEnd
-->
Cara.TupleEnd.T

Cara.Rec.foo.bar
-->
Cara.Rec.foo.bar.T

-}
adtName : String -> String
adtName s =
    s ++ ".T"


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

        {-
           .foo
           -->
           Cara.RecGet.foo
        -}
        AST.RecordGetter field ->
            HVM.Var (intrinsics.recordGetter field)

        {-
           {a:1, b:2}
           -->
           (Cara.Rec.a.b 1 2)

           Needs an ADT definition and field getters to be generated elsewhere (see `recordsFile`).
        -}
        AST.Record { sortedFields } ->
            HVM.App
                { function = HVM.Var (intrinsics.record { sortedFields = List.map .field sortedFields })
                , args =
                    sortedFields
                        |> List.map (.expr >> exprToTerm)
                }
