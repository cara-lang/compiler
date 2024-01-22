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

        initFile : HVM.File
        initFile =
            case maxTupleArity of
                Nothing ->
                    HVM.emptyFile

                Just maxArity ->
                    { adts = []
                    , rules =
                        List.range 0 (maxArity - 1)
                            |> List.concatMap
                                (\index ->
                                    [ [ tupleGetterDef index (Common.tupleIndexToNumericField index) ]
                                    , BiDict.getReverse index Common.namedTupleFields
                                        |> Set.toList
                                        |> List.map (tupleGetterDef index)
                                    ]
                                        |> List.concat
                                )
                    }
    in
    program
        |> List.map declToFile
        |> List.foldr HVM.concatFiles initFile


extractedVar : String
extractedVar =
    "a"


{-|

    (Cara.GetRec.el1    (a,*))     = a
    (Cara.GetRec.first  (a,*))     = a
    (Cara.GetRec.el2    (*,(a,*))) = a
    (Cara.GetRec.second (*,(a,*))) = a

-}
tupleGetterDef : Int -> String -> HVM.Rule
tupleGetterDef index field =
    { functionName = intrinsics.recordGetter field
    , args = [ tupleGetterPattern index ]
    , body = HVM.Var extractedVar
    }


{-| index 0: (a,_)
PTup (PVar "a", PWildcard)
index 1: (_,(a,_))
PTup (PWildcard, PTup (PVar "a", PWildcard))
index 2: (_,(_,(a,_)))
PTup (PWildcard, PTup (PWildcard, PTup (PVar "a", PWildcard)))
-}
tupleGetterPattern : Int -> HVM.Pattern
tupleGetterPattern index =
    let
        wrap : HVM.Pattern -> HVM.Pattern
        wrap p =
            HVM.PTup ( HVM.PWildcard, p )
    in
    Basics.Extra.doNTimes
        index
        wrap
        (HVM.PTup ( HVM.PVar extractedVar, HVM.PWildcard ))


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


{-| This should convert `(1,2,3,4)` into `(1,(2,(3,(4,Cara.tupleEnd))))`

PERF: would it be better to compile tuples to `data` of the given arity rather
than to nested HVM 2-tuples?

-}
tuple : List HVM.Term -> HVM.Term
tuple terms =
    List.foldr
        (\new acc -> HVM.Tup ( new, acc ))
        (HVM.Var intrinsics.tupleEnd)
        (List.reverse terms)


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
            , rules =
                case r.lhs of
                    AST.PVar name ->
                        [ { functionName = name
                          , args = []
                          , body = exprToTerm r.expr
                          }
                        ]

                    AST.PConstructor ctr ->
                        {- Here we blindly assume the constructor is a singleton
                           one (type Foo = Bar(Int)) and so it should allow
                           the `Bar(a,b) = someFoo` destructuring.

                           Desugar phase should have caught this if it's not.
                           If it's not, the HVM file will not compile as HVM
                           also does exhaustive checking.

                           TODO do something general for the patterns inside the constructor, like:

                               patternsToRules
                                   { patterns = ctr.args
                                   , ctrId = ctr.id
                                   , value = r.expr
                                   }
                        -}
                        case ctr.args of
                            [ AST.PVar name ] ->
                                {-
                                   Bar(x) = someFoo
                                   -->
                                   x = match someFoo {
                                       (Bar a): a
                                   }
                                -}
                                [ { functionName = name
                                  , args = []
                                  , body =
                                        HVM.Match
                                            { value = exprToTerm r.expr
                                            , arms =
                                                [ ( HVM.PCtr
                                                        (idToString ctr.id)
                                                        [ HVM.PVar extractedVar ]
                                                  , HVM.Var extractedVar
                                                  )
                                                ]
                                            }
                                  }
                                ]

                            {-
                               B1((b2a,b2b,b2c,b2d)) = b2
                               -->
                               b2a = match b2 { (B1 (a,*)): a }
                               b2b = match b2 { (B1 (*,(a,*))): a }
                               b2c = match b2 { (B1 (*,(*,(a,*)))): a }
                               b2d = match b2 { (B1 (*,(*,(*,(a,*))))): a }

                               TODO this needs generalizing with the above - some kind of recursive function
                            -}
                            [ AST.PTuple ps ] ->
                                ps
                                    |> List.indexedMap
                                        (\index p ->
                                            case p of
                                                AST.PVar name ->
                                                    { functionName = name
                                                    , args = []
                                                    , body =
                                                        HVM.Match
                                                            { value = exprToTerm r.expr
                                                            , arms =
                                                                [ ( HVM.PCtr
                                                                        (idToString ctr.id)
                                                                        [ {- TODO this is the part that differs from above - guess if we want to generalize
                                                                             this, we need to hold this path (PVar vs PTuple>PVar)?
                                                                          -}
                                                                          tupleGetterPattern index
                                                                        ]
                                                                  , HVM.Var extractedVar
                                                                  )
                                                                ]
                                                            }
                                                    }

                                                _ ->
                                                    Debug.Extra.todo1 "Codegen.HVM.declToFile DLetStmt PConstructor PTuple non-PVar" ( ctr, p )
                                        )

                            _ ->
                                Debug.Extra.todo1 "Codegen.HVM.declToFile DLetStmt PConstructor" ctr

                    _ ->
                        Debug.Extra.todo1 "Codegen.HVM.declToFile unhandled DLetStmt" r
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
    }
intrinsics =
    { unit = "Cara.unit"
    , char = "Cara.char"
    , recordGetter = \field -> "Cara.RecGet." ++ field
    , tupleEnd = "Cara.tupleEnd"
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
                    "record"
            in
            HVM.Lam
                { name = argName
                , body =
                    HVM.App
                        { function = HVM.Var (intrinsics.recordGetter field)
                        , args = [ HVM.Var argName ]
                        }
                }
