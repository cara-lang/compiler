module Codegen.HVM exposing (codegenProgram)

{-| This module deals with desugaring (lowering) from `AST.Backend`
to `HVM.AST`.
-}

import AST.Backend as AST
import Debug.Extra
import HVM.AST as HVM
import Id.Qualified exposing (QualifiedId)


codegenProgram : AST.Program -> HVM.File
codegenProgram program =
    program
        |> List.map declToFile
        |> List.foldr HVM.concatFiles HVM.emptyFile


todoRule : String -> a -> List HVM.Rule
todoRule message thing =
    [ { functionName = message
      , args = []
      , body = HVM.Str <| Debug.toString thing
      }
    ]


todoTerm : String -> HVM.Term
todoTerm message =
    app "Todo" [ HVM.Str message ]


app : String -> List HVM.Term -> HVM.Term
app name args =
    if List.isEmpty args then
        HVM.Var name

    else
        HVM.App
            { function = HVM.Var name
            , args = args
            }


{-| PERF: would it be better to compile tuples to `data` of the given arity rather than to nested HVM 2-tuples?
-}
tuple : List HVM.Term -> HVM.Term
tuple terms =
    case terms of
        [] ->
            HVM.Era

        fst :: rest ->
            {- TODO make sure we're unwrapping tuples in the same direction:
               (((a,b),c),d)
            -}
            List.foldl
                (\new acc -> HVM.Tup ( acc, new ))
                fst
                rest


idToString : QualifiedId -> String
idToString id =
    if id.qualifiers == ( "<root>", [] ) then
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
                [ case r.lhs of
                    AST.PVar name ->
                        { functionName = name
                        , args = []
                        , body = exprToTerm r.expr
                        }

                    _ ->
                        Debug.Extra.todo1 "declToFile DLetStmt - non-PVar" r
                ]
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
    }
intrinsics =
    { unit = "Cara.unit"
    , char = "Cara.char"
    , recordGetter = \field -> "Cara.Get." ++ field
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
            todoTerm "exprToTerm rootIdentifier"

        AST.Lambda1 { arg, body } ->
            todoTerm "exprToTerm lambda"

        AST.If { cond, then_, else_ } ->
            -- TODO Is there going to be Data.U60.if or do we have to create it ourselves?
            todoTerm "exprToTerm if-then-else"

        AST.RecordGetter field ->
            HVM.Lam (intrinsics.recordGetter field) TODO
