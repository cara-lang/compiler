module Codegen.HVM exposing (codegenProgram)

{-| This module deals with desugaring (lowering) from `AST.Backend`
to `HVM.AST`.
-}

import AST.Backend as AST
import Debug.Extra
import HVM.AST as HVM exposing (File, Pattern(..), Rule, Term(..))
import Id.Qualified exposing (QualifiedId)


codegenProgram : AST.Program -> HVM.File
codegenProgram program =
    program
        |> List.map declToFile
        |> List.foldr HVM.concatFiles HVM.emptyFile


todoRule : String -> a -> List Rule
todoRule message thing =
    [ { functionName = message
      , args = []
      , body = Str <| Debug.toString thing
      }
    ]


todoTerm : String -> Term
todoTerm message =
    app "Todo" [ Str message ]


app : String -> List Term -> Term
app name args =
    if List.isEmpty args then
        Var name

    else
        App
            { function = Var name
            , args = args
            }


{-| PERF: would it be better to compile tuples to `data` of the given arity rather than to nested HVM 2-tuples?
-}
tuple : List Term -> Term
tuple terms =
    case terms of
        [] ->
            Era

        fst :: rest ->
            {- TODO is foldl the right direction? I guess this only does
               (((a,b),c),d) vs (a,(b,(c,d))) and so it's fine either way, it
               just needs to be consistent with the unwrapping later?
            -}
            List.foldl
                (\new acc -> Tup ( acc, new ))
                fst
                rest


idToString : QualifiedId -> String
idToString id =
    if id.qualifiers == ( "<root>", [] ) then
        id.name

    else
        Id.Qualified.toString id


declToFile : AST.Decl -> File
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


exprToTerm : AST.Expr -> Term
exprToTerm expr =
    case expr of
        AST.Int n ->
            U60 n

        AST.Float _ ->
            -- TODO wait for when HVM-Lang supports F60 again
            Era

        AST.Char str ->
            -- We can't use HVM chars as Cara chars are really extended grapheme clusters
            app "Cara.char" [ Str str ]

        AST.String str ->
            Str str

        AST.Bool bool ->
            if bool then
                U60 0

            else
                U60 1

        AST.Unit ->
            Var "Cara.unit"

        AST.Tuple xs ->
            tuple (List.map exprToTerm xs)

        AST.List xs ->
            Lst (List.map exprToTerm xs)

        AST.FnCall1 { fn, arg } ->
            App
                { function = exprToTerm fn
                , args = [ exprToTerm arg ]
                }

        AST.Let1 { name, value, body } ->
            Let
                { pat = PVar name
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
