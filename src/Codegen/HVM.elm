module Codegen.HVM exposing (codegenProgram)

import AST.Backend as AST
import Desugar.CaseOf
import HVM.AST as HVM exposing (Rule, Term(..))
import Id.Qualified exposing (QualifiedId)


codegenProgram : AST.Program -> HVM.File
codegenProgram program =
    { rules = List.concatMap declToRules program }


todoRule : String -> a -> List Rule
todoRule message thing =
    [ { lhs = todoTerm message
      , rhs = Str <| Debug.toString thing
      }
    ]


todoTerm : String -> Term
todoTerm message =
    ctr "Todo" [ Str message ]


ctr : String -> List Term -> Term
ctr name args =
    Ctr { name = name, args = args }


tuple : List Term -> Term
tuple terms =
    let
        len =
            List.length terms
    in
    ctr
        ("Data.Cara.tuple" ++ String.fromInt len)
        terms


declToRules : AST.Decl -> List Rule
declToRules decl =
    case decl of
        AST.DType r ->
            {- For HVM, we don't need to create any constructor functions:
               we can pull constructors out of thin air.

               We will need a `Foo.match` function for the Scott encoding of
               `case..of` expressions though. (See Desugar.CaseOf)
            -}
            typeToMatchFnRules r

        AST.DLetStmt r ->
            todoRule "DLetStmt" r


typeToMatchFnRules :
    { id : QualifiedId
    , vars : List String
    , constructors : List AST.Constructor
    }
    -> List Rule
typeToMatchFnRules r =
    let
        -- My.Nested.Module.Foo.match
        fnName : String
        fnName =
            Desugar.CaseOf.matchFn r.id
                |> Id.Qualified.toString

        -- onBar
        toHandlerName : String -> String
        toHandlerName ctrName =
            "on" ++ ctrName

        handlerNames : List Term
        handlerNames =
            List.map (.id >> .name >> toHandlerName >> Var) r.constructors
    in
    List.map2
        (\ctr_ handlerName ->
            -- (Foo.match (Bar a0 a1) onBar onBaz onQuux) = (onBar a0 a1)
            let
                -- [a0, a1, a2]
                argNames =
                    List.range 0 (ctr_.argsCount - 1)
                        |> List.map (\i -> Var ("a" ++ String.fromInt i))
            in
            { lhs = ctr fnName (ctr ctr_.id.name argNames :: handlerNames)
            , rhs = functionTerm handlerName argNames
            }
        )
        r.constructors
        handlerNames


functionTerm : Term -> List Term -> Term
functionTerm function args =
    let
        oneArgApplication : Term -> Term -> Term
        oneArgApplication term fn =
            App
                { function = fn
                , arg = term
                }
    in
    List.foldl oneArgApplication function args


exprToTerm : AST.Expr -> Term
exprToTerm expr =
    case expr of
        AST.Int n ->
            U60 n

        AST.Float f ->
            F60 f

        AST.Char str ->
            ctr "Data.Cara.char" [ Str str ]

        AST.String str ->
            Str str

        AST.Bool bool ->
            if bool then
                U60 0

            else
                U60 1

        AST.Unit ->
            ctr "Data.Cara.unit" []

        AST.Tuple xs ->
            tuple (List.map exprToTerm xs)

        AST.List xs ->
            Lst (List.map exprToTerm xs)

        AST.FnCall1 { fn, arg } ->
            App
                { function = exprToTerm fn
                , arg = exprToTerm arg
                }

        AST.Let1 { name, value, body } ->
            Let
                { name = name
                , expr = exprToTerm value
                , body = exprToTerm body
                }

        AST.Constructor_ { id, args } ->
            ctr
                (Id.Qualified.toString id)
                (List.map exprToTerm args)

        AST.RootIdentifier qid ->
            todoTerm "exprToTerm rootIdentifier"

        AST.Lambda1 { arg, body } ->
            todoTerm "exprToTerm lambda"

        AST.If { cond, then_, else_ } ->
            ctr "Data.U60.if"
                (List.map exprToTerm
                    [ cond
                    , then_
                    , else_
                    ]
                )


patternToTerm : AST.Pattern -> Term
patternToTerm pattern =
    case pattern of
        AST.PUnit ->
            ctr "Data.Cara.unit" []

        AST.PVar var ->
            Var var

        AST.PConstructor { id, args } ->
            ctr
                (Id.Qualified.toString id)
                (List.map patternToTerm args)

        AST.PInt int ->
            U60 int

        AST.PFloat float ->
            F60 float

        AST.PChar c ->
            ctr "Data.Cara.char" [ Str c ]

        AST.PString s ->
            Str s

        AST.PAs name inner ->
            Let
                { name = name
                , expr = patternToTerm inner
                , body = patternToTerm inner
                }

        AST.PList ps ->
            Lst (List.map patternToTerm ps)

        AST.PTuple ps ->
            tuple (List.map patternToTerm ps)

        AST.PWildcard ->
            Var "_"

        AST.PSpread maybeVar ->
            Debug.todo "pattern to term - spread"

        AST.PRecordSpread ->
            Debug.todo "pattern to term - record spread"

        AST.PRecordFields recordFields ->
            Debug.todo "pattern to term - record fields"
