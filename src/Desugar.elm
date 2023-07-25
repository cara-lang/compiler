module Desugar exposing (desugarProgram)

import AST.Backend as B
import AST.Frontend as F
import Error exposing (DesugarError)
import Result.Extra as Result


desugarProgram : F.Program -> Result DesugarError B.Program
desugarProgram decls =
    decls
        |> Result.combineMap desugarDecl
        |> Result.map List.concat


desugarDecl : F.Decl -> Result DesugarError (List B.Decl)
desugarDecl decl =
    case decl of
        F.DTypeAlias r ->
            Debug.todo "desugar type alias"

        F.DType r ->
            Debug.todo "desugar type"

        F.DModule r ->
            Debug.todo "desugar module"

        F.DExtendModule r ->
            Debug.todo "desugar extend module"

        F.DStatement stmt ->
            desugarStmt stmt

        F.DUnitTest r ->
            Debug.todo "desugar unit test"

        F.DParameterizedTest r ->
            Debug.todo "desugar parameterized test"

        F.DPropertyTypeTest r ->
            Debug.todo "desugar property type test"

        F.DPropertyGenTest r ->
            Debug.todo "desugar property gen test"


desugarStmt : F.Stmt -> Result DesugarError (List B.Decl)
desugarStmt stmt =
    case stmt of
        F.SLet r ->
            Debug.todo "desugar slet"

        F.SLetBang r ->
            Debug.todo "desugar sletbang"

        F.SBang bang ->
            desugarBang bang

        F.SFunctionDef r ->
            Debug.todo "desugar sfunctiondef"

        F.SBinaryOperatorDef r ->
            Debug.todo "desugar sbinaryoperatordef"

        F.SUnaryOperatorDef r ->
            Debug.todo "desugar sunaryoperatordef"

        F.SValueAnnotation r ->
            Debug.todo "desugar svalueannotation"

        F.SBinaryOperatorAnnotation r ->
            Debug.todo "desugar sbinaryoperatorannotation"

        F.SUnaryOperatorAnnotation r ->
            Debug.todo "desugar sunaryoperatorannotation"

        F.SUseModule r ->
            Debug.todo "desugar susemodule"


desugarBang : F.Bang -> Result DesugarError (List B.Decl)
desugarBang bang =
    case bang of
        F.BValue expr ->
            Debug.todo "desugar bang value"

        F.BCall { fn, args } ->
            Debug.todo "desugar bang call"
