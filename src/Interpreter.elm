module Interpreter exposing
    ( Outcome(..)
    , interpret
    )

import AST
    exposing
        ( AST(..)
        , Bang(..)
        , Declaration(..)
        , Expr(..)
        , Pattern(..)
        , Statement(..)
        , Type(..)
        )
import Effect exposing (Effect)
import Env exposing (Env)
import Error exposing (InterpreterError(..))
import Id exposing (Id)
import Tree.Zipper as Zipper exposing (Zipper)
import Value exposing (Value(..))


type Outcome
    = DoneInterpreting Env (Zipper AST) Value
    | NeedsEffect Env (Zipper AST) Effect
    | FoundError InterpreterError


interpret : Env -> Zipper AST -> Outcome
interpret env ast =
    case Zipper.label ast of
        Program ->
            interpretProgram env ast

        Expr expr ->
            interpretExpr env ast expr

        Type type_ ->
            interpretType env ast type_

        Ptrn ptrn ->
            interpretPattern env ast ptrn

        Bang bang ->
            interpretBang env ast bang

        Stmt stmt ->
            interpretStatement env ast stmt

        Decl decl ->
            interpretDecl env ast decl


interpretExpr : Env -> Zipper AST -> Expr -> Outcome
interpretExpr env ast expr =
    case expr of
        Int n ->
            done env ast (VInt n)

        Float _ ->
            Debug.todo "branch 'Float _' not implemented"

        Char _ ->
            Debug.todo "branch 'Char _' not implemented"

        String _ ->
            Debug.todo "branch 'String _' not implemented"

        Bool _ ->
            Debug.todo "branch 'Bool _' not implemented"

        Unit ->
            Debug.todo "branch 'Unit' not implemented"

        Tuple ->
            Debug.todo "branch 'Tuple' not implemented"

        List ->
            Debug.todo "branch 'List' not implemented"

        Record ->
            Debug.todo "branch 'Record' not implemented"

        UnaryOp ->
            Debug.todo "branch 'UnaryOp' not implemented"

        BinaryOp ->
            Debug.todo "branch 'BinaryOp' not implemented"

        Call ->
            Debug.todo "branch 'Call' not implemented"

        RecordGet _ ->
            Debug.todo "branch 'RecordGet _' not implemented"

        Block ->
            Debug.todo "branch 'Block' not implemented"

        EffectBlock _ ->
            Debug.todo "branch 'EffectBlock _' not implemented"

        Constructor _ ->
            Debug.todo "branch 'Constructor _' not implemented"

        Identifier id ->
            interpretIdentifier id env ast

        RootIdentifier id ->
            interpretRootIdentifier id env ast

        Lambda ->
            Debug.todo "branch 'Lambda' not implemented"

        Closure _ ->
            Debug.todo "branch 'Closure _' not implemented"

        RecordGetter _ ->
            Debug.todo "branch 'RecordGetter _' not implemented"

        If ->
            Debug.todo "branch 'If' not implemented"

        Case ->
            Debug.todo "branch 'Case' not implemented"


interpretType : Env -> Zipper AST -> Type -> Outcome
interpretType env ast type_ =
    case type_ of
        UnitType ->
            Debug.todo "branch 'UnitType' not implemented"

        NamedType _ ->
            Debug.todo "branch 'NamedType _' not implemented"

        CallType ->
            Debug.todo "branch 'CallType' not implemented"

        VarType _ ->
            Debug.todo "branch 'VarType _' not implemented"

        FnType ->
            Debug.todo "branch 'FnType' not implemented"

        TupleType ->
            Debug.todo "branch 'TupleType' not implemented"

        RecordType ->
            Debug.todo "branch 'RecordType' not implemented"


interpretPattern : Env -> Zipper AST -> AST.Pattern -> Outcome
interpretPattern env ast pattern =
    case pattern of
        UnitPattern ->
            Debug.todo "branch 'UnitPattern' not implemented"

        VarPattern _ ->
            Debug.todo "branch 'VarPattern _' not implemented"

        ConstructorPattern _ ->
            Debug.todo "branch 'ConstructorPattern _' not implemented"

        IntPattern _ ->
            Debug.todo "branch 'IntPattern _' not implemented"

        FloatPattern _ ->
            Debug.todo "branch 'FloatPattern _' not implemented"

        ListPattern ->
            Debug.todo "branch 'ListPattern' not implemented"

        TuplePattern ->
            Debug.todo "branch 'TuplePattern' not implemented"

        WildcardPattern ->
            Debug.todo "branch 'WildcardPattern' not implemented"

        SpreadPattern _ ->
            Debug.todo "branch 'SpreadPattern _' not implemented"

        RecordSpreadPattern ->
            Debug.todo "branch 'RecordSpreadPattern' not implemented"

        RecordFieldsPattern _ ->
            Debug.todo "branch 'RecordFieldsPattern _' not implemented"


interpretBang : Env -> Zipper AST -> Bang -> Outcome
interpretBang env ast bang =
    case bang of
        ValueBang ->
            Debug.todo "branch 'ValueBang' not implemented"

        CallBang ->
            case Zipper.firstChild ast of
                Nothing ->
                    Debug.todo "call bang"

                Just firstChild ->
                    case Zipper.label firstChild of
                        Expr (Identifier id) ->
                            if Id.isIoPrintln id then
                                interpretPrintln env firstChild

                            else
                                Debug.todo "call bang - ID"

                        _ ->
                            Debug.todo "Unsupported AST node in the Fn position of a CallBang"


interpretStatement : Env -> Zipper AST -> AST.Statement -> Outcome
interpretStatement env ast stmt =
    case stmt of
        LetStmt { name } ->
            interpretLet name env ast

        LetBangStmt _ ->
            Debug.todo "branch 'LetBangStmt _' not implemented"

        BangStmt ->
            Debug.todo "branch 'BangStmt' not implemented"


interpretDecl : Env -> Zipper AST -> AST.Declaration -> Outcome
interpretDecl env ast decl =
    case decl of
        TypeAlias _ ->
            Debug.todo "branch 'TypeAlias _' not implemented"

        TypeDecl _ ->
            Debug.todo "branch 'TypeDecl _' not implemented"

        Module { name } ->
            interpretModule name env ast

        ExtendModule _ ->
            Debug.todo "branch 'ExtendModule _' not implemented"

        Function _ ->
            Debug.todo "branch 'Function _' not implemented"

        BinaryOperatorDecl ->
            Debug.todo "branch 'BinaryOperatorDecl' not implemented"

        UnaryOperatorDecl ->
            Debug.todo "branch 'UnaryOperatorDecl' not implemented"

        Statement ->
            Debug.todo "branch 'Statement' not implemented"

        ValueAnnotation _ ->
            Debug.todo "branch 'ValueAnnotation _' not implemented"

        FunctionAnnotation _ ->
            Debug.todo "branch 'FunctionAnnotation _' not implemented"


done : Env -> Zipper AST -> Value -> Outcome
done env ast value =
    case removeCurrent ast of
        Nothing ->
            DoneInterpreting env ast value

        Just newAst ->
            DoneInterpreting env newAst value


removeCurrent : Zipper AST -> Maybe (Zipper AST)
removeCurrent ast =
    Zipper.removeTree ast


interpretProgram : Env -> Zipper AST -> Outcome
interpretProgram env ast =
    case Zipper.firstChild ast of
        Nothing ->
            done env ast VUnit

        Just child ->
            case interpret env child of
                DoneInterpreting newEnv newAst _ ->
                    interpretProgram newEnv newAst

                other ->
                    other


interpretLet : String -> Env -> Zipper AST -> Outcome
interpretLet name env ast =
    case Zipper.firstChild ast of
        Nothing ->
            FoundError (ExpectedChildNode "let")

        Just child ->
            case interpret env child of
                DoneInterpreting newEnv newAst val ->
                    done (Env.add name val newEnv) newAst VUnit

                other ->
                    other


interpretModule : String -> Env -> Zipper AST -> Outcome
interpretModule name env ast =
    if Zipper.firstChild ast == Nothing then
        done env ast VUnit

    else
        let
            envWithOpenModule =
                env
                    |> Env.addModule name
                    |> Env.open [ name ]
        in
        case envWithOpenModule of
            Nothing ->
                -- Should never happen
                FoundError (ExpectedModule name)

            Just envWithOpenModule_ ->
                case interpretProgram envWithOpenModule_ ast of
                    DoneInterpreting envAfterChildren _ _ ->
                        done envAfterChildren ast VUnit

                    NeedsEffect newEnv newAst eff ->
                        case Zipper.parent newEnv of
                            Nothing ->
                                FoundError ExpectedParent

                            Just parentEnv ->
                                NeedsEffect parentEnv newAst eff

                    other ->
                        other


{-| Expects the `ast` to be on the first argument to the println.
-}
interpretPrintln : Env -> Zipper AST -> Outcome
interpretPrintln env ast =
    -- TODO removeCurrent?
    --NeedsEffect newEnv ast (Effect.Println (Value.toString val))
    --NeedsEffect newEnv newAst (Effect.Println (Value.toString val))
    Debug.todo "interpretPrintln"


interpretIdentifier : Id -> Env -> Zipper AST -> Outcome
interpretIdentifier id env ast =
    let
        go env_ =
            case env_ of
                Nothing ->
                    FoundError (VarNotFound id)

                Just env__ ->
                    case Env.get id env__ of
                        Nothing ->
                            go (Zipper.parent env__)

                        Just value ->
                            done env ast value
    in
    go (Just env)


interpretRootIdentifier : Id -> Env -> Zipper AST -> Outcome
interpretRootIdentifier id env ast =
    case Env.get id (Zipper.root env) of
        Nothing ->
            FoundError (RootVarNotFound id)

        Just value ->
            done env ast value
