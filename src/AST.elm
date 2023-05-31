module AST exposing
    ( AST(..)
    , Bang(..)
    , Declaration(..)
    , Expr(..)
    , Pattern(..)
    , Statement(..)
    , Type(..)
    , toString
    )

import Env exposing (Env)
import Id exposing (Id)
import String.Extra as String
import Tree
import Tree.Zipper as Zipper exposing (Zipper)


type AST
    = Expr Expr
    | Type Type
    | Ptrn Pattern
    | Bang Bang
    | Stmt Statement
    | Decl Declaration
    | Program


type Expr
    = -- Literals
      Int Int
    | Float Float
    | Char String -- Holding a string because of extended grapheme clusters
    | String String
    | Bool Bool -- TODO define this as Constructor instead?
    | Unit
      -- Collections
    | Tuple -- (1,True). children: [...ElementExprs]
    | List -- [1,2]. children: [...ElementExprs]
    | Record -- {a:1, ...b, c:True}. children: [...RecordContentExprs]
      -- Calls
    | UnaryOp -- ~a. children: [OpVar, ArgExpr]
    | BinaryOp -- a+b. children: [LeftExpr, OpVar, RightExpr]
    | Call -- foo(), bar(1,2). children: [Fn, ...ArgExprs]
    | RecordGet { field : String } -- record.field . children: [Record]
      -- Blocks
    | Block -- x = { ... }. children: [Stmts, ?Expr]
    | EffectBlock { monadModule : Id } -- x = My.Monad { ... }. children: like Block has
      -- Other
    | Constructor Id -- Foo, Bar.Foo, Foo(1,2), Bar.Foo(1,2). children: args of the constructor
    | Identifier Id -- foo, Bar.foo
    | RootIdentifier Id -- ::foo, ::Bar.foo
    | Lambda -- children: [...ArgPatterns, Body]
    | Closure Env -- children: same as Lambda
    | RecordGetter String
    | If -- children: [Cond, Then, Else]
    | Case -- children: [Subject, ...CaseBranches]


type Type
    = NamedType Id -- Int, Base.Maybe
    | CallType -- List[a]. children: [FnType, ...ArgTypes]
    | VarType String -- a
    | FnType -- x -> y. children: [FromType, ToType]
    | TupleType -- (Int, Bool). children: [...ElementTypes]
    | RecordType -- {a:Int,b:Bool}. children: [...RecordFieldTypes]
    | UnitType -- ()


type Pattern
    = -- TODO other patterns
      UnitPattern -- ()
    | VarPattern String -- a
    | ConstructorPattern Id -- Foo, Bar.Foo, Foo(a), Foo(_), Foo([]). children: [...ArgPatterns]
    | IntPattern Int -- 1
    | FloatPattern Float -- 1.2345
    | ListPattern -- [1,a]. children: [...ElementPatterns]
    | TuplePattern -- (1,a). children: [...ElementPatterns]
    | WildcardPattern -- _
    | SpreadPattern (Maybe String) -- ...a, ..._
    | RecordSpreadPattern -- {..}
    | RecordFieldsPattern (List String) -- {a}, {a,b}


type Bang
    = ValueBang -- foo!, Bar.foo!, x |> foo!, foo.bar!. children: [Expr]
    | CallBang -- foo!(1,2), Bar.foo!(1,2), x |> foo!(1,2), foo.bar!(1,2). children: [FnExpr, ...ArgExprs]


type Statement
    = -- TODO other stmts
      -- TODO modifiers, types, lhs is Pattern instead of just a name
      LetStmt { name : String } -- children: [Expr]
    | LetBangStmt { name : String } -- children: [Bang]
    | BangStmt -- children: Bang


type Declaration
    = TypeAlias String -- TODO mod, vars, body. TODO children
    | TypeDecl String --  TODO mod, vars, constructors. TODO children
    | Module { name : String } -- TODO mod. children: [...Decls]
    | ExtendModule Id -- children: [...Decls]
    | Function String -- TODO mod, args, resultType, body. TODO children
    | BinaryOperatorDecl -- TODO mod, op, left, right, resultType, body. TODO children
    | UnaryOperatorDecl -- TODO mod, op, arg, resultType, body. TODO children
    | Statement -- children: [Stmt]
    | ValueAnnotation String -- children: [Type]
    | FunctionAnnotation String -- TODO mod, args, resultType. TODO children


toString : Zipper AST -> String
toString ast =
    ast
        |> Zipper.toTree
        |> Tree.restructure
            astToString
            (\self children ->
                """{SELF}
{CHILDREN}"""
                    |> String.replace "{SELF}" self
                    |> String.replace "{CHILDREN}"
                        (String.join "\n" children
                            |> String.trimRight
                            |> String.indent
                        )
            )


astToString : AST -> String
astToString ast =
    case ast of
        Program ->
            "Program"

        Expr expr ->
            "Expr: " ++ exprToString expr

        Type type_ ->
            "Type: " ++ typeToString type_

        Ptrn pattern ->
            "Ptrn: " ++ patternToString pattern

        Bang bang ->
            "Bang: " ++ bangToString bang

        Stmt stmt ->
            "Stmt: " ++ stmtToString stmt

        Decl decl ->
            "Decl: " ++ declToString decl


exprToString : Expr -> String
exprToString expr =
    case expr of
        Int n ->
            "Int {N}"
                |> String.replace "{N}" (String.fromInt n)

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

        Identifier _ ->
            Debug.todo "branch 'Identifier _' not implemented"

        RootIdentifier _ ->
            Debug.todo "branch 'RootIdentifier _' not implemented"

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


typeToString : Type -> String
typeToString type_ =
    case type_ of
        UnitType ->
            "()"

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


patternToString : Pattern -> String
patternToString ptrn =
    case ptrn of
        IntPattern n ->
            "Int {N}"
                |> String.replace "{N}" (String.fromInt n)

        UnitPattern ->
            Debug.todo "branch 'UnitPattern' not implemented"

        VarPattern _ ->
            Debug.todo "branch 'VarPattern _' not implemented"

        ConstructorPattern _ ->
            Debug.todo "branch 'ConstructorPattern _' not implemented"

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


bangToString : Bang -> String
bangToString bang =
    case bang of
        ValueBang ->
            "Value"

        CallBang ->
            Debug.todo "branch 'CallBang' not implemented"


stmtToString : Statement -> String
stmtToString stmt =
    case stmt of
        BangStmt ->
            "Bang"

        LetStmt _ ->
            Debug.todo "branch 'LetStmt _' not implemented"

        LetBangStmt _ ->
            Debug.todo "branch 'LetBangStmt _' not implemented"


declToString : Declaration -> String
declToString decl =
    case decl of
        TypeAlias name ->
            "Type alias {NAME}"
                |> String.replace "{NAME}" name

        TypeDecl _ ->
            Debug.todo "branch 'TypeDecl _' not implemented"

        Module _ ->
            Debug.todo "branch 'Module _' not implemented"

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
