module AST exposing
    ( Bang(..)
    , BangOrExpr(..)
    , BinaryOp(..)
    , CaseBranch
    , Constructor
    , ConstructorArg
    , Decl(..)
    , Expr(..)
    , LetModifier(..)
    , ModuleModifier(..)
    , Pattern(..)
    , Program
    , RecordExprContent(..)
    , RecordTypeField
    , Stmt(..)
    , Type(..)
    , TypeAliasModifier(..)
    , TypeModifier(..)
    , UnaryOp(..)
    , binaryOp
    , isEffectfulStmt
    , isSpreadPattern
    , lambdaToString
    , patternToString
    , unaryOp
    )

import Env exposing (Env)
import Id exposing (Id)
import Intrinsic exposing (Intrinsic)


type alias Program =
    List Decl


type Expr
    = -- Literals
      Int Int
    | Float Float
    | Char String -- Holding a string because of extended grapheme clusters
    | String String
    | Bool Bool -- TODO define this as Constructor instead?
    | Unit
      -- Collections
    | Tuple (List Expr) -- (1,True)
    | List (List Expr) -- [1,2]
    | Record (List RecordExprContent) -- {a:1, ...b, c:True}
      -- Calls
    | UnaryOp UnaryOp Expr -- ~a
    | BinaryOp Expr BinaryOp Expr -- a+b
    | Call { fn : Expr, args : List Expr } -- foo(), bar(1,2)
    | RecordGet { record : Expr, field : String } -- record.field
      -- Blocks
    | Block { stmts : List Stmt, ret : Expr } -- x = { ... }
    | EffectBlock { monadModule : List String, stmts : List Stmt, ret : BangOrExpr } -- x = My.Monad { ... }
      -- Other
    | Constructor_ { id : Id, args : List Expr } -- Foo, Bar.Foo, Foo(1,2), Bar.Foo(1,2)
    | Identifier Id -- foo, Bar.foo
    | RootIdentifier Id -- ::foo, ::Bar.foo
    | Lambda { args : List Pattern, body : Expr }
    | RecordGetter String -- .field
    | If { cond : Expr, then_ : Expr, else_ : Expr }
    | Case { subject : Expr, branches : List CaseBranch }


type Type
    = TNamed Id -- Int, Base.Maybe
    | TApplication { type_ : Type, args : List Type } -- List[a]
    | TVar String -- a
    | TFn { from : Type, to : Type } -- x -> y
    | TThunk { to : Type } -- -> x
    | TTuple (List Type) -- (Int, Bool)
    | TRecord (List RecordTypeField) -- {a:Int,b:Bool}
    | TUnit -- ()


type Pattern
    = -- TODO what about the "as" renaming pattern?
      -- TODO PChar
      -- TODO PString
      PUnit -- ()
    | PVar String -- a
    | PConstructor { id : Id, args : List Pattern } -- Foo, Bar.Foo, Foo(a), Foo(_), Foo([])
    | PInt Int -- 1
    | PFloat Float -- 1.2345
    | PList (List Pattern) -- [1,a]
    | PTuple (List Pattern) -- (1,a)
    | PWildcard -- _
    | PSpread (Maybe String) -- ...a, ..._
    | PRecordSpread -- {..}
    | PRecordFields (List String) -- {a}, {a,b}
      {- These aren't ever parsed, but they're a way for interpretEffectBlock
         to create an expression out of SUnaryOperatorDef and
         SBinaryOperatorDef statements.
      -}
    | PUnaryOpDef UnaryOp
    | PBinaryOpDef BinaryOp


type Bang
    = BValue Expr -- foo!, Bar.foo!, x |> foo!, foo.bar!
    | BCall { fn : Expr, args : List Expr } -- foo!(1,2), Bar.foo!(1,2), x |> foo!(1,2), foo.bar!(1,2)


type BangOrExpr
    = B Bang
    | E Expr


type Stmt
    = SLet { mod : LetModifier, lhs : Pattern, type_ : Maybe Type, expr : Expr }
    | SLetBang { mod : LetModifier, lhs : Pattern, type_ : Maybe Type, bang : Bang }
    | SBang Bang
    | SFunctionDef { name : String, args : List Pattern, body : Expr }
    | SBinaryOperatorDef { op : BinaryOp, left : Pattern, right : Pattern, body : Expr }
    | SUnaryOperatorDef { op : UnaryOp, arg : Pattern, body : Expr }
    | SValueAnnotation { mod : LetModifier, name : String, type_ : Type }
    | SBinaryOperatorAnnotation { mod : LetModifier, op : BinaryOp, left : Type, right : Type, ret : Type }
    | SUnaryOperatorAnnotation { mod : LetModifier, op : UnaryOp, arg : Type, ret : Type }
    | SUseModule Id


type TypeModifier
    = TypeNoModifier
    | TypePrivate
    | TypeOpaque


type TypeAliasModifier
    = TypeAliasNoModifier
    | TypeAliasPrivate


type LetModifier
    = LetNoModifier
    | LetPrivate


type Decl
    = DTypeAlias { mod : TypeAliasModifier, name : String, vars : List String, body : Type }
    | DType { mod : TypeModifier, name : String, vars : List String, constructors : List Constructor }
    | DModule { mod : ModuleModifier, name : String, decls : List Decl }
    | DExtendModule { module_ : List String, decls : List Decl }
    | DStatement Stmt
    | DUnitTest { name : Maybe String, expr : Expr }
    | DParameterizedTest
        { name : Maybe String
        , table : List Expr
        , -- implicit lambda
          args : List Pattern
        , expr : Expr
        }
    | DPropertyTypeTest
        { name : Maybe String
        , types : Type
        , -- implicit lambda
          args : List Pattern
        , expr : Expr
        }
    | DPropertyGenTest
        { name : Maybe String
        , gens : Expr
        , -- implicit lambda
          args : List Pattern
        , expr : Expr
        }


type ModuleModifier
    = ModuleNoModifier
    | ModulePrivate


type alias CaseBranch =
    -- 1 -> "hello", 1 | 2 -> "hello"
    { orPatterns : List Pattern
    , body : Expr
    }


type RecordExprContent
    = Field { field : String, expr : Expr } -- a:123
    | Pun String -- a
    | Spread Id -- ...b


type alias RecordTypeField =
    -- a:Int
    { field : String
    , type_ : Type
    }


type alias Constructor =
    -- Foo
    -- Bar(Int)
    -- Baz(n: Int, verbose: Bool)
    { name : String
    , args : List ConstructorArg
    }


type alias ConstructorArg =
    -- a:Int
    -- Int
    { name : Maybe String
    , type_ : Type
    }


type UnaryOp
    = NegateNum -- -e
    | NegateBool -- !e
    | NegateBin -- ~e
    | InfiniteRange -- e..


type BinaryOp
    = -- arithmetic
      Plus -- +
    | Minus -- -
    | Times -- *
    | Div -- /
    | Mod -- %
    | Pow -- **
      -- binary
    | OrBin -- |
    | AndBin -- &
    | XorBin -- ^
    | ShiftL -- <<
    | ShiftR -- >>
    | ShiftRU -- >>>, unsigned
      -- comparisons and booleans
    | Lte -- <=
    | Lt -- <
    | Eq -- ==
    | Neq -- !=
    | Gt -- >
    | Gte -- >=
    | OrBool -- ||
    | AndBool -- &&
      -- appendables
    | Append -- ++
      -- ranges
    | RangeInclusive -- ..
    | RangeExclusive -- ...


isEffectfulStmt : Stmt -> Bool
isEffectfulStmt stmt =
    case stmt of
        SLetBang _ ->
            True

        SBang _ ->
            True

        --
        SLet _ ->
            False

        SFunctionDef _ ->
            False

        SBinaryOperatorDef _ ->
            False

        SUnaryOperatorDef _ ->
            False

        SValueAnnotation _ ->
            False

        SBinaryOperatorAnnotation _ ->
            False

        SUnaryOperatorAnnotation _ ->
            False

        SUseModule _ ->
            False


isSpreadPattern : Pattern -> Bool
isSpreadPattern pattern =
    case pattern of
        PSpread _ ->
            True

        PUnit ->
            False

        PVar _ ->
            False

        PConstructor _ ->
            False

        PInt _ ->
            False

        PFloat _ ->
            False

        PList _ ->
            False

        PTuple _ ->
            False

        PWildcard ->
            False

        PRecordSpread ->
            False

        PRecordFields _ ->
            False

        PUnaryOpDef _ ->
            False

        PBinaryOpDef _ ->
            False


binaryOp : BinaryOp -> String
binaryOp op =
    case op of
        Plus ->
            "+"

        Minus ->
            "-"

        Times ->
            "*"

        Div ->
            "/"

        Mod ->
            "%"

        Pow ->
            "**"

        OrBin ->
            "|"

        AndBin ->
            "&"

        XorBin ->
            "^"

        ShiftL ->
            "<<"

        ShiftR ->
            ">>"

        ShiftRU ->
            ">>>"

        Lte ->
            "<="

        Lt ->
            "<"

        Eq ->
            "=="

        Neq ->
            "!="

        Gt ->
            ">"

        Gte ->
            ">="

        OrBool ->
            "||"

        AndBool ->
            "&&"

        Append ->
            "++"

        RangeInclusive ->
            ".."

        RangeExclusive ->
            "..."


unaryOp : UnaryOp -> String
unaryOp op =
    case op of
        NegateNum ->
            "-"

        NegateBool ->
            "!"

        NegateBin ->
            "~"

        InfiniteRange ->
            ".."


lambdaToString : { args : List Pattern, body : Expr } -> String
lambdaToString { args, body } =
    "\\{ARGS} -> {BODY}"
        |> String.replace "{ARGS}" (String.join "," (List.map patternToString args))
        |> String.replace "{BODY}" (exprToString body)


patternToString : Pattern -> String
patternToString pattern =
    case pattern of
        PUnit ->
            "()"

        PVar name ->
            name

        PConstructor { id, args } ->
            Id.toString id
                ++ (if List.isEmpty args then
                        ""

                    else
                        "("
                            ++ String.join "," (List.map patternToString args)
                            ++ ")"
                   )

        PInt n ->
            String.fromInt n

        PFloat n ->
            String.fromFloat n

        PList xs ->
            "[" ++ String.join "," (List.map patternToString xs) ++ "]"

        PTuple xs ->
            "(" ++ String.join "," (List.map patternToString xs) ++ ")"

        PWildcard ->
            "_"

        PSpread val ->
            "..." ++ Maybe.withDefault "_" val

        PRecordSpread ->
            "{..}"

        PRecordFields fields ->
            "{" ++ String.join "," fields ++ "}"

        PUnaryOpDef _ ->
            "<UNARY OP DEF>"

        PBinaryOpDef _ ->
            "<BINARY OP DEF>"


recordExprContentToString : RecordExprContent -> String
recordExprContentToString content =
    case content of
        Field { field, expr } ->
            field ++ ":" ++ exprToString expr

        Pun field ->
            field

        Spread id ->
            "..." ++ Id.toString id


exprToString : Expr -> String
exprToString expr =
    case expr of
        Int n ->
            String.fromInt n

        Float n ->
            String.fromFloat n

        Char c ->
            "'" ++ c ++ "'"

        String s ->
            "\"" ++ s ++ "\""

        Bool b ->
            if b then
                "True"

            else
                "False"

        Unit ->
            "()"

        Tuple xs ->
            "(" ++ String.join "," (List.map exprToString xs) ++ ")"

        List xs ->
            "[" ++ String.join "," (List.map exprToString xs) ++ "]"

        Record contents ->
            "{" ++ String.join "," (List.map recordExprContentToString contents) ++ "}"

        UnaryOp InfiniteRange e ->
            exprToString e ++ unaryOp InfiniteRange

        UnaryOp op e ->
            unaryOp op ++ exprToString e

        BinaryOp left op right ->
            exprToString left ++ binaryOp op ++ exprToString right

        Call { fn, args } ->
            exprToString fn
                ++ "("
                ++ String.join "," (List.map exprToString args)
                ++ ")"

        RecordGet { record, field } ->
            exprToString record ++ "." ++ field

        Block { stmts, ret } ->
            "{\n" ++ String.join "\n" (List.map (stmtToString >> indent) stmts) ++ "\n}"

        EffectBlock _ ->
            Debug.todo "expr to string - effect block"

        Constructor_ { id, args } ->
            Id.toString id
                ++ (if List.isEmpty args then
                        ""

                    else
                        "("
                            ++ String.join "," (List.map exprToString args)
                            ++ ")"
                   )

        Identifier id ->
            Id.toString id

        RootIdentifier id ->
            "::" ++ Id.toString id

        Lambda r ->
            lambdaToString r

        RecordGetter field ->
            "." ++ field

        If _ ->
            Debug.todo "expr to string - if"

        Case _ ->
            Debug.todo "expr to string - case"


stmtToString : Stmt -> String
stmtToString stmt =
    Debug.todo "stmt to string"


indent : String -> String
indent str =
    "    " ++ str
