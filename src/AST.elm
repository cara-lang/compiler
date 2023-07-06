module AST exposing
    ( Argument
    , Bang(..)
    , BinaryOp(..)
    , CaseBranch
    , Constructor
    , ConstructorArg
    , Decl(..)
    , Expr(..)
    , FunctionModifier(..)
    , LetModifier(..)
    , LetStmt
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
    | Block { letStmts : List LetStmt, ret : Expr } -- x = { ... }
    | EffectBlock { monadModule : Id, stmts : List Stmt, ret : Maybe Expr } -- x = My.Monad { ... }
      -- Other
    | Constructor_ { id : Id, args : List Expr } -- Foo, Bar.Foo, Foo(1,2), Bar.Foo(1,2)
    | Identifier Id -- foo, Bar.foo
    | RootIdentifier Id -- ::foo, ::Bar.foo
    | Lambda { args : List Argument, body : Expr }
    | RecordGetter String -- .field
    | If { cond : Expr, then_ : Expr, else_ : Expr }
    | Case { subject : Expr, branches : List CaseBranch }


type Type
    = TNamed Id -- Int, Base.Maybe
    | TApplication { type_ : Type, args : List Type } -- List[a]
    | TVar String -- a
    | TFn { from : Type, to : Type } -- x -> y
    | TTuple (List Type) -- (Int, Bool)
    | TRecord (List RecordTypeField) -- {a:Int,b:Bool}
    | TUnit -- ()


type Pattern
    = -- TODO other patterns
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


type alias Argument =
    { pattern : Pattern
    , type_ : Maybe Type
    }


type Bang
    = BValue Expr -- foo!, Bar.foo!, x |> foo!, foo.bar!
    | BCall { fn : Expr, args : List Expr } -- foo!(1,2), Bar.foo!(1,2), x |> foo!(1,2), foo.bar!(1,2)


type alias LetStmt =
    { mod : LetModifier
    , lhs : Pattern
    , type_ : Maybe Type
    , expr : Expr
    }


type Stmt
    = SLet LetStmt
    | SLetBang { mod : LetModifier, lhs : Pattern, type_ : Maybe Type, bang : Bang }
    | SBang Bang


type FunctionModifier
    = FunctionNoModifier
    | FunctionPrivate


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
    | DExtendModule { id : Id, decls : List Decl }
    | DFunction { mod : FunctionModifier, name : String, args : List Argument, retType : Maybe Type, body : Expr }
    | DBinaryOperator { op : BinaryOp, body : Expr } -- TODO mod, left, right, resultType
    | DUnaryOperator { op : UnaryOp, body : Expr } -- TODO mod, arg, resultType
    | DStatement Stmt
    | DValueAnnotation { name : String, type_ : Type }
    | DUnitTest { name : Maybe String, expr : Expr }
    | DParameterizedTest
        { name : Maybe String
        , table : List Expr
        , -- implicit lambda
          args : List Argument
        , expr : Expr
        }
    | DPropertyTypeTest
        { name : Maybe String
        , types : Type
        , -- implicit lambda
          args : List Argument
        , expr : Expr
        }
    | DPropertyGenTest
        { name : Maybe String
        , gens : Expr
        , -- implicit lambda
          args : List Argument
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
