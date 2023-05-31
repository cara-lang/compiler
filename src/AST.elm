module AST exposing
    ( Bang(..)
    , Decl(..)
    , Expr(..)
    , Pattern(..)
    , Program
    , Stmt(..)
    , Type(..)
    )

import Env exposing (Env)
import Id exposing (Id)


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
    | Block { stmts : List Stmt, ret : Maybe Expr } -- x = { ... }
    | EffectBlock { monadModule : Id, stmts : List Stmt, ret : Maybe Expr } -- x = My.Monad { ... }
      -- Other
    | Constructor_ { id : Id, args : List Expr } -- Foo, Bar.Foo, Foo(1,2), Bar.Foo(1,2)
    | Identifier Id -- foo, Bar.foo
    | RootIdentifier Id -- ::foo, ::Bar.foo
    | Lambda { args : List Pattern, body : Expr }
    | Closure { args : List Pattern, body : Expr, env : Env }
    | RecordGetter String -- .field
    | If { cond : Expr, then_ : Expr, else_ : Expr }
    | Case { subject : Expr, branches : List CaseBranch }


type Type
    = TNamed Id -- Int, Base.Maybe
    | TCall { type_ : Type, args : List Type } -- List[a]
    | TVar String -- a
    | TFn { from : Type, to : Type } -- x -> y
    | TTuple (List Type) -- (Int, Bool)
    | TRecord (List RecordTypeField) -- {a:Int,b:Bool}
    | TUnit -- ()


type Pattern
    = -- TODO other patterns
      PUnit -- ()
    | PVar String -- a
    | PConstructor { id : Id, children : List Pattern } -- Foo, Bar.Foo, Foo(a), Foo(_), Foo([])
    | PInt Int -- 1
    | PFloat Float -- 1.2345
    | PList (List Pattern) -- [1,a]
    | PTuple (List Pattern) -- (1,a)
    | PWildcard -- _
    | PSpread (Maybe String) -- ...a, ..._
    | PRecordSpread -- {..}
    | PRecordFields (List String) -- {a}, {a,b}


type Bang
    = BValue Expr -- foo!, Bar.foo!, x |> foo!, foo.bar!
    | BCall { fn : Expr, args : List Expr } -- foo!(1,2), Bar.foo!(1,2), x |> foo!(1,2), foo.bar!(1,2)


type Stmt
    = -- TODO modifiers, types, lhs is Pattern instead of just a name
      SLet { name : String, expr : Expr }
    | SLetBang { name : String, bang : Bang }
    | SBang Bang


type Decl
    = DTypeAlias { name : String, body : Type } -- TODO mod, vars
    | DTypeDecl { name : String, constructors : List Constructor } --  TODO mod, vars
    | DModule { name : String, decls : List Decl } -- TODO mod
    | DExtendModule { id : Id, decls : List Decl }
    | DFunction { name : String, body : Expr } -- TODO mod, args, resultType
    | DBinaryOperator { op : BinaryOp, body : Expr } -- TODO mod, left, right, resultType
    | DUnaryOperator { op : UnaryOp, body : Expr } -- TODO mod, arg, resultType
    | DStatement Stmt
    | DValueAnnotation { name : String, type_ : Type }
    | DFunctionAnnotation { name : String, resultType : Type } -- TODO mod, args


type alias CaseBranch =
    -- 1 -> "hello", 1 | 2 -> "hello"
    { orPatterns : List Pattern
    , body : Expr
    }


type RecordExprContent
    = Field { field : String, value : Expr } -- a:123
    | Pun String -- a
    | Spread Id -- ...b


type alias RecordTypeField =
    -- a:Int
    { field : String
    , type_ : Type
    }


type alias Constructor =
    -- Foo, Bar(Int), Baz(n: Int, verbose: Bool)
    { name : String
    , args : List ConstructorArg
    }


type alias ConstructorArg =
    -- a:Int, Int
    { name : Maybe String
    , type_ : Type
    }


type UnaryOp
    = NegateNum -- -e
    | NegateBool -- !e
    | NegateBin -- ~e
    | InfiniteRangeInclusive -- e..


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
    | ShiftRU -- >>>
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
