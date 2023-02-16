export type Expr =
    // literals
    | {expr:'int',    int:bigint}
    | {expr:'float',  float:number}
    | {expr:'char',   char:string} // It's holding a string because of extended grapheme clusters.
    | {expr:'string', string:string}
    | {expr:'bool',   bool:boolean} // TODO define this as Constructor instead?
    | {expr:'unit'}
  
    // collections
    | {expr:'tuple',  elements:Expr[]}      // (1,True)
    | {expr:'list',   elements:Expr[]}      // [1,2]
    | {expr:'record', contents:RecordExprContent[]} // {a:1, ...b, c:True}

    // calls
    | {expr:'unary-op',   op:UnaryOp,  arg:Expr}
    | {expr:'binary-op',  op:BinaryOp, left:Expr, right:Expr}
    | {expr:'call',       fn:Expr,     args:Expr[]}  // foo(), bar(1,2)
    | {expr:'record-get', record:Expr, field:string} // r.a

    // blocks
    | {expr:'block',        block:Block}                              // x = { ... }
    | {expr:'effect-block', monadModule:UpperIdentifier, block:Block} // x = My.Monad { ... }

    // other
    | {expr:'constructor', id:UpperIdentifier} // Foo, Bar.Foo
    | {expr:'identifier',  id:LowerIdentifier} // foo, Bar.foo
    | {expr:'lambda',  args:Pattern[], body:Expr}
    | {expr:'closure', args:Pattern[], body:Expr, env:Map<LowerIdentifier,Expr>} // not creatable via syntax, only by the interpreter
    | {expr:'record-getter', field:string} // .a
    | {expr:'if', cond:Expr, then:Expr, else:Expr}
    | {expr:'case',subject:Expr, branches:CaseBranch[]}

export type Type =
    | {type:'named',  qualifiers:string[], name:string} // Int, Base.Maybe
    | {type:'call',   fn: Type, args:Type[]}            // List[a]
    | {type:'var',    var:Typevar}                      // a
    | {type:'fn',     from:Type, to:Type}               // x -> y
    | {type:'tuple',  elements:Type[]}                  // (Int, Bool)
    | {type:'record', fields:RecordTypeField[]}         // {a:Int,b:Bool}
    | {type:'unit'}                                     // ()

export type Pattern =
    | {pattern:'unit'}                           // ()
    | {pattern:'var', var:string}                // a
    | {pattern:'constructor', id:UpperIdentifier, args:Pattern[]} // Foo, Bar.Foo, Foo(a), Foo(_), Foo([])
    | {pattern:'int', int:bigint}                // 1
    | {pattern:'float', float:number}            // 1.2345
    | {pattern:'list',  elements:Pattern[]}      // [1,a]
    | {pattern:'tuple', elements:Pattern[]}      // (1,a)
    | {pattern:'wildcard'}                       // _
    | {pattern:'spread', var:string|null}        // ...a, ..._
    | {pattern:'record-spread'}                  // {..}
    | {pattern:'record-fields', fields:string[]} // {a}, {a,b}
    // TODO other patterns

export type Bang =
    | {bang:'value', val:Expr}             // foo!,      Bar.foo!,      x |> foo!,      foo.bar!
    | {bang:'call',  fn:Expr, args:Expr[]} // foo!(1,2), Bar.foo!(1,2), x |> foo!(1,2), foo.bar!(1,2)

export type Stmt =
    | {stmt:'let',      mod:LetModifier, type:Type|null, lhs:Pattern, body:Expr} // x = 123,           x: Int = 123
    | {stmt:'let-bang', mod:LetModifier, type:Type|null, lhs:Pattern, body:Bang} // x = Bar.foo!(123), x: Maybe[Int] = Bar.foo!(123)
    | {stmt:'bang',     bang:Bang}                                               // Bar.foo!(123)

export type Decl =
    | {decl:'type-alias',         mod:TypeAliasModifier, name:string, vars:Typevar[], body:Type}
    | {decl:'type',               mod:TypeModifier,      name:string, vars:Typevar[], constructors:Constructor[]}
    | {decl:'module',             mod:ModuleModifier,    name:string, decls:Decl[]}
    | {decl:'extend-module',      module:UpperIdentifier, decls:Decl[]}
    | {decl:'function',           mod:LetModifier, name:string, args:FnArg[], resultType:Type|null, body:Expr}
    | {decl:'binary-operator',    mod:LetModifier, op:BinaryOp, left:FnArg, right:FnArg, resultType:Type|null, body:Expr}
    | {decl:'unary-operator',     mod:LetModifier, op:UnaryOp, arg:FnArg, resultType:Type|null, body:Expr}
    | {decl:'statement',          stmt:Stmt}
    | {decl:'value-annotation',   name:string, type:Type}
    | {decl:'function-annotation',mod:LetModifier, name:string, args:FnTypeArg[], resultType:Type|null}

export type Block =           {stmts:Stmt[], ret:Expr|null}
export type Identifier =      {qualifiers:string[], name:string}
export type LowerIdentifier = {qualifiers:string[], name:string} // x, IO.println
export type UpperIdentifier = {qualifiers:string[], name:string} // X, IO.Println
export type RecordTypeField = {field:string, type:Type}  // a:Int
export type Constructor =     {name:string, args:ConstructorArg[]} // Foo, Bar(Int), Baz(n: Int, verbose: Bool)
export type ConstructorArg =  {name:string|null, type:Type}      // a:Int, Int
export type FnTypeArg =       {name:string|null, type:Type|null} // a:Int, Int, a
export type FnArg =           {pattern:Pattern, type:Type|null}  // a, _, a:Int
export type Typevar =         string;
export type CaseBranch =      {orPatterns:Pattern[], body:Expr} // 1 -> "hello", 1 | 2 -> "hello"
export type RecordExprContent = 
    | {recordContent:'field',  field:string, value:Expr} // a:123
    | {recordContent:'pun',    field:string}             // a
    | {recordContent:'spread', recordId:LowerIdentifier} // ...b

export type LetModifier =
    | 'NoModifier'
    | 'Private'

export type TypeAliasModifier =
    | 'NoModifier'
    | 'Private'

export type TypeModifier =
    | 'NoModifier'
    | 'Private'
    | 'Opaque'

export type ModuleModifier =
    | 'NoModifier'
    | 'Private'

export type UnaryOp =
    | 'NegateNum'  // -e
    | 'NegateBool' // !e
    | 'NegateBin'  // ~e
    | 'InfiniteRangeInclusive' // e..

export type BinaryOp =
    // arithmetic
    | 'Plus'  // +
    | 'Minus' // -
    | 'Times' // *
    | 'Div'   // /
    | 'Mod'   // %
    | 'Pow'   // **

    // binary
    | 'OrBin'   // |  
    | 'AndBin'  // &  
    | 'XorBin'  // ^  
    | 'ShiftL'  // << 
    | 'ShiftR'  // >> 
    | 'ShiftRU' // >>>

    // comparisons and booleans
    | 'Lte'     // <=
    | 'Lt'      // <
    | 'Eq'      // ==
    | 'Neq'     // !=
    | 'Gt'      // >
    | 'Gte'     // >=
    | 'OrBool'  // ||
    | 'AndBool' // &&

    // appendables
    | 'Append' // ++

    // ranges
    | 'RangeInclusive' // ..
    | 'RangeExclusive' // ...
