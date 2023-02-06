export type Expr =
    // literals
    | {expr:'int',    int:number}
    | {expr:'float',  float:number}
    | {expr:'char',   char:string} // It's holding a string because of extended grapheme clusters.
    | {expr:'string', string:string}
    | {expr:'bool',   bool:boolean} // TODO define this as Constructor instead?
    | {expr:'unit'}
  
    // collections
    | {expr:'tuple',  elements:Expr[]}
    | {expr:'list',   elements:Expr[]}
    | {expr:'record', fields:RecordField[]}
    | {expr:'constructor', name:string, args:Expr[]}

    // calls
    | {expr:'unary-op',   op:UnaryOp,  arg:Expr}
    | {expr:'binary-op',  op:BinaryOp, left:Expr, right:Expr}
    | {expr:'call',       fn:Expr,     args:Expr[]}
    | {expr:'record-get', record:Expr, field:string} // r.a
    | {expr:'pipeline',   arg:Expr,    fn:Expr}      // a |> b

    // other
    | {expr:'id', id:Identifier}
    | {expr:'lambda',  args:Pattern[], body:Expr}
    | {expr:'closure', args:Pattern[], body:Expr, env:Map<Identifier,Expr>} // not creatable via syntax, only by the interpreter
    | {expr:'record-getter', field:string} // .a
    | {expr:'if', cond:Expr, then:Expr, else:Expr}

export type Type =
    | {type:'named',  name:string}              // Int
    | {type:'var',    var:Typevar}              // a
    | {type:'call',   name:string, args:Type[]} // List[a]
    | {type:'fn',     from:Type, to:Type}       // x -> y
    | {type:'tuple',  elements:Type[]}          // (Int, Bool)
    | {type:'record', fields:RecordTypeField[]} // {a:Int,b:Bool}
    | {type:'unit'}                             // ()

export type Pattern =
    | {pattern:'var', var:string}
    // TODO other patterns

export type Bang =
    | {bang:'value', val:Identifier}             // foo!
    | {bang:'call',  fn:Identifier, args:Expr[]} // foo!(123,456)

export type Stmt =
    | {stmt:'let',      mod:LetModifier, name:string, body:Expr} // x = 123
    | {stmt:'let-bang', mod:LetModifier, name:string, body:Bang} // x = foo!(123)
    | {stmt:'bang',     bang:Bang}                               // foo!(123)

export type Decl =
    | {decl:'type-alias',       mod:TypeAliasModifier, name:string, vars:Typevar[], body:Type}
    | {decl:'type',             mod:TypeModifier,      name:string, vars:Typevar[], constructors:Constructor[]}
    | {decl:'module',           mod:ModuleModifier, name:string, decls:Decl[]}
    | {decl:'extend-module',    id:Identifier, decls:Decl[]}
    | {decl:'function',         name:string, args:Pattern[], body:Expr} // only the non-block, simple expression kind
    | {decl:'statement',        stmt:Stmt}
    | {decl:'block',            name:string, block:Block}                                      // x = { ... }
    | {decl:'block-fn',         name:string, args:Pattern[], block:Block}                      // x(a,b) = { ... }
    | {decl:'effect-block',     name:string, qualifiers:string[], block:Block}                 // x = My.Monad { ... }
    | {decl:'effect-block-fn',  name:string, args:Pattern[], qualifiers:string[], block:Block} // x(a,b) = My.Monad { ... }
    | {decl:'value-annotation', name:string, type:Type} // TODO move this into Stmt?
    // TODO Function annotation: x(y: Int): Bool

export type Block =           {stmts:Stmt[], ret:Expr|null}
export type Identifier =      {qualifiers:string[], name:string} // x, IO.println
export type RecordField =     {field:string, value:Expr} // a:123
export type RecordTypeField = {field:string, type:Type}  // a:Int
export type Constructor =     {name:string, args:ConstructorArg[]} // Foo, Bar(Int), Baz(n: Int, verbose: Bool)
export type ConstructorArg =  {name:string|null, type:Type}
export type Typevar =         string;

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
    | 'NegateNum'  // -
    | 'NegateBool' // !
    | 'NegateBin'  // ~

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
