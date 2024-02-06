#import "template.typ": *

#show: project.with(
  title: "The Cara Language Specification",
  authors: (
    (name: "Martin Janiczek", email: "https://cara-lang.com"),
  ),
  date: "June 5, 2023",
)

// Display inline code in a small box
// that retains the correct baseline.
#show raw.where(block: false): box.with(
  fill: luma(240),
  inset: (x: 3pt, y: 0pt),
  outset: (y: 3pt),
  radius: 2pt,
)

// Display block code in a larger block
// with more padding.
#show raw.where(block: true): block.with(
  fill: luma(240),
  inset: 10pt,
  radius: 4pt,
)

#show ref: set text(blue)

#show table: set table(stroke: 1pt + luma(180))

#show heading: it => {
  if it.level < 4 { pagebreak(weak: true) + text(size: 16pt, it) } 
  else            { text(size: 14pt, it) }
}

#let example(body) = {
  block(
    fill: luma(220),
    inset: 8pt,
    radius: 4pt,
    [#body]
  )
}

#let syntax(body) = {
  block(
    fill: luma(220),
    inset: 4pt,
    radius: 7pt,
    box(
      inset: (x: 4pt, top: 4pt, bottom: -6pt),
      smallcaps("Syntax")
    ) + body
  )
}

#let todo(body) = {
  block(
    fill: rgb("#b1f2eb"),
    inset: 8pt,
    radius: 4pt,
    [*TODO:* #body]
  )
}

= Introduction
Cara #footnote[https://cara-lang.com] is a general-purpose programming language aiming to combine the functional style of ML-family languages (Elm, OCaml, Haskell) with the familiar mainstream ALGOL-style syntax (Kotlin, Rust).

More specifically, Cara aims to inhabit a sweet spot in the programming language design space: pleasant to write, read and maintain while staying safe and dependable.

This is achieved by building on functional features (algebraic data types, purity, automatic full type inference) while keeping a façade of imperative procedural syntax.

Outside the scope of this specification are the reference interpreter and compiler #footnote[https://github.com/cara-lang/compiler], the latter of which aims to transpile Cara programs to HVM #footnote[https://github.com/HigherOrderCO/HVM] programs, which can in turn be compiled to native binaries with _automatic parallelism_.

This specification document describes the detailed features, syntax, and semantics of the Cara language.

#pagebreak()

= Example programs

```rs
quickSort(List[Int]): List[Int]
quickSort([]) = []
quickSort([x,...xs]) = {
  (lt, gt) = List.partition(#(x >= _), xs)
  quickSort(lt) ++ x ++ quickSort(gt)
}

[5,1,3,2,4]
  |> quickSort
  |> IO.println! // -> [1,2,3,4,5]
```

```rs
#!/usr/bin/env cara
dst = IO.ask!("Enter destination filename: ")
dstHandle = FS.open!(dst, FS.Write)

timestampFmt = "hh:mm:ss.fff"
1..10 |> IO.forEach!(\i -> IO {
  time = Time.now!()
  dstHandle |> FS.write!("[${Time.format(timestampFmt, time)}] ${i=}\n")
})
```

```rs
fizzbuzz(n) =
  if n % 15 == 0 then "FizzBuzz"
  else if n % 3 == 0 then "Fizz"
  else if n % 5 == 0 then "Buzz"
  else "$n"

1..20
  |> List.map(fizzbuzz)
  |> String.join(", ")
  |> IO.println!
```

```rs
type Maybe[a] =
  | Nothing
  | Just(a)

traverse(fn: a -> Maybe[b], list: List[a]): Maybe[List[b]]
traverse(fn,list) = go(list,[])
  where
    go([],bs) = Just(List.reverse(bs))
    go([a,...as],bs) = 
      case fn(a) of
        Nothing -> Nothing
        Just(b) -> go(as,b++bs)

xs = [1,2,3,4,5]
ys = [6,7,8,9,10]
f = \n -> if n == 3 then Nothing else Just(n)
IO.println!(xs |> traverse(f)) // -> Nothing
IO.println!(ys |> traverse(f)) // -> Just([6,7,8,9,10])
```

= In this document <in-this-document>


#syntax[
```py
# This is how Ungrammar-like syntax is stylized.
# Each syntax section usually begins with this block.
# It's not meant to be precise, particularly not around whitespace and EOLs.
# 
# For more on Ungrammar see:
# https://rust-analyzer.github.io/blog/2020/10/24/introducing-ungrammar.html
```
]

#todo[This is how TODOs are stylized.

Hopefully there are none by the time you read it!]

```rs
// This is how Cara source code is stylized.
x = 1
IO.println!("Hello world!")
```

#example[This is how examples are stylized.

```rs
IO.println!("They can contain code too!")
```
]

This is how links are stylized: @in-this-document.

This is how `inline code` is stylized.

#pagebreak()

= Syntax

At the very top, a Cara program consists of a series of declarations:

#syntax[
```py
program = shebangComment? declaration*
```
]

The program is implicitly in an `IO` effect block (@effect-block-expr), with the exception that declarations are allowed (effect blocks normally only permit statements):

```rs
// myScript.cara
name = IO.ask!("What's your name? ")
x = 1 + 2 + 3
IO.println!("Hello $name! Sum of first three natural numbers is $x.")
```

== Declarations <decl>

Declaration can be one of the following:

#syntax[
```py
declaration = sumTypeDecl
            | typeAliasDecl
            | moduleDecl
            | extendModuleDecl
            | statementDecl
            | functionDecl
            | unaryOpDecl
            | binaryOpDecl
            | valueAnnotationDecl
            | functionAnnotationDecl
```
]

=== Sum type <sum-type-decl>

#syntax[
```py
# type User = Anonymous | LoggedIn(token: String, name: String)
# type Maybe[a] = Nothing | Just(a)
sumTypeDecl = sumModifier? 'type' upperName
              ('[' typeVar (',' typeVar)* ']')?
              '=' constructorList

sumModifier = 'private' | 'opaque'

# Foo | Bar
# \n | Anonymous | LoggedIn(String)
constructorList = (EOL+ '|')?
                  constructor ('|' constructor)*

# Foo
# Foo(Int)
# Foo(Int, String)
# Foo(x: Int, y: Int)
constructor = upperName
              ('(' constructorArg (',' constructorArg)* ')')?

# Int
# x: Int
constructorArg = (constructorArgName ':')? type

# x
# userName
constructorArgName = lowerName
```
]

A core feature of Cara are _sum types_, sometimes also called _tagged unions._ #footnote[https://en.wikipedia.org/wiki/Tagged_union]

These allow representing a choice: eg. an user can be an anonymous user _OR_ a logged in user _OR_ a logged in administrator.

Contrast this with _product types_, which are ubiquitous in programming and allow holding multiple pieces of data at the same time: an user has a name _AND_ an address.

Here is an example of defining a new sum type in Cara:

```rs
type User =
  | Anonymous
  | LoggedIn
  | LoggedInAdmin
  
currentUser = LoggedInAdmin // A value can only be one of these at any given time!
```

This is only the simplest variant of sum types, but they can also contain data:

```rs
type User =
  | Anonymous
  | LoggedIn({ token: String, permissions: List[Permission] })
  | LoggedInAdmin({ token: String })

currentUser = LoggedInAdmin({ token: "xoxb-..." })
```

Each constructor can also hold multiple pieces of data without resorting to a record or tuple:

```rs
type User =
  | Anonymous
  | LoggedIn(String, List[Permission]) // holding String AND List[Permission]
  | LoggedInAdmin(String)

currentUser = LoggedIn("xoxb-...", [])
```

Each defined constructor also creates a way to create values of this type:

```rs
Anonymous     // : User
LoggedIn      // (String, List[Permission]): User
LoggedInAdmin // (String): User

LoggedIn("xoxb-...", [])  // : User
LoggedInAdmin("xoxb-...") // : User
```

The constructor arguments can also be labeled:

```rs
type User =
  | Anonymous
  | LoggedIn(token: String, permissions: List[Permission])
  | LoggedInAdmin(token: String)

currentUser = LoggedIn("xoxb-...", [])
```

The labels are enforced when two values of the same type are next to each other:

```rs
type Expr =
  | EInt(Int)
  | EPlus(x: Expr, y: Expr)
```

If a constructor holds no arguments, it is not a function and doesn't need the function call parentheses when used. Thus, continuing the examples above:

```rs
user1 = Anonymous   // ✔️
user2 = Anonymous() // ✘
```

Data can be extracted from the sum types via pattern matching:

```rs
token =
  case currentUser of
    Anonymous            -> Nothing
    LoggedIn(token, _)   -> Just(token)
    LoggedInAdmin(token) -> Just(token)
```

==== Alternative shorthand syntax

There is an alternative syntax for sum type declaration that puts the first type on the same line as the `=` and doesn't use the leading `|` character:

```rs
type NullableInt = MyNull | MyInt(Int)
```

```rs
type NullableInt = MyNull 
                 | MyInt(Int)
```

Note that putting the `=` on a new line is not allowed:

```rs
type NullableInt
  = MyNull // ✘
  | MyInt(Int)
```

==== Generic sum types

Sum types can be parameterized over a type they will hold:
  
```rs
type List[a] = 
  | Empty
  | Cons(a, List[a])
```

```rs
type Result[err,ok] = 
  | Err(err)
  | Ok(ok)
```

```rs
x: List[Result[String,Int]] = [Err("boo"),Ok(42)]

type alias MyResult[ok] = Result[Int,ok]
y: MyResult[a] = Err(123)
```

==== TODO Private sum type
==== TODO Opaque sum type

=== Type alias <type-alias-decl>

#syntax[
```py
# type alias Foo = Int
# private type alias Bar[a,b] = Result[a,b]
typeAliasDecl = 'private'? 'type' 'alias' upperName
                ('[' typeVar (',' typeVar)* ']')?
                '=' type
```
]

Type alias gives a new name to an existing type:

```rs
type alias XY = (Int, Int)
type alias C = { real: Float, imag: Float }
type alias MyResult[a] = Result[String,a]

position: XY = (1,2)
```

This is _structural_, not _nominal_, and so you can use values of the original type where values of the alias are expected and vice versa:

```rs
velocity: (Int,Int) = (0,-5)

step1((px,py): XY, (vx,vy): XY): XY =
  (px+vx, py+vy)
  
step2((px,py): (Int,Int), (vx,vy): (Int,Int)): (Int,Int) =
  (px+vx, py+vy)
 
step1((0,1), velocity) // -> (0,-4)
step2((0,1), velocity) // -> (0,-4)
```

==== TODO Private type alias

=== Module <module-decl>

#syntax[
```py
# module Foo { ... }
# private module Foo { ... }
moduleDecl = 'private'? 'module' upperName
             '{' declaration* '}'
```
]

A module serves as a container or _namespace_ for a set of declarations:

```rs
module Maybe {
  
  type Maybe[a] = Nothing | Just(a)
  
  map(fn: a -> b, Nothing) = Nothing
  map(fn: a -> b, Just(a)) = Just(fn(a))
      
}
// outside the module, its public contents can be used via the namespace:
Maybe.Nothing // -> Nothing
Maybe.Just(1) |> Maybe.map(#(_ + 1)) // -> Just(2)
```

Modules can be nested:

```rs
module Foo {
  module Bar {
    x = 1
  }
}

Foo.Bar.x // -> 1
```

There can also be multiple modules next to each other; there is no one-to-one correspondence between modules and files.

#todo[Talk about opening a module (importing its members into the current namespace).]

==== TODO Private module

=== Extend module <extend-module-decl>

#syntax[
```py
# extend module Foo { ... }
# extend module Foo.Bar { ... }
extendModuleDecl = 'extend' 'module' upperIdentifier
                   '{' declaration* '}'
```
]

Defining a module second time or redefining existing definitions is disallowed, but you can add new definitions with the `extend module` declaration:

```rs
module Foo {
  x = 1
}

extend module Foo {
  y = x + 1
  module Bar {
    z = 0
  }
}

Foo.y // -> 2
Foo.Bar.z // -> 0
```

The `extend module` declaration can't access private definitions.

=== Statement <statement-decl>

#syntax[
```py
# x = 1
# x = foo!(123)
# foo!(123)
statementDecl = statement
```
]

Statements like `x = 1` and `IO.println!(123)` are declarations as well. See @stmt for more on Statements.

=== Function <function-decl>

#syntax[
```py
# f() = 1
# f(a,b) = a + b
# f(a,b): Int = a + b
# f(a: Int, b: Int) = a + b
# f(a: Int, b: Int): Int = a + b
# private f(a,b) = a + b
functionDecl = 'private'? lowerName
               '(' (functionArg (',' functionArg)*)? ')'
               (':' type)?
               '=' expr
```
]

Functions are a syntax sugar over let statements (@let-stmt) and case expressions (@case-expr) that allows a concise equation style.

```rs
fib(0) = 0
fib(1) = 1
fib(n) = fib(n-1) + fib(n-2)
```

The above is equivalent to the following case expression:

```rs
fib(n) =
  case n of
    0 -> 0
    1 -> 1
    _ -> fib(n-1) + fib(n-2)
```

which is itself equivalent to the following let statement:

```rs
fib = \n -> 
  case n of
    0 -> 0
    1 -> 1
    _ -> fib(n-1) + fib(n-2)
```

#todo[Mention difference between `fn(Int,Bool): String` (function of two arguments) and `fn((Int,Bool)): String` (function of one tuple argument). Do we want to make them equivalent and interchangeable?]

==== TODO Private function

=== Unary operator overloading <unary-op-decl>

#syntax[
```py
# `!`(x: Int) = x + 1
# private `!`(x: Int) = x + 1
unaryOpDecl = 'private'? '`' unaryOp '`'
              '(' functionArg ')'
              (':' type)?
              '=' expr

unaryOp = '-' | '!' | '~' | '..'
```
]

Overloading unary operators is done via a function declaration where the function name is the operator literal surrounded by backticks:

```rs
type alias C = (Float, Float)
`!`((r,i): C): C = (r,-i) // complex conjugate

!(1,2) // -> (1,-2)
```

=== Binary operator overloading <binary-op-decl>

#syntax[
```py
# `+`(x: (), y: ()) = 42
binaryOpDecl = 'private'? '`' binaryOp '`'
               '(' functionArg ',' functionArg ')'
               (':' type)?
               '=' expr

binaryOp = '+' | '-' | '*' | '/' | '%' | '**'
         | '|' | '^' | '&'
         | '||' | '&&'
         | '++'
         | '..' | '...'
         | '<=' | '<' | '==' | '!=' | '>' | '>='
         | '<<' | '>>' | '>>>'
```
]

Overloading binary operators is done via a function declaration where the function name is the operator literal surrounded by backticks:

```rs
type alias XY = (Int, Int)
`*`(k: Int, (x,y): XY): XY = (k*x, k*y)

5 * (1,2) // -> (5,10)
```

=== Value type annotation <value-type-ann-decl>

#syntax[
```py
# x: Int
valueAnnotationDecl = lowerName ':' type
```
]

Values can be given type annotations outside their definition:

```rs
x : (Int, Bool)
x = (123, True)
```

This is equivalent to defining both on the same line:

```rs
x: (Int, Bool) = (123, True)
```

=== Function type annotation <function-type-ann-decl>

#syntax[
```py
# x(Int, Bool): Int
# add(a: Int, b: Int): Int
functionAnnotationDecl = lowerName
                         '(' (functionTypeArg (',' functionTypeArg)*)? ')'
                         ':' type
```
]

Functions can be given type annotations outside their definition:

```rs
lengthEquals(Int, String): Bool
lengthEquals(length, str) =
  String.length(str) == length
```

This is equivalent to defining both on the same line:

```rs
lengthEquals(length: Int, str: String): Bool =
  String.length(str) == length
```

The return type and the argument types in the signatures are mandatory; labels (`x: Int`) are optional.

Note it's disallowed to have two arguments of the same type next to each other without labelling them:

```rs
setDiff(Set[a], Set[a]): Set[a]                   // ✘
setDiff(orig: Set[a], toRemove: Set[a]): Set[a]   // ✔️
```

The labels in the type signatures only serve the programmer, they can differ from the signatures in the declarations themselves.

== Statements <stmt>

#syntax[
```py
statement = letStatement
          | letBangStatement
          | bangStatement
```
]

Statements exist inside block expressions (@block-expr) and wherever declarations are admitted (@statement-decl), eg. in the top-level and in modules:

```rs
IO.println!("In top-level")

module Foo {
  IO.println!("In a module")
}

x1 = IO {
  IO.println!("In a block")
}

x2 = {
  y = 1
  y + 1 // can't use bangs in non-effect blocks
}
```

Statements can roughly be translated into monadic expressions:

#table(
  columns: (auto, 1fr, 1fr,),
  [*Statement*],[*Example*],  [*Translation (Haskell)*],
  [Let],        [`x = 1`],    [`pure 1 >>= \x -> ...`],
  [Let-bang],   [`x = bang!`],[`bang   >>= \x -> ...`],
  [Bang],       [`bang!`],    [`bang   >>= \_ -> ...`],
)

Thus a list of statements with a return expression at the end can be reduced into a single expression:

```rs
isTitleNonEmpty(doc) = Maybe {
  head = doc.head!
  title = head.title!
  !title.isEmpty()
}
```

is equivalent to:

```rs
isTitleNonEmpty(doc) =
  doc.head |> Maybe.andThen (\head ->
    head.title |> Maybe.andThen (\title ->
      Maybe.pure(!title.isEmpty())
    )
  )
```

=== Let <let-stmt>

#syntax[
```py
# x = 123
# private x = 123
# (r,i) = complexNumber
# Foo(a) = myFoo
letStatement = 'private'? pattern
               (':' type)?
               '=' expr
```
]

A let statement executes an expression and binds its value according to the pattern on the left-hand side.

```rs
n = 10
string = List.repeat(n, "Hello world!")
           |> String.join(" ")
```

It is possible to use pattern-matching on the left-hand side:

```rs
(x,y) = position
```

It is disallowed to use the `_` wildcard pattern in let statements though (as any expression on the right-hand side is guaranteed to be effect-less if used outside a bang, and calculating it and throwing it away would not make sense).

#todo[What about debug logging? ie. the Elm pattern `let _ = Debug.log "x" (foo 123) in ...`]

=== Let-bang <let-bang-stmt>

A let-bang statement executes a bang, along with any effects it might have, and binds its resulting value according to the left-hand side pattern.

#syntax[
```py
# x: Int = foo!
# private (x,y) = Foo.bar!(123)
letBangStatement = 'private'? pattern
                   (':' type)?
                   '=' bang
```
]

This is handy as it effectively unwraps a monadic value, similarly to Haskell's `do` notation #footnote[https://en.wikibooks.org/wiki/Haskell/do_notation], Gleam's `use` #footnote[https://gleam.run/book/tour/use.html] and Roc's backpassing #footnote[https://github.com/roc-lang/roc/blob/master/roc-for-elm-programmers.md#backpassing], saving you from nested callback lambdas.

```rs
destination = IO.ask!("Enter destination filename: ") // "foo.txt"
destinationHandle = FS.open!(destination, FS.Write)   // <handle>
time = Time.now!                                      // 2023-06-04 ...
// ... presumably write something to the destination
```

#todo[Should it be `Time.now!` or `Time.now!()`?]

#todo[Is it possible to relax the requirements and have bangs anywhere on the right side, even multiple? Seems like it should be possible to automate this. Do we want it, or would it lead to a more confusing code?]

The catch is that behind the scenes this _is equivalent_ to the nested callback lambdas, and your final value can't escape the monadic context (`IO` in the above example). For more information see @effect-block-expr on effect blocks: the top-level scope is implicitly in an IO effect block.

Note the difference between a let and let-bang:

```rs
input: Maybe[Int] = Just(10)
x = Maybe {
  a = input! // Int,        10
  b = input  // Maybe[Int], Just(10)
}
```

=== Bang <bang-stmt>

A bang statement executes a bang, along with any effects it might have, and throws away its result.

#syntax[
```py
# foo!
# Foo.bar!(123)
bangStatement = bang
```
]

A typical example of a bang is the humble `IO.println`:

```rs
IO.println!(123)
```

Because it returns `IO[()]` (and thus the best a let-bang could hope to unwrap from it is `()`), there is no point in using a let-bang for it and instead a bang statement is typically used.

== Bangs <bang>

There are two kinds of bangs:

#syntax[
```py
bang = valueBang
     | callBang
```
]

=== Value bang <value-bang>

#syntax[
```py
# foo!
# Bar.foo!
valueBang = expr '!'
```
]

A value bang allows unwrapping a monadic value:

```rs
maybeInt = Just(123)
maybeInt! // 123, if in effect block
```

=== Call bang <call-bang>

#syntax[
```py
# foo!(123)
# Bar.foo!("abc","def")
callBang = expr '!'
           '(' expr (',' expr)* ')'
```
]

A call bang is equivalent to a let statement and a value bang:

```rs
// Original
List.head!([1,2,3]) // Int, if in Maybe context

// Equivalent to:
x = List.head([1,2,3]) // Maybe[Int]
x!                     // Int, if in Maybe context
```

Note that since pipelines parse into function calls, it's also possible to have bangs looking like `x |> IO.println!` and `x |> Foo.bar!(1,2,3)`.

== Expressions <expr>

#syntax[
```py
expr = intExpr
     | floatExpr
     | charExpr
     | stringExpr
     | multilineStringExpr
     | boolExpr
     | unitExpr
     | tupleExpr
     | listExpr
     | recordExpr
     | recordAccessExpr
     | recordGetterExpr
     | blockExpr
     | effectBlockExpr
     | constructorExpr
     | idExpr
     | rootIdExpr
     | callExpr
     | pipelineExpr
     | lambdaExpr
     | holeExpr
     | ifExpr
     | caseExpr
     | opExpr
```
]

=== TODO Integers <int-expr>

#todo[Mention `_` allowed in integers (`123_456_789`).]

=== TODO Floats <float-expr>
=== TODO Characters <char-expr>
=== TODO Strings <string-expr>

#todo[Note that string interpolation is present in this type of string!]

=== TODO Multi-line strings <multiline-string-expr>

#todo[Note that string interpolation is present in this type of string!]

=== Booleans <bool-expr>

#syntax[
```py
# True
# False
boolExpr = 'True' | 'False'
```
]

Booleans `True` and `False` represent truth values, useful for holding yes/no and on/off information. Closely connected to predicates (functions `pred(a): Bool`):

```rs
isEmpty(s: String): Bool =
  String.length(s) == 0
  
isEmpty("hello") // -> False
isEmpty("")      // -> True
```

Used in if-expressions (@if-expr):

```rs
if isEmpty(myString) then
  doSomething()
else
  doSomethingElse()
```

Note that it's often better to create your own sum type of two values than to hold the data as a boolean:

```rs
type ReportLevel = Short | Verbose
level = Verbose

// rather than:
isLevelVerbose = True
```

=== Unit <unit-expr>

#syntax[
```py
# ()
unitExpr = '(' ')'
```
]

An unit expression is the only value of the unit type (@unit-type). As such it has no space to convey varying information and is often used to mean "nothing". Such as in `IO.println` returning an `IO[()]`: there's no information other than that the call succeeded.

```rs
echo: IO[()] = IO {
  input = IO.ask!("")
  IO.println!(input)
  () // I'm done!
}
```

Note that the above function is equivalent to one that omits the `()`, since blocks without the return value return `()` implicitly:

```rs
echo: IO[()] = IO {
  input = IO.ask!("")
  IO.println!(input) // <- we're NOT returning the return value of this
  // the return value is missing, `()` is used automatically
}
```

=== Tuples <tuple-expr>

#syntax[
```py
# (1,True,foo)
tupleExpr = '(' expr ',' expr (',' expr)* ')'
```
]

Tuples are _heterogenous_ collections of data (meaning, the elements do not have to be of the same type):

```rs
myTuple = (1, "Hello", ['a','b','c'])
```

They are an example of a _product type_ (contrast with _sum types_, @sum-type-decl): eg. a tuple `("Martin", 29)` holds the user name _AND_ their age _at the same time._ For another example of product types see records (@record-expr).

Tuples start from length 2, since:
- tuples of length 0 are just an unit expression (@unit-expr).
- tuples of length 1 are just the expression they contain.

#todo[Mention how Cara deals with leading/trailing commas in tuples.]

#todo[Mention the implicit getters for tuples: `.el1 .. .elN-1`.]

=== Lists <list-expr>

#syntax[
```py
# []
# [1]
# [1,2]
listExpr = '[' (expr (',' expr)*)? ']'
```
]

Lists are _homogenous_ collections of data (meaning, the elements have to be of the same type):

```rs
list0: List[a]    = []
list1: List[Int]  = [1]
list2: List[Bool] = [True, False]

badList = [1,True] // ✘ This won't compile
```

#todo[Mention how Cara deals with leading/trailing commas in lists.]

=== Records <record-expr>

#syntax[
```py
# {a:1, b:True}
# {...a, x:123, y}
recordExpr = '{' (recordExprContent (',' recordExprContent)*)? '}'

recordExprContent = recordExprField
                  | recordExprPun
                  | recordExprSpread
```
]

Records are collections of data, each with an associated unique _label_:

```rs
user = {name: "Martin", age: 29}
```

The order of fields doesn't matter: the following records are equivalent:

```rs
user1 = {name: "Martin", age: 29}
user2 = {age: 29, name: "Martin"}
user1 == user2 // -> True
```

#todo[Mention default comparison instance to be one with fields sorted ASC. Can we get to a world where most everything is comparable? (ie. sum types as well)]

It is an error to give a record two fields of the same name:

```rs
{name: "Martin", name: "Janiczek"} // ✘ This won't compile
```

#todo[Mention how Cara deals with leading/trailing commas in records.]

There are three types of record content:

==== Record field

#syntax[
```py
# field: 123
recordExprField = lowerName ':' expr
```
]

Fields are a combination of a field name and its corresponding value:

```rs
{x: 123, y: True} // record with two fields
```

==== Record pun

#syntax[
```py
# field
recordExprPun = lowerName
```
]

Puns only consist of the field name. The value is taken from the current scope:

```rs
x = 1
record = {x} // -> {x: 1}
```

It is an error to use a pun for a field name that cannot be found in the current scope:

```rs
record = {y} // ✘ This won't compile
```

==== Record spread

#syntax[
```py
# ...record
recordExprSpread = '...' lowerIdentifier
```
]

Spread adds fields from an existing record to the record being created.

```rs
velocity = {vx: 1, vy: 2}
ball = {x: 123, y: -5, ...velocity}
```

#todo[Add an end-to-end test for record creation via spread.]

#todo[Mention what happens when there's a collision. Is spread somehow special in that collisions are allowed (what field wins?), or are they disallowed?]

#todo[Add an end-to-end test for what happens when fields collide when using spread.]

=== Record access <record-access-expr>

#syntax[
```py
# myRecord.field
# createPerson(123).age
recordAccessExpr = expr '.' lowerName
```
]

Record access is one of ways to extract a field value from a record.

```rs
myRecord = {name: "Martin", age: 29}
myRecord.name // -> "Martin"
myRecord.age  // -> 29
```

The expression on the left doesn't need to be a record _literal_:

```rs
createPerson(age: Int): {name: String, age: Int}
createPerson(age) =
  { name: "Unknown soldier"
  , age
  }

createPerson(35).age // -> 35
```

Record access is equivalent to calling a record getter (@record-getter-expr):

```rs
myRecord = {name: "Martin", age: 29}
myRecord.name // -> "Martin"
// is equivalent to:
myRecord |> .name
.name(myRecord)
```

=== Record getters <record-getter-expr>

#syntax[
```py
# .recordField
recordGetterExpr = '.' lowerName
```
]

Record getters are a syntax shorthand for a function that extracts a field value from a record:

```rs
.name
// is equivalent to
(\{name} -> name)
(\record -> record.name)
```

```rs
people =
  [
    {name: "Martin", age: 29},
    {name: "Dumbledore", age: 115},
  ]

ages = people |> List.map(.age) // -> [29, 115]
```

=== Blocks <block-expr>

#syntax[
```py
# { 
#   x = 1     // let
#   y = 1 + x // let
#   (x,y)     // expr
# }
blockExpr = '{'
            (EOL+ letStatement)+
            EOL+ expr EOL+
            '}'
```
]

Blocks allow the use of `let` statements (@let-stmt) inside expressions:

```rs
greeting = {
  n = 3
  string = "Hello world!"
  List.repeat(n,string)
    |> String.join(" ")
}
// -> "Hello world! Hello world! Hello world!"
```

(The consequence of that is that bangs are disallowed inside blocks. For blocks with bangs see effect blocks: @effect-block-expr.)

The last item inside a block (which must be an expression, not a `let` statement) will be returned.

#todo[Mention what happens to the bindings from inside the block expression (they get discarded after exiting the scope.]

=== Effect blocks <effect-block-expr>

#syntax[
```py
# Maybe { 
#   head = doc.head!               // let-bang
#   title = head.title!            // let-bang
#   cap = String.capitalize(title) // let
#   cap != ""                      // expr
# }

# IO {
#   input = IO.ask!("") // let-bang
#   IO.println!(input)  // bang
# }
effectBlockExpr = upperIdentifier 
                  '{'
                  (EOL+ statement)+
                  (EOL+ expr)?
                  EOL+
                  '}'
```
]

Effect blocks are like normal blocks (@block-expr), but allow using bangs and let-bangs inside, as well as not specifying the return value.

When the return value is not specified, `()` is used implicitly:

```rs
echo: IO[()] = IO {
  input = IO.ask!("")
  IO.println!(input)
  // return expr is missing; returning ()
}
```

For more info on how statements inside an effect block translate back to a single expression, see the section on Statements: @stmt.

The identifier before the `{...}` specifies which monad's `pure` and `andThen` functions will be used when translating the block to an expr.

This also means that depending on how the underlying monad behaves, the latter lines might not happen (eg. if encountering a Nothing in a Maybe context):

```rs
convertThenNegate(string: String): Maybe[Int] = Maybe {
  int = String.toInt!(string)
  -int
}

try1 = convertThenNegate("123")   // -> Just(-123)
try2 = convertThenNegate("hello") // -> Nothing, doesn't get past the first line
```

#todo[Mention what happens to the bindings from inside the block expression (they get discarded after exiting the scope.]

=== Sum type constructors <constructor-expr>

#syntax[
```py
# Bar
# Foo.Bar(1,2,3)
constructorExpr = upperIdentifier
                  ('(' expr (',' expr)* ')')? 
```
]

When you define a sum type (@sum-type-decl):
- a value will be created for every constructor _without_ arguments.
- a function will be created for every constructor _with_ arguments.

```rs
type Outcome =
  | GotNothing       // 0 args, creates value    GotNothing: Outcome
  | GotError(Error)  // 1 arg,  creates function GotError(Error): Outcome
  | GotInts(Int,Int) // 2 args, creates function GotInts(Int,Int): Outcome
```

These functions work as advertised:

```rs
outcome: Outcome = GotInts(1,2)
```

You can extract data from the values and/or dispatch on them with `case..of` pattern matching (@constructor-pattern):

```rs
outcomeInt: Int = 
  case outcome of
    GotNothing   -> -1
    GotError(_)  -> -2
    GotInts(a,b) -> a + b
```

#todo[Do we want some kind of `fn(x) = case x of` shorthand like OCaml and Haskell has?]

=== Identifiers <id-expr>

#syntax[
```py
# foo
# Foo.bar
idExpr = lowerIdentifier
```
]

Identifiers allow access to values previously bound via let or let-bang statements (@stmt):

```rs
x = 123
x + 50 // -> evaluates to `123 + 50`
```

#todo[Perhaps talk about allowed characters in an identifier/name or reference some section talking about them.]

#todo[Talk about the scoping rules / how we try to match the identifier in ancestors if it can't be found in the current scope.]

=== Root identifiers <root-id-expr>

#syntax[
```py
# ::foo
# ::Foo.bar
rootIdExpr = '::' lowerIdentifier
```
]

Root identifiers allow access to values previously bound via let or let-bang statements (@stmt); they differ from identifiers (@id-expr) by searching in the root scope only.

```rs
x = 1
module Foo {
  x = 2
  module Bar {
    x = 3
    IO.println!(x)   // 3
    IO.println!(::x) // 1
  }
}
```

#todo[Is this even needed? Is there a structure of modules in which the `id-expr` syntax is not able to access a binding in some kind of sibling?]

#todo[Do we need a relative access as well? A way to get into a parent scope (`../`)? If yes, and if root identifiers are still useful, should they have the `/` syntax instead?]

=== Function calls <call-expr>

#syntax[
```py
# x()
# x(1)
# x(1,2)

callExpr = expr '(' 
           (expr (',' expr)*)?
           ')'
```
]

Function calls allow running a function (replacing the call expression with the body of the function with its arguments bound to the values from the call expression):

```rs
myFn(x) = (x, x + 1, x + 2)

myFn(100)
// --------> 
{
  x = 100
  (x, x + 1, x + 2)
}
// --------> 
(100, 100 + 1, 100 + 2)
// --------> 
(100, 101, 102)
```

This extends to any pattern used in function arguments:

```rs
myFn({a,b}, Foo(c)) = a + b + c

myFn({a: 1, b: 2, c: 3, d: 4}, Foo(5))
// --------> 
{
  {a,b} = {a: 1, b: 2, c: 3, d: 4}
  Foo(c) = Foo(5)
  a + b + c
}
// --------> 
1 + 2 + 5
// --------> 
8
```

=== Pipelines <pipeline-expr>

#syntax[
```py
# a |> b
# a |> fn(1,2)
pipelineExpr =  expr '|>' expr
```
]

Pipelines are a syntax sugar for function calls (@call-expr): the left argument is piped into the _last_ argument of the function on the right.

```rs
sub(x,y) = x - y
1 |> sub(5)
// equivalent to:
sub(5,1) // -> 4
```

```rs
addThree(x,y,z) = x + y + z
1 |> addThree(10,20)
// equivalent to:
addThree(10,20,1) // -> 31
```

The expression on the right doesn't need to be a function call, it can be anything that evaluates to a function (`myRecord.someFunction`, `myFunction`, `\x -> x + 1` etc.):

```rs
negate(x) = -x
10 |> negate
// equivalent to:
negate(10) // -> -10
```

Note that there is no currying or partial application in Elm: the arity needs to agree; all of the following would throw an error:

```rs
addThree(x,y,z) = x + y + z
1 |> addThree           // ✘
1 |> addThree(10)       // ✘
1 |> addThree(10,20,30) // ✘
```

=== TODO Anonymous functions (lambdas) <lambda-expr>
=== TODO Hole shorthand <hole-expr>
=== TODO If expressions <if-expr>
=== TODO Case expressions <case-expr>
=== Operators <op-expr>

#syntax[
```py
opExpr = unaryOpExpr
       | binaryOpExpr
```
]

Operators are 1-argument functions (_unary_) or 2-argument functions (_binary_) that are called with special syntax rules instead of the classic `foo(1,2)` function call.

```rs
a = -5    // unary: numeric negation
b = 5..   // unary: infinite range
c = 1 + 2 // binary: plus
```

There is a hardcoded number of binary and unary operators in Cara. Users are free to assign them specific meaning for various type combinations, but it is disallowed to define a _new_ operator literal.

==== Unary operators <unary-op-expr>

#syntax[
```py
unaryOpExpr = expr '..'
            | ('-' | '!' | '~') expr
```
]

- Numeric negation: `-e`
- Boolean negation: `!e`
- Binary negation:  `~e`
- Infinite range:   `e..`

For overloading unary operators see @unary-op-decl.

==== Binary operators <binary-op-expr>

#syntax[
```py
binaryOpExpr = expr binaryOp expr
```
]

All binary operators take a left and right expression as an argument (`e + e`).

In the following table, lower precedence value means lower priority when disambiguating expressions. // (think "inserting parentheses")

#example[Thus `1 + 2 * 3` parses as `1 + (2 * 3)`, since `+` has precedence 12 and `*` has precedence 13.]

Left-associative operators (eg. `&&`) of the same precedence do get grouped left-first:

#example[`1 + 2 - 3` turns into `(1 + 2) - 3`.]

Right-associative operators (eg. `**`) of the same precedence instead do get grouped right-first:

#example[`2 ** 3 ** 4` turns into `2 ** (3 ** 4)`.]

#pagebreak(weak: true)

#table(
  columns: (auto, auto, auto, 1fr,),
  [*Precedence*],[*Operator*],[*Literal*],[*Note*],
  [1],[Boolean AND],[`&&`],[#todo[What about short-circuiting?]],
  [2],[Boolean OR],[`||`],[#todo[What about short-circuiting?]],
  [3],[Append],[`++`],[],
  [4],[Pipeline],[`|>`],[This is not an user-definable operator. Parses into a function call instead.],
  [5],[Inclusive range],[`..`],[],
  [5],[Exclusive range],[`...`],[],
  [6],[Binary OR],[`|`],[],
  [7],[Binary XOR],[`^`],[],
  [8],[Binary AND],[`&`],[],
  [9],[Equal],[`==`],[],
  [9],[Not equal],[`!=`],[],
  [10],[Less than or equal],[`<=`],[],
  [10],[Less than],[`<`],[],
  [10],[Greater than],[`>`],[],
  [10],[Greater than or equal],[`>=`],[],
  [11],[Left shift],[`<<`],[],
  [11],[Right shift],[`>>`],[],
  [11],[Unsigned right shift],[`>>>`],[],
  [12],[Plus],[`+`],[],
  [12],[Minus],[`-`],[],
  [13],[Times],[`*`],[],
  [13],[Division],[`/`],[],
  [13],[Modulo],[`%`],[],
  [14],[Power],[`**`],[Right-associative.],
  [15],[Left parenthesis],[`(`],[This is not an user-definable operator. Right-associative.],
  [16],[Record getter],[`.`],[This is not an user-definable operator.],
)

For overloading unary operators see @binary-op-decl.

== TODO Patterns <pattern>

Patterns are syntax that simplifies extracting data from expressions and making conditions over expressions.

They are used on the "left hand sides" of function arguments, let statements and `case..of` expressions.

Patterns have two "powers":

1. A pattern can fail matching an expression.

#example[The pattern `(a,b)` will not match the expression `123`.]

2. If a pattern matches an expression, it has the opportunity to introduce new local bindings.

#example[
```rs
type X = Foo(Int)
myFoo = Foo(123)
Foo(n) = myFoo // <- matching pattern Foo(n) with the value myFoo
```
The above puts the number `123` into a variable `n`.
]

Multiple function equations with pattern matches do get combined into a single function with a `case..of` consisting of those equations:

#example[
```rs
fib(0) = 0
fib(1) = 1
fib(n) = fib(n-1) + fib(n-2)
```
lowers into:
```rs
fib(x) =
  case x of
    0 -> 0
    1 -> 1
    n -> fib(n-1) + fib(n-2)
```
]

Patterns need to be exhaustive; the compiler should raise an error when it finds a non-exhaustive set of patterns.

=== TODO Unit <unit-pattern>
=== TODO Variable <var-pattern>

#todo[Make sure we use `lowerName` here, not `lowerIdentifier`.]

=== TODO Constructor <constructor-pattern>
=== TODO Int <int-pattern>
=== TODO Float <float-pattern>
=== TODO Tuple <tuple-pattern>
=== TODO List <list-pattern>
=== TODO Wildcard <wildcard-pattern>
=== TODO Spread <spread-pattern>
=== TODO Record spread <record-spread-pattern>
=== TODO Record fields <record-field-pattern>

== TODO Types <type>

=== TODO Unit <unit-type>
=== TODO Variable <var-type>
=== TODO Named type <named-type>
=== TODO Type application <app-type>
=== TODO Function <function-type>
=== TODO Tuple <tuple-type>
=== TODO Record <record-type>

== Comments <comment>

Comments are annotations in the source code that are ignored by the compiler or the interpreter. Cara has three types of comments:

#syntax[
```py
comment = lineComment
        | blockComment
        | shebangComment
```
]

=== Line comments <line-comment>

#syntax[
```py
lineComment = '//' .* EOL
```
]

Line comments start with `//` and anything to the right of the two slashes is ignored. A newline ends the line comment.

```rs
// This is a line comment
IO.println!(1) // Another example
```

=== Block comments <block-comment>

#syntax[
```py
blockComment = '/*'
               ((! '*/') | blockComment)*
               '*/'
```
]

Block comments start with `/*` and end with `*/`. Thus, they are handy for multi-line text.

```rs
/* A block comment
   spanning multiple lines
*/
```

These can happen anywhere in the source code: #footnote[TODO: I'm pretty sure there is some exception to this but I'm not aware of it yet.]

```rs
myFunction(1, /* Some description */ 2)
```

Block comments can be nested, which is ocassionally useful when temporarily commenting code:

```rs
/*
  /* Bar */
  foo(1,2)
*/
```

Unbalanced block comments will raise an error:
```rs
/*
  /* This will fail. ✘
  foo(1,2)
*/
```

=== Shebangs <shebang-comment>

#syntax[
```py
shebangComment = '#!' .* EOL
```
]

Shebangs #footnote[https://en.wikipedia.org/wiki/Shebang_(Unix)] are a special type of line comment that starts with `#!` and has special meaning on UNIX systems: it tells the system how to run the script when executed:

```rs
#!/usr/bin/env cara
IO.println!("This is a script you can run via ./myScript.cara")
```

```bash
$ ./myScript.cara
This is a script you can run via ./myScript.cara
```

Without the shebang line the system wouldn't know how to run the script:

```bash
$ ./myScript.cara
./myScript.cara: line 1: syntax error near unexpected token `"This is a script you can run via ./myScript.cara"'
./myScript.cara: line 1: `IO.println!("This is a script you can run via ./myScript.cara")'
```

(Note you also need to make the script executable beforehand, eg. via `chmod +x ./myScript.cara`.)

Shebang can only be present as the very first bytes of the script:

```rs
// Welcome in my program
#!/home/martin/.bin/cara
// This will trigger an error
```

```rs
  #!/home/martin/.bin/cara
// This will trigger an error as well
```

= TODO Semantics
== TODO Effects
== TODO Type system

= TODO sections
== Record update
== Standard library
=== Maybe
=== Result
== GADTs
== Tests
== Function overloading
== Type classes
== Imports, exports and modules
== Type annotations
== Type aliases
== Sum types
== Pattern matching
== Pattern guards
== Where
