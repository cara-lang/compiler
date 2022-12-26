DEFINITELY:
* usable for scripting
  * allow shebang
* comments: // and /* ... */
* pure
* functional
* immutable
* strong static type system
  * automatic type inference (bidi? HM?)
* functions can have multiple arities with different implementations (and return types)
  * (Seq.sum(lambda, seq) vs Seq.sum(seq))
  * can have the same arity multiple times, dispatching on the input types
* strong stdlib
  * I have a much higher tolerance and need for `elm-community/*-extra`
  * Closer to Clojure than to Elm!
* ADTs
* anonymous records
  * types like:  { x: Int, y: Bool }
  * values like: { x: 123, y: False }
* record updates
  * {...old, x: 1} // sets the x field if it exists in `old` already
                   // adds the x field if it doesn't!
* record creation using locally available names
  * seed = 123
    acc = []
    {seed,acc} // -> { seed: 123, acc: [] }
* record unboxing
  * email(LoggedIn({..})) = Just(email) // <- {..} unboxes all the fields to locals
* unlimited length tuples
  * probably done via (a,b,c) being (a,(b,c)) under the hood
* lambdas - anonymous functions
  * \x -> 1 + x
* lambda shorthand: 
  * `(_ + 1) === \x -> x + 1`
  * `(_1 + _2) === \x y -> x + y`
  * The specific syntax might change, depending on how it plays with the rest, like implicit parens in pipelines
* automatic tail recursion (Erlang/Elm style)
  * modulo cons? allowing for `go n = n :: go (n - 1)` to be tail-optimized
* equational style (Haskell-like) as an alternative syntax to case..of
  * fib(0) = 0
    fib(1) = 1
    fib(n) = fib(n-1) + fib(n-2)
* pattern matching has `A|B|C -> ...`
* integer types: Int8, Int16, ..., Int64, BigInt
  * automatic upcasting from i32 to i64 etc
* float types: Float32, Float64, BigDecimal
* do notation or something similar
  * hopefully automatically inferred, no difference between let x = 1 and x <- y
* no let..in keywords, but let..in is implicit with whitespace
  * instead things follow each other sequentially, and the last thing is the returned item
  * fn(n) =
      y = n + 1
      x = y + 2
      x + y      // <- returned
  * out of order still possible with `where`
    * fn(n) =
        x + y
          where
            x = y + 2
            y = n + 1
* string interpolation with $ and ${...}:
  * "hello $foo" --> "hello world"
  * "hello ${user.name}" --> "hello Martin"
  * debug helper ${...=}
    * similar to Python f-string f'{abc=}'
    * "${foo=}" returns "foo=123"
    * "${user.name=}" returns "user.name=Martin"
* `|>` pipelines
  * They put the left side into the last position of right side by default
    * data |> x(2) --> x(2, data)
    * Can be overriden with _
      * data |> x(2,_,3) --> x(2,data,3)
* ++ for sequence-adding
  * with overloaded operators, it could be arbitrary:
    * A ++ [B,C,D] --> [A,B,C,D]
    * [A,B,C] ++ D --> [A,B,C,D]
    * [A,B]++[C,D] --> [A,B,C,D]
* type annotations
  * can be omitted if the function has single variant (arity + set of arg types + return type)
  * must be present if there are multiple variants, and must directly precede the function
  * either argument name can be omitted, or argument type can be omitted, but not both
  * arguments are enforced if there are multiple arguments of the same type
    * foo(Int, Int): Int       // <- disallowed
    * foo(x: Int, Int): Int    // <- disallowed
    * foo(x: Int, y: Int): Int // <- allowed
  * return type cannot be omitted
* Int == Int64
* Float == Float64
* number thousand separators (_ anywhere in a number)
* allow ' in var names
* Strings: seqs of extended grapheme clusters
  * https://manishearth.github.io/blog/2017/01/14/stop-ascribing-meaning-to-unicode-code-points/
  * check out how Swift and Perl 6 do things
  * Char = basically a string (not an integer)


WANTED:
* type _aliases_
* general purpose rather than HTML/JS (at least as first priority)
* no currying?
* give users access to the whole syntax (operators, implementing core typeclasses etc.)
* deep record updates: {...old.x, foo: old.x.foo + 1}
* compile to HVM -> by proxy to native, parallel
* no \case, we don't have currying?
* ranges: `1..5, 1...5, 1,3..8, 5..1, 5...1, 5,3..-8` = list-like things. Likely a Sequence protocol like Clojure has, and majority of stdlib working on sequences rather than lists?
* implements LSP(?) and Debug Adapter Protocol (to have VSCode/... debugger out of box)
  * https://microsoft.github.io/debug-adapter-protocol/
* holes? to aid programming and ask the typechecker for its opinion
* Fraction type?
* operator overloading probably done in the Kotlin way: operator fun plus, etc.
* Probably no <| pipelines? << and >> still might have their place. 
* 0x, 0b, 0o, floats scientific notation?
* inline pragmas? for Maybe.map to become tail-safe etc.
* holes syntax: would it be better to do \(_ + 1) instead of (_ + 1)?
* what if N-tuples are just syntax sugar for concrete record {el0,el1}, {el0,el1,el2}, etc.?
* GADTs?
* automatic letrec
* explicit qualification like `List.map` etc. is preferable over typeclass-y `fmap`
* functions can be implicitly namespaced, as if methods on a type
  * Logger.pure(
  * `pure` and `bind` are used in the monad blocks `MonadName { ... }` by the compiler
  * can we later do something cool with the namespace itself?
    * see `monad-state` test, with `derive IdGen.each as Monad.each`
  * allow unqualified dot notation on these? myXY.addX(1) if there is a fn XY.addX? and not for other unqualified fns?
* name collisions of type constructors can have collisions; they need to be distinguished like:
  * type X = A | B(Int)
  * type Y = A | B(Int)
  * foo = A // error
  * foo = X.A // ok!


TODO:
* Monad syntax ("computation expressions")... should we default to the await, and give a special not-so-often-used syntax to _not_ await?
* Consider Maybe[List[b]] instead of Maybe(List(b)) to distinguish fn annotations and fn headers a little?
* It should be possible to infer holes just from the presence of _, instead of from the extra parentheses. We shall see?
  * Seq.any(n % _ == 0)    // preferable to:
  * Seq.any((n % _ == 0))  // or even:
  * Seq.any(\(n % _ == 0))
* check out OCaml modules
* anonymous ADTs? check out open unions/tags in Roc/Grace/...
* default arguments?
* typeclasses? interfaces? protocols? late extension of those? (Clojure, Kotlin)
  * adhoc polymorphism: function overloading, sep. implementations for different types
  * parametric polymorphism: one function with one implementation working on many types
    * id : a -> a
    * map : (a -> b) -> [a] -> [b]
  * open: allow adding after the fact (not just near the type/class definition)
  * Haskell typeclasses: 0 or 1 implementations, never more (can be done with newtypes)
  * George Wilson: https://www.youtube.com/watch?v=2EdQFCP5mZ8
  * no hierarchy / inheritance; object conforms to a protocol only if it implements the contract
  * Haskell: dispatches on the return type as well. Clojure: dispatches only on first fn arg
  * Haskell: allows overloading even values, not just functions (minBound)
* extension functions / objects / vals
* unsure: imports: by default `import SplitMix` implies `exposing (SplitMix)` if the module exposes that. (Should this default/principal type to import be defined in the imported module?)
* example of scripting main that takes cmdline args, and perhaps reads 
* guard syntax for equational style? perhaps not?
* does dot syntax putting things on the left instead of on the right (like |>) play nice? Isn't it contradictory? Which way wins in the stdlib? Should those be unified?
* does ordered let together with implicit main mean top level declarations are ordered like F# is?
* how to do multiline strings?
* `as` to be an allowed identifier
* dot syntax
  * can't really be used with qualified functions... in which scenarios would it be useful then? Would it have some automatic resolution?
    * dstHandle.write("...") == FS.write("...", dstHandle) ?
  * It still seems very elegant for functions written in this module. Perhaps let's keep it :)
  * --------------
  * a single-line alternative to when |> would be too verbose / vertically heavy
  * encouraged as an alternative to stacking functions g(f(x)), combats the lack of <| or $ a little
  * `data.x().y()` means `y(x(data))`
  * `data.x(1).y(2,3)` means `y(2,3,x(1,data))`
  * as with |>, can be overriden with _
    * `data.x(_,1).y(2,_,3)` means `y(2,x(data,1),3)`
* postfix op for seq access - desugar to `getAt`? Not all seqs would implement that
  * [1]      -> getAt(i: Int, coll: Seq(a)): Maybe(a) given ...
  * [-1]     -> -//-
  * [1..3]   -> getAt(r: Range, coll: Seq(a)): Maybe(a) given ...
  * [1,3..8] -> -//-
  * [3..] ? do we want to allow infinite ranges?
* collection API: HandAxe? https://www.youtube.com/watch?v=YDq251FbmK4

CURRENT THINKING:
* algebraic effects? have a specific way to say "no effect!" but otherwise the default is that whatever usages do, we do also? have a way to say "at least +Log", or "whatever, but disallow Log"?


----------------


WHAT'S MISSING FROM ELM:

* plugging into comparable, appendable, ...
* custom operators
* custom everything (full language unlocked -- effect modules etc.)
* deriving implementations of Functor etc.
* GADTs
* some form of do notation?
* A|B|C -> or patterns
* equational fn declarations
* lambda holes
* Extension functions / values


WHAT'S GOOD ABOUT ELM:

* pipeline style
* explicit function qualification
* immutability
* purity


FOMO FROM OTHER LANGUAGES:

* OCaml modules
* Algebraic effects instead of monads?

-----------------------------

Current thinking about monads:

Monad syntax sugar:

1) Monad-y code needs to be run inside MonadName { ... } blocks.
2) MonadName { ... } blocks are available if these functions are in scope:
     MonadName.pure
     MonadName.bind
3) IO { ... } is implicit if omitting main().
4) If a monadic value is to be left "unwrapped",
     use x = monadicValue
      or x = monadicValue(x,y,z) if creating it needs arguments.
5) If a monadic value is to be executed,
     use x = monadicValue!
      or x = monadicValue!(x,y,z) if creating it needs arguments.
6) If not binding the result to a name, `_ =` can be omitted:
     log!("hello") // is the same as
     _ = log!("hello")
