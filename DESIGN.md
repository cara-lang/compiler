WANTED (but doesn't yet have tests):
* functions can have multiple arities with different implementations (and return types)
  * (Seq.sum(lambda, seq) vs Seq.sum(seq))
  * can have the same arity multiple times, dispatching on the input types
* strong stdlib
  * I have a much higher tolerance and need for `elm-community/*-extra`
  * Closer to Clojure than to Elm!
* automatic tail recursion (Erlang/Elm style)
  * modulo cons? allowing for `go n = n :: go (n - 1)` to be tail-optimized
    * https://www.microsoft.com/en-us/research/uploads/prod/2022/07/trmc.pdf
* pattern matching has `A|B|C -> ...`
* integer types: Int8, Int16, ..., Int64, BigInt
  * automatic upcasting from i32 to i64 etc
* float types: Float32, Float64, BigDecimal
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
* allow ' in var names

* Extending others' modules with new functions and constants
* `pub` for sharing that with everybody, without `pub` for just this module?


WANTED:
* ocaml has
    match foo with
      | X abc
      | Y def -> 123
      | Z ghi -> 234
  as "or-patterns"

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
  * https://dev.realworldocaml.org/gadts.html
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
  * https://dev.realworldocaml.org/first-class-modules.html
  * https://www.reddit.com/r/adventofcode/comments/zvl018/comment/j1vhh48/?utm_source=reddit&utm_medium=web2x&context=3
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
* unsure: do we want the order of definitions to not matter? Probably not
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
  * OCaml 5 just got effects (via GADTs): https://v2.ocaml.org/releases/5.0/manual/effects.html


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
7) Non-monadic exprs without an `x =` binding are only allowed as the last stmt
