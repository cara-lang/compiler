WANTED:

* reference page (all on one page) - https://tour.gleam.run/everything/ - Cmd+F friendly

* arbitrary precision integers as the default (do we ever want 8-bit 16-bit ones etc?)

* record destructuring that allows renaming

* pattern match pinning? ^, allowing to check against variables and not just literals?

* case..of that takes the same pattern across multiple cases:
```
  match t with
  | ML.Var (pos, _) | ML.Hole (pos, _) | ML.Abs (pos, _, _)
  | ML.App (pos, _, _) | ML.Let (pos, _, _, _, _) | ML.Annot (pos, _, _)
  | ML.Tuple (pos, _) | ML.LetProd (pos, _, _, _)
  | ML.Variant (pos, _, _) | ML.Match (pos, _, _)
    -> pos
```

* String pattern matching ("ABC" ++ rest ++ "DEF"), similar to List pattern matching
    * TODO somehow also pattern match on a char? `'C' ++ str` seems easy enough but what about arbitrary char?

* `[]`(): the postfix "array-access" operator
    * [n], [-n], [m..n], [m...n], [str] 
    * examples: 
        * list[2]
        * list[-1]
        * list[2..3]
        * array[0]
        * array[-2]
        * tuple[2]
        * tuple[-n] probably not useful?
        * tuple[2..3] - dropping first 2 elements? is that doable?
        * string[0]
        * string[2]
        * string[0..2]
        * dict["xyz"]
        * record["abc"] ?? unsure if we can do that type-safely. probably not or only with literals

* functions can have multiple arities with different implementations (and return types)
  * (Seq.sum(lambda, seq) vs Seq.sum(seq))
  * can have the same arity multiple times, dispatching on the input types
* automatic tail recursion (Erlang/Elm style)
  * modulo cons? allowing for `go n = n :: go (n - 1)` to be tail-optimized
    * https://www.microsoft.com/en-us/research/uploads/prod/2022/07/trmc.pdf
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
    * in new syntax: foo : x:Int -> y:Int -> Int
  * return type cannot be omitted

* Extending others' modules with new functions and constants

* if ... then ... without else implicitly returns () from the else, which means the then... also needs to return ().
  * automatic monad-wrapping of the ()? That's probably the only place where this would make sense.

* deep record updates: {...old.x, foo: old.x.foo + 1}
* compile to HVM -> by proxy to native, parallel
* ranges: `1..5, 1...5, 1,3..8, 5..1, 5...1, 5,3..-8` = list-like things. Likely a Sequence protocol like Clojure has, and majority of stdlib working on sequences rather than lists?
* implements LSP(?) and Debug Adapter Protocol (to have VSCode/... debugger out of box)
  * https://microsoft.github.io/debug-adapter-protocol/
* holes? to aid programming and ask the typechecker for its opinion
  * might be cool if not only the compiler tells you, but the LSP tells you or the formatter replaces the hole with the needed type
* Fraction type?
* operator overloading probably done in the Kotlin way: operator fun plus, etc.
* Probably no <| pipelines? << and >> still might have their place. 
* inline pragmas? for Maybe.map to become tail-safe etc.

* Opaque types by default? Keyword for opt-in transparency instead of for opt-in opaqueness?

----------------------

lambda syntax like TS and Kotlin? `(i: X, j: Y) => expression`? Would be more familiar than Elm's `\i j -> expression` or Roc's `\i,j -> expression`

GADTs?

Would really love them to make more precise constructors and more powerful pattern matching:

(pseudo-Elm:)

    type Expr a where
      I : Int -> Expr Int
      B : Bool -> Expr Bool
      Add : Expr Int -> Expr Int -> Expr Int
      Mul : Expr Int -> Expr Int -> Expr Int
      Eq : Expr a -> Expr a -> Expr Bool

    eval : Expr a -> a      -- see? no `Maybe (Either Int Bool)`
    eval expr =
      case expr of
        I i -> i                        -- see?
        B b -> b                        -- these two rows?
        Add e1 e2 -> eval e1 + eval e2
        Mul e1 e2 -> eval e1 * eval e2
        Eq e1 e2 -> eval e1 == eval e2  -- see? no manual checking of impossible cases!


  * https://dev.realworldocaml.org/gadts.html
  * https://github.com/ollef/sixten?tab=readme-ov-file#algebraic-data-types-and-pattern-matching
  Can we even have GADTs with Hindley-Milner?
    * There is some issue with QuickCheck IIRC. `https://wiki.haskell.org/QuickCheck_/_GADT`
    * Might be useful for implementation: https://www.microsoft.com/en-us/research/publication/practical-type-inference-for-arbitrary-rank-types/

    * Slides: https://www.cis.upenn.edu/~sweirich/talks/icfp06-wobbly.pdf
    * Paper: https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/gadt-pldi.pdf
    * Seems like we need to enforce functions using case..of to have type annotations.
        * Eg: https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/gadt.html
        >  The general principle is this: type refinement is only carried out based on user-supplied type annotations. So if no type signature is supplied for eval, no type refinement happens, and lots of obscure error messages will occur.

----------------------

* automatic letrec
* explicit qualification like `List.map` etc. is preferable over typeclass-y `fmap`
* functions can be implicitly namespaced, as if methods on a type
  * Logger.pure(
  * `pure` and `bind` are used in the monad blocks `MonadName { ... }` by the compiler
  * can we later do something cool with the namespace itself?
    * see `monad-state` test, with `derive IdGen.each as Monad.each`
* dot notation: myXY.addX(1) if there is a fn XY.addX
    * do we want it?
    * it will undermine the `List.map` preference
* name collisions of type constructors can have collisions; they need to be distinguished like:
  * type X = A | B(Int)
  * type Y = A | B(Int)
  * foo = A // error
  * foo = X.A // ok!

* Automatic hole driven development / expansion?

TODO:
* check out OCaml modules
  * https://dev.realworldocaml.org/first-class-modules.html
  * https://www.reddit.com/r/adventofcode/comments/zvl018/comment/j1vhh48/?utm_source=reddit&utm_medium=web2x&context=3
  * OCaml modules can contain other modules as well
  * `open M` - basically unqualified import
  * `let open M in ...` - scoped unqualified import (would eg. be good in Elm's views?)
    * same effect as `Gen.(generate 10 bool)` but we probably don't want that
  * `module QR = QuickCheck.Runner` is basically Elm's `import ... as`
  * Apart from that OCaml has signatures (interfaces) and functors (module -> module functions)
  * we need a way to export only certain functions/types

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
* ! for do-notation: do we want to allow it anywhere, not just once per line?
    * `foo = f!(x!,y,z!)` translates to
      ```
      xx = x!
      zz = z!
      foo = f!(xx,y,zz)
      ```
* postfix op for seq access - desugar to `getAt`? Not all seqs would implement that
  * [1]      -> getAt(i: Int, coll: Seq(a)): Maybe(a) given ...
  * [-1]     -> -//-
  * [1..3]   -> getAt(r: Range, coll: Seq(a)): Maybe(a) given ...
  * [1,3..8] -> -//-
  * [3..] ? do we want to allow infinite ranges?
* collection API: https://doc.rust-lang.org/stable/std/iter/trait.Iterator.html
* collection API: HandAxe? https://www.youtube.com/watch?v=YDq251FbmK4

CURRENT THINKING:
* algebraic effects? have a specific way to say "no effect!" but otherwise the default is that whatever usages do, we do also? have a way to say "at least +Log", or "whatever, but disallow Log"?
  * OCaml 5 just got effects (via GADTs): https://v2.ocaml.org/releases/5.0/manual/effects.html

=============================

Data structures - Okasaki? Clojure impl?
Fast F# - "Writing a dictionary", fast hashing/... etc.
  - https://www.youtube.com/watch?v=Hd1XoubzdK4
  - https://github.com/matthewcrews/FastDictionaryTest

--------------------------
* `value1: Doc = ...` translates to:
  ```
  value1: Doc
  value1 = ...
  ```
--------------------------

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

-------------------

FOMO FROM OTHER LANGUAGES:

* OCaml modules
* Algebraic effects instead of monads?

-------------------

- ' and _ in identifiers
- ? and - in identifiers?

- eprintln
- check out jakt::libc::io. How will we do this in HVM?
- punning for function calls? (probably only makes sense if we forced f(x=1,y=2) in some places)
- auto-derived Printable/... (doesn't need an inverse Read instance)

- Seqs (eg. for infinite ranges): https://clojure.org/reference/sequences

----------------------------

Tests: "they don't run by default; but they are exposed values (if not `private`), they all compile down to (() -> TestResult) and you can run them from the REPL etc. or even use them in your program

-----------------------------

Interpreter: don't parse + interpret stdlib on each run. What are our options? Compiling to bytecode? Have the final (non-OCaml but HVM?) interpreter run on HVM code anyway, so the stdlib can be a precompiled HVM file to be concatenated to the rest?

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

We should allow the last bang (`x!(a,b)`) result to be returned from the expr block

---------------------------

Promises made:
- [ ] cara build (from Tests docs)
- [ ] cara run   (from Tests docs)
- [ ] cara test  (from Tests docs)
- [ ] lists have optional end and start , delimiter
- [ ] complex numbers in stdlib
- [ ] bigintegers in stdlib
- [ ] bigdecimals in stdlib
- [ ] fractions in stdlib
- [ ] int8, int16, int32, int64 in language
- [ ] uint8, uint16, uint32, uint64 in language
- [ ] float32, float64 in language



-----------------------------------

From core-lang.dev/design:

> "Always rules" are better than "almost rules":
> = assigns
> : ascribes
> @ annotates
> . selects
> () encloses values
> [] encloses types

- perhaps something similar should hold for Cara? "if I see @ it's always for XYZ"
-------------------------------

From ptls.dev/online/collatz.html:

output =
  iterate(step, 175) -- sequence starts at 175
  |> takeUntil(eq(1))
  |> scale(4)
  |> println

--> ASCII chart, how cool is that!

                                    ▂         █                                  
                                  ▁ █       ▅ █▁                                 
                                ▃ █▁█▅  ▂ ▆ █▃██▁                                
▁▂▁▃▂▄▂▆▃▂▅▃▂▁▂▁▃▂▄▂▁▃▂▄▂▁▃▂▅▃▇▄█▅████▆▃█▅█▇█████▄▂▆▃▂▄▂▁▃▂▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

Overall pointless is worth studying more: https://ptls.dev/docs.html
