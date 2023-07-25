module Desugar.CaseOf exposing (desugarCaseOf, desugarTypeDecl)

{-| Use [Scott encoding](https://crypto.stanford.edu/~blynn/compiler/scott.html)
to convert `case..of` expressions to a `Foo.match` function call.

Example:

     type Foo =
         | Bar
         | Baz(Int)
         | Quux(Bool,String)

should desugar into:

     Foo.match : Foo -> a -> (Int -> a) -> (Bool -> String -> a) -> a
     Foo.match(Bar,      onBar,onBaz,onQuux) = onBar
     Foo.match(Baz(a),   onBar,onBaz,onQuux) = onBaz(a)
     Foo.match(Quux(a,b),onBar,onBaz,onQuux) = onQuux(a,b)

and this usage of case..of:

     num val =
       case val of
         Bar -> 1
         Baz(n) -> n
         Quux(_,_) -> 2

should get desugared into:

     num(val) = Foo.match(val, 1, \n -> n, \_,_ -> 2)

This is compatible with HVM, since HVM has its own pattern matching and we don't
need to desugar any further.

---

The only wrinkle in this plan is with nested pattern matching:

     type Maybe[a] = Nothing | Just(a)

     case nestedMaybe of
         Nothing -> 1
         Just(Nothing) -> 2
         Just(Just(n)) -> n

This should get desugared into:

     Maybe.match(nestedMaybe, 1, \x -> Maybe.match(x, 2, \n -> n))

Which suggests we should first decompose the case..of into two nested ones?

     case nestedMaybe of
         Nothing -> 1
         Just(x) ->
            case x of
                Nothing -> 2
                Just(n) -> n

-}

import AST.Backend as B
import AST.Frontend as F


{-| Generate the Scott encoding function.
TODO: use the TypeModifier?
-}
desugarTypeDecl :
    { mod : TypeModifier
    , name : String
    , vars : List String
    , constructors : List Constructor
    }
    -> B.Expr
desugarTypeDecl r =
    Debug.todo "desugar type decl"


{-| Convert the case..of to a `MyType.match` function call.
-}
desugarCaseOf : { subject : Expr, branches : List CaseBranch } -> B.Expr
desugarCaseOf r =
    Debug.todo "desugar case..of"
