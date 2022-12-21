<img alt="Cara logo" src="https://github.com/cara-lang/compiler/raw/main/assets/logo.svg" width="196" height="196" />

Cara is a programming language!

Here are some of its characteristics:

* general-purpose
* functional
* pure
* immutable
* statically typed, with automatic type inference
* compiled and interpreted
* good for scripting

## Status

At the moment, Cara is in the design phase: I'm trying to figure out its features, syntax and behaviour and produce a grammar, a set of examples and end-to-end tests showing whole programs. You can see some in `examples/` and `tests/end-to-end/`.

## Examples

```cara
#!/usr/bin/env cara

dst = IO.ask("Enter destination filename: ")
dstHandle = FS.open(src, IO.Write)

(1..10).forEach(\i -> 
  time = Time.now()
  FS.write("[${Time.format("hh:mm:ss.fff")}] Hello number $i\n")
)
```

```cara
isPrime(n) =
  2..sqrt(n)
    |> Seq.any(n % _ == 0)

x = isPrime(1111111111111111111)
IO.println(x)
// -> True
```

```cara
quickSort(list: List(Int)): List(Int)
quickSort([]) = []
quickSort(x::xs) =
  (lt, gt) = List.partition((x >= _), xs)
  quickSort(lt) ++ x ++ quickSort(gt)
```

```cara
type Maybe(a) =
  | Nothing
  | Just(a)

traverse(fn: a -> Maybe(b), list: List(a)): Maybe(List(b))
traverse(fn,list) = go(list,[])
  where
    go([],bs) = Just(List.reverse(bs))
    go(a::as,bs) = 
      case fn(a) of
        Nothing -> Nothing
        Just(b) -> go(as,b::bs)
```
