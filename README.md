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

timestampFmt = "hh:mm:ss.fff"

(1..10).forEach(\i -> 
  time = Time.now()
  dstHandle |> FS.write("[${Time.format(timestampFmt, time)}] Hello number $i\n")
)
```

```cara
isPrime(n) =
  2..sqrt(n)
    |> Seq.any(n % _ == 0)

x = isPrime(1111111111111111111)

IO.inspect(x)
// -> True
```

```cara
quickSort(list: List(Int)): List(Int)
quickSort([]) = []
quickSort(x::xs) =
  (lt, gt) = List.partition((x >= _), xs)
  quickSort(lt) ++ x ++ quickSort(gt)

xs = [3,1,2,5,4]

IO.inspect(xs.quickSort())
// -> [1,2,3,4,5]
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

xs = [1,2,3,4,5]
ys = [6,7,8,9,10]
f = \n -> if n == 3 then Nothing else Just(n)

IO.inspect(xs.traverse(f)) // -> Nothing
IO.inspect(ys.traverse(f)) // -> Just([6,7,8,9,10])
```
