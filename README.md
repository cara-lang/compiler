<img alt="Cara logo" src="https://github.com/cara-lang/compiler/raw/main/assets/logo.svg" width="196" height="196" />

Cara is a programming language!

Here are some of its characteristics:

* general-purpose
* functional
* pure
* immutable
* statically typed, with automatic type inference
* compiled via HVM for automatic parallelism...
* ...or interpreted (for scripting)

## Status

At the moment, Cara is in the design phase: I'm trying to figure out its features, syntax and behaviour and produce a grammar, a set of examples and end-to-end tests showing whole programs. You can see some in `end-to-end-tests/`.

## Examples

```rust
#!/usr/bin/env cara

dst = IO.ask!("Enter destination filename: ")
dstHandle = FS.open!(src, FS.Write)

timestampFmt = "hh:mm:ss.fff"

1..10 |> IO.forEach!(\i -> IO {
  time = Time.now!()
  dstHandle |> FS.write!("[${Time.format(timestampFmt, time)}] Hello number $i\n")
})
```

```rust
isPrime(n) =
  2..sqrt(n)
    |> Seq.any(#(n % _ == 0))

x = isPrime(1111111111111111111)

IO.println!(x) // -> True
```

```rust
quickSort : List[Int] -> List[Int]
quickSort([]) = []
quickSort([x,...xs]) = {
  (lt, gt) = List.partition(#(x >= _), xs)
  quickSort(lt) ++ x ++ quickSort(gt)
}

[5,1,3,2,4]
  |> quickSort
  |> IO.println!
```

```rust
type Maybe[a] =
  | Nothing
  | Just(a)

traverse: (a -> Maybe[b]) -> List[a] -> Maybe[List[b]]
traverse(fn,list) = {
  go([],bs) = Just(List.reverse(bs))
  go([a,...as],bs) = 
    case fn(a) of
      Nothing -> Nothing
      Just(b) -> go(as,b++bs)

  go(list,[])
}

xs = [1,2,3,4,5]
ys = [6,7,8,9,10]
f = \n -> if n == 3 then Nothing else Just(n)

IO.println!(xs |> traverse(f)) // -> Nothing
IO.println!(ys |> traverse(f)) // -> Just([6,7,8,9,10])
```

```rust
fizzbuzz(n) =
  if n % 15 == 0 then "FizzBuzz"
  else if n % 3 == 0 then "Fizz"
  else if n % 5 == 0 then "Buzz"
  else "$n"

1..20
  |> List.map(fizzbuzz)
  |> String.join(", ")
  |> IO.println!

// -> 1, 2, Fuzz, 4, Buzz, Fizz, 7, 8, Fizz, Buzz, 11, Fizz, 13, 14, FizzBuzz, 16, 17, Fizz, 19, Buzz
```
