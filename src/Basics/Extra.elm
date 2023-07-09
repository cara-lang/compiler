module Basics.Extra exposing (doNTimes)


doNTimes : Int -> (a -> a) -> a -> a
doNTimes n fn a =
    if n <= 0 then
        a

    else
        doNTimes (n - 1) fn (fn a)
