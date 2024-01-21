module Common exposing
    ( namedTupleFields
    , tupleIndexToNumericField
    )

import BiDict exposing (BiDict)


namedTupleFields : BiDict String Int
namedTupleFields =
    BiDict.fromList
        [ ( "first", 0 )
        , ( "second", 1 )
        , ( "third", 2 )
        , ( "fourth", 3 )
        , ( "fifth", 4 )
        , ( "sixth", 5 )
        , ( "seventh", 6 )
        , ( "eighth", 7 )
        , ( "ninth", 8 )
        , ( "tenth", 9 )
        ]


tupleIndexToNumericField : Int -> String
tupleIndexToNumericField n =
    "el" ++ String.fromInt n
