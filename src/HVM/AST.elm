module HVM.AST exposing
    ( ADT
    , ADTConstructor
    , BinOp(..)
    , File
    , Pattern(..)
    , Rule
    , Term(..)
    , concatFiles
    , emptyFile
    )

{-| This module defines the HVM AST. It's not an exact port of
<https://github.com/HigherOrderCO/hvm-lang/blob/master/src/term/mod.rs>
but stays pretty close.
-}


type alias File =
    { rules : List Rule
    , adts : List ADT
    }


{-|

    (foo a b c) = (+ a (+ b c))
    -->
    { functionName = "foo"
    , args =
        [ PVar "a"
        , PVar "b"
        , PVar "c"
        ]
    , body =
        Opx
            { op = Add
            , left = Var "a"
            , right =
                Opx
                    { op = Add
                    , left = Var "b"
                    , right = Var "c"
                    }
            }

    }

    (foo a (Baz b) (Quux c d e)) = (+ a e)
    -->
    { functionName = "foo"
    , args =
        [ PVar "a"
        , PCtr "Baz" [PVar "b"]
        , PCtr "Quux" [PVar "c" "d" "e"]
        ]
    , body =
        Opx
            { op = Add
            , left = Var "a"
            , right = Var "e"
            }

    }

-}
type alias Rule =
    { functionName : String
    , args : List Pattern
    , body : Term
    }


{-|

    data Foo = Bar | (Baz a) | (Quux a b c)
    -->
    { name = "Foo"
    , constructors =
        [ { name = "Bar", arity = 0 }
        , { name = "Baz", arity = 1 }
        , { name = "Quux", arity = 3 }
        ]
    }

-}
type alias ADT =
    { name : String
    , constructors : List ADTConstructor
    }


type alias ADTConstructor =
    { name : String
    , arity : Int
    }


type BinOp
    = Add
    | Sub
    | Mul
    | Div
    | Mod
    | And
    | Or
    | Xor
    | Shl
    | Shr
    | Lte
    | Ltn
    | Eql
    | Gte
    | Gtn
    | Neq
    | Not


{-| Omitted: Chn (scopeless lambda)
Omitted: Lnk (use of a Channel variable)
-}
type Term
    = Lam
        { name : String
        , body : Term

        -- , tag : Tag
        }
    | Var String
    | Let
        { pat : Pattern
        , value : Term
        , next : Term
        }
    | App
        -- in HVM-Lang, this is a 1-arg application, but we don't care about that, the syntax allows multiple!
        { function : Term
        , args : List Term

        -- , tag : Tag
        }
    | Tup ( Term, Term )
      --| Dup
      --    { leftName : String
      --    , rightName : String
      --    , expr : Term
      --    , body : Term
      --    }
      --| Sup
      --    { left : Term
      --    , right : Term
      --    }
    | U60 Int
      --| F60 Float
    | Str String -- this technically lowers to SCons | SNil
    | Lst (List Term) -- this technically lowers to LCons | LNil
    | Opx
        { op : BinOp
        , left : Term
        , right : Term
        }
    | Match
        { value : Term
        , arms : List ( Pattern, Term )
        }
      --| Ref
    | Era


type Pattern
    = PWildcard
    | PVar String
    | PCtr String (List Pattern)
      --| PZero
      --| PUnnamedSucc
      --| PNamedSucc String
    | PTup ( Pattern, Pattern )
    | PList (List Pattern)


emptyFile : File
emptyFile =
    { adts = []
    , rules = []
    }


concatFiles : File -> File -> File
concatFiles f1 f2 =
    { adts = f1.adts ++ f2.adts
    , rules = f1.rules ++ f2.rules
    }
