module Parser.Internal exposing
    ( Parser
    , succeed, fail, failUnrecoverably
    , map, map2, andThen, skip, keep
    , many, manyUntilEOF
    , separatedList, separatedNonemptyList
    , maybe, butNot, butNot_, butNotU, butNotU_, disallowed
    , lazy, inContext
    , isAtEnd, skipEol, skipEolBeforeIndented
    , token, tokenData, peekToken, peekTokenAfterEol, ifNextIs
    , getTokens, moveLeft, moveRight, rewindTo
    , log, logCurrent, logCurrentBefore, logCurrentAround, logCurrentAfter
    , TokenPred(..), oneOf
    , InfixParserTable, InfixParser, pratt
    )

{-|

@docs Parser
@docs succeed, fail, failUnrecoverably
@docs map, map2, andThen, skip, keep
@docs many, manyUntilEOF
@docs separatedList, separatedNonemptyList
@docs maybe, butNot, butNot_, butNotU, butNotU_, disallowed
@docs lazy, inContext
@docs isAtEnd, skipEol, skipEolBeforeIndented
@docs token, tokenData, peekToken, peekTokenAfterEol, ifNextIs
@docs getTokens, moveLeft, moveRight, rewindTo
@docs log, logCurrent, logCurrentBefore, logCurrentAround, logCurrentAfter
@docs TokenPred, oneOf
@docs InfixParserTable, InfixParser, pratt

-}

import Error exposing (ParserContext, ParserError(..))
import List.Zipper as Zipper exposing (Zipper)
import Loc exposing (Loc)
import Token exposing (Token, Type(..))


type ErrorSeverity
    = RecoverableError
    | UnrecoverableError


type alias Parser a =
    Zipper Token
    -> List ParserContext
    -> Result Error ( a, Zipper Token, List ParserContext )


type alias Error =
    { loc : Loc
    , error : ParserError
    , severity : ErrorSeverity
    , context : List ParserContext
    }


type alias InfixParser a =
    { left : a
    , precedence : Int
    , isRight : Bool
    }
    -> Parser a


type alias InfixParserTable a =
    Token.Type
    -> { skippedEol : Bool }
    -> Maybe (InfixParserCase a)


type alias InfixParserCase a =
    { precedence : Int
    , isRight : Bool
    , parser : InfixParser a
    }


succeed : a -> Parser a
succeed a =
    \tokens context -> Ok ( a, tokens, context )


fail : ParserError -> Parser a
fail err =
    \tokens context -> fail_ tokens context err


fail_ : Zipper Token -> List ParserContext -> ParserError -> Result Error a
fail_ tokens context err =
    Err
        { loc = (Zipper.current tokens).loc
        , error = err
        , severity = RecoverableError
        , context = context
        }


{-| Bails out of any `oneOf`s.
Reports error directly to user.
-}
failUnrecoverably : ParserError -> Parser a
failUnrecoverably err =
    \tokens context -> failUnrecoverably_ tokens context err


failUnrecoverably_ : Zipper Token -> List ParserContext -> ParserError -> Result Error a
failUnrecoverably_ tokens context err =
    Err
        { loc = (Zipper.current tokens).loc
        , error = err
        , severity = UnrecoverableError
        , context = context
        }


map : (a -> b) -> Parser a -> Parser b
map fn parser =
    \tokens context ->
        parser tokens context
            |> Result.map (\( a, b, c ) -> ( fn a, b, c ))


map2 : (a -> b -> c) -> Parser a -> Parser b -> Parser c
map2 fn pA pB =
    \tokens context ->
        pA tokens context
            |> Result.andThen
                (\( a, tokensA, contextA ) ->
                    pB tokensA contextA
                        |> Result.map
                            (\( b, tokensB, contextB ) ->
                                ( fn a b, tokensB, contextB )
                            )
                )


andThen : (a -> Parser b) -> Parser a -> Parser b
andThen fn parser =
    \tokens context ->
        case parser tokens context of
            Ok ( a, tokens_, context_ ) ->
                fn a tokens_ context_

            Err err ->
                Err err


isAtEnd : Zipper Token -> Bool
isAtEnd tokens =
    (Zipper.current tokens).type_ == EOF


skipEol : Parser ()
skipEol =
    \tokens context ->
        Ok
            ( ()
            , tokens
                |> Zipper.find (\t -> t.type_ /= EOL)
                -- TODO maybe return an error instead?
                |> Maybe.withDefault tokens
            , context
            )


skipEolBeforeIndented : Parser ()
skipEolBeforeIndented =
    \tokens context ->
        let
            go : Zipper Token -> Zipper Token
            go currentTokens =
                case
                    ( (Zipper.current currentTokens).type_
                    , Zipper.next currentTokens
                    )
                of
                    ( EOL, Just next ) ->
                        if (Zipper.current next).loc.col > 1 then
                            -- next is indented, continue skipping!
                            go next

                        else
                            -- next is not indented, stop
                            currentTokens

                    _ ->
                        -- either not EOL or we're at the last token, stop
                        currentTokens

            finalTokens : Zipper Token
            finalTokens =
                go tokens
        in
        Ok ( (), finalTokens, context )


{-| Consumes 0+ children.
Never raises an error.
In case a child raises an error, stops looping.
-}
many : Parser a -> Parser (List a)
many childParser =
    \tokens context ->
        let
            go : List a -> Parser (List a)
            go acc tokens_ context_ =
                case childParser tokens_ context_ of
                    Err err ->
                        case err.severity of
                            RecoverableError ->
                                Ok ( List.reverse acc, tokens_, context_ )

                            UnrecoverableError ->
                                Err err

                    Ok ( child, tokens__, context__ ) ->
                        go (child :: acc) tokens__ context__
        in
        go [] tokens context


{-| Consumes 0+ children.
Either ends at EOF or re-raises the child's error.
-}
manyUntilEOF : Parser a -> Parser (List a)
manyUntilEOF childParser =
    \tokens context ->
        let
            go : List a -> Parser (List a)
            go acc tokens_ context_ =
                if isAtEnd tokens_ then
                    Ok ( List.reverse acc, tokens_, context_ )

                else
                    case childParser tokens_ context_ of
                        Err err ->
                            Err err

                        Ok ( child, tokens__, context__ ) ->
                            go (child :: acc) tokens__ context__
        in
        go [] tokens context


{-|

    : left (item (sep item)*)? right

-}
separatedList :
    { left : Token.Type
    , right : Token.Type
    , sep : Token.Type
    , item : Parser a
    , skipEol : Bool
    , allowTrailingSep : Bool
    }
    -> Parser (List a)
separatedList config =
    let
        eol =
            if config.skipEol then
                skipEol

            else
                succeed ()

        trailingSep =
            if config.allowTrailingSep then
                succeed identity
                    |> skip (maybe (token config.sep))
                    |> skip eol
                    |> keep (succeed ())

            else
                succeed ()
    in
    succeed identity
        |> skip (token config.left)
        |> skip eol
        |> keep
            -- (item (sep item)*)?
            (maybe
                -- item (sep item)*
                (succeed (::)
                    |> keep config.item
                    |> skip eol
                    |> keep
                        -- (sep item)*
                        (many
                            -- sep item
                            (succeed identity
                                |> skip eol
                                |> skip (token config.sep)
                                |> skip eol
                                |> keep config.item
                            )
                        )
                )
                |> map (Maybe.withDefault [])
            )
        |> skip eol
        |> skip trailingSep
        |> skip (token config.right)


{-|

    : left item (sep item)* right

-}
separatedNonemptyList :
    { left : Token.Type
    , right : Token.Type
    , sep : Token.Type
    , item : Parser a
    , skipEol : Bool
    , allowTrailingSep : Bool
    }
    -> Parser ( a, List a )
separatedNonemptyList config =
    let
        eol =
            if config.skipEol then
                skipEol

            else
                succeed ()

        trailingSep =
            if config.allowTrailingSep then
                succeed identity
                    |> skip (maybe (token config.sep))
                    |> skip eol
                    |> keep (succeed ())

            else
                succeed ()
    in
    succeed Tuple.pair
        |> skip (token config.left)
        |> skip eol
        |> keep config.item
        |> keep
            -- (sep item)*
            (many
                -- sep item
                (succeed identity
                    |> skip eol
                    |> skip (token config.sep)
                    |> skip eol
                    |> keep config.item
                )
            )
        |> skip eol
        |> skip trailingSep
        |> skip (token config.right)


{-| Doesn't advance the tokens stream.

A more optimized/lazier variant of Parser.maybe?

-}
ifNextIs : Token.Type -> Parser a -> Parser (Maybe a)
ifNextIs token_ parser =
    \tokens context ->
        if (Zipper.current tokens).type_ == token_ then
            parser tokens context
                |> Result.map (\( a, b, c ) -> ( Just a, b, c ))

        else
            Ok ( Nothing, tokens, context )


type TokenPred
    = P (Token.Type -> Bool)
    | T Token.Type


{-| Commited: if the prefix agrees, the parser will be tried as the only option.
If the parser fails, the error of that parser will be re-raised.

Noncommited: these will be tried one after another until one succeeds.
If the oneOf list runs out, an error will be raised that got the furthest Loc-wise.

-}
oneOf :
    { commited : List ( List TokenPred, Parser a )
    , noncommited : List (Parser a)
    }
    -> Parser a
oneOf config =
    if config == { commited = [], noncommited = [] } then
        fail EmptyOneOf

    else
        \tokens context ->
            -- Try commited first: pick the first commited parser whose prefix agrees with the current tokens
            case oneOfCommited tokens config.commited of
                Nothing ->
                    -- If none agrees, just pick the first noncommited parser that succeeds.
                    oneOfNoncommited config.noncommited tokens context

                Just parser ->
                    parser tokens context


oneOfCommited : Zipper Token -> List ( List TokenPred, Parser a ) -> Maybe (Parser a)
oneOfCommited tokens commited =
    case commited of
        [] ->
            Nothing

        ( prefix, parser ) :: rest ->
            if prefixMatches prefix (Just tokens) then
                Just parser

            else
                oneOfCommited tokens rest


prefixMatches : List TokenPred -> Maybe (Zipper Token) -> Bool
prefixMatches prefix tokens =
    case prefix of
        [] ->
            True

        (T wantedType) :: rest ->
            if Just wantedType == Maybe.map (Zipper.current >> .type_) tokens then
                prefixMatches rest (tokens |> Maybe.andThen Zipper.next)

            else
                False

        (P pred) :: rest ->
            if Just True == Maybe.map (Zipper.current >> .type_ >> pred) tokens then
                prefixMatches rest (tokens |> Maybe.andThen Zipper.next)

            else
                False


oneOfNoncommited : List (Parser a) -> Parser a
oneOfNoncommited noncommited =
    \tokens context ->
        let
            go :
                Maybe ( Loc, ParserError )
                -> List (Parser a)
                -> List ParserContext
                -> Result Error ( a, Zipper Token, List ParserContext )
            go furthestError parsers context_ =
                case parsers of
                    [] ->
                        case furthestError of
                            Nothing ->
                                fail_ tokens context_ OneOfDidntMatchAnyCommited

                            Just ( _, error ) ->
                                fail_ tokens context_ error

                    parser :: rest ->
                        case parser tokens context_ of
                            Err err ->
                                case err.severity of
                                    RecoverableError ->
                                        let
                                            newFurthestError : ( Loc, ParserError )
                                            newFurthestError =
                                                case furthestError of
                                                    Nothing ->
                                                        ( err.loc, err.error )

                                                    Just ( oldLoc, oldErr ) ->
                                                        if Loc.compare oldLoc err.loc == LT then
                                                            -- old < new, new is further
                                                            ( err.loc, err.error )

                                                        else
                                                            ( oldLoc, oldErr )
                                        in
                                        go (Just newFurthestError) rest context_

                                    UnrecoverableError ->
                                        Err err

                            Ok ok ->
                                Ok ok
        in
        go Nothing noncommited context


type Step state a
    = Loop state
    | Done a


loop : state -> (state -> Parser (Step state a)) -> Parser a
loop state callback tokens context =
    case callback state tokens context of
        Err err ->
            Err err

        Ok ( step, newTokens, newContext ) ->
            case step of
                Loop newState ->
                    loop newState callback newTokens newContext

                Done result ->
                    Ok ( result, newTokens, newContext )


pratt :
    { isRight : Bool
    , precedence : Int
    , prefix : Parser a
    , infix : InfixParserTable a
    }
    -> Parser a
pratt config =
    let
        initPrecedence : Int
        initPrecedence =
            if config.isRight then
                config.precedence - 1

            else
                config.precedence
    in
    config.prefix
        |> andThen
            (\prefix ->
                let
                    go : a -> Maybe (InfixParserCase a) -> Parser a
                    go left maybeInfix =
                        case maybeInfix of
                            Nothing ->
                                succeed left

                            Just infix ->
                                if initPrecedence < infix.precedence then
                                    succeed identity
                                        |> skip skipEolBeforeIndented
                                        |> skip moveRight
                                        |> keep
                                            (infix.parser
                                                { left = left
                                                , precedence = infix.precedence
                                                , isRight = infix.isRight
                                                }
                                            )
                                        |> andThen
                                            (\newLeft ->
                                                oneOf
                                                    { commited = []
                                                    , noncommited =
                                                        [ succeed
                                                            (\nextToken nextNonEolToken ->
                                                                go newLeft
                                                                    (config.infix
                                                                        nextNonEolToken
                                                                        { skippedEol = nextToken == EOL }
                                                                    )
                                                            )
                                                            |> keep peekToken
                                                            |> keep peekTokenAfterEol
                                                            |> andThen identity
                                                        , succeed newLeft
                                                        ]
                                                    }
                                            )

                                else
                                    succeed left
                in
                succeed
                    (\nextToken nextNonEolToken ->
                        go prefix
                            (config.infix
                                nextNonEolToken
                                { skippedEol = nextToken == EOL }
                            )
                    )
                    |> keep peekToken
                    |> keep peekTokenAfterEol
                    |> andThen identity
            )


moveLeft : Parser ()
moveLeft =
    \tokens context ->
        case Zipper.previous tokens of
            Nothing ->
                fail_ tokens context CouldntMoveLeft

            Just prevTokens ->
                Ok ( (), prevTokens, context )


moveRight : Parser ()
moveRight =
    \tokens context ->
        case Zipper.next tokens of
            Nothing ->
                fail_ tokens context RanPastEndOfTokens

            Just nextTokens ->
                Ok ( (), nextTokens, context )


token : Token.Type -> Parser ()
token type_ =
    \tokens context ->
        case Zipper.next tokens of
            Nothing ->
                fail_ tokens context RanPastEndOfTokens

            Just nextTokens ->
                if (Zipper.current tokens).type_ == type_ then
                    Ok ( (), nextTokens, context )

                else
                    fail_ tokens context (ExpectedToken type_)


tokenData : (Token.Type -> Maybe a) -> Parser a
tokenData getter =
    \tokens context ->
        case Zipper.next tokens of
            Nothing ->
                fail_ tokens context RanPastEndOfTokens

            Just nextTokens ->
                case getter (Zipper.current tokens).type_ of
                    Nothing ->
                        fail_ tokens context CouldntGetTokenData

                    Just data ->
                        Ok ( data, nextTokens, context )


skip : Parser this -> Parser prev -> Parser prev
skip this prev =
    map2 (\p _ -> p) prev this


keep : Parser this -> Parser (this -> next) -> Parser next
keep this fnP =
    map2 (\fn t -> fn t) fnP this


maybe : Parser a -> Parser (Maybe a)
maybe parser =
    \tokens context ->
        case parser tokens context of
            Err _ ->
                Ok ( Nothing, tokens, context )

            Ok ( a, nextTokens, nextContext ) ->
                Ok ( Just a, nextTokens, nextContext )


butNot : a -> ParserError -> Parser a -> Parser a
butNot disallowedValue err parser =
    \tokens context ->
        parser tokens context
            |> Result.andThen
                (\( value, newTokens, newContext ) ->
                    if value == disallowedValue then
                        fail_ tokens context err

                    else
                        Ok ( value, newTokens, newContext )
                )


butNot_ : (a -> Bool) -> ParserError -> Parser a -> Parser a
butNot_ disallowedPred err parser =
    \tokens context ->
        parser tokens context
            |> Result.andThen
                (\( value, newTokens, newContext ) ->
                    if disallowedPred value then
                        fail_ tokens context err

                    else
                        Ok ( value, newTokens, newContext )
                )


{-| Unrecoverable variant of `butNot`
-}
butNotU : a -> ParserError -> Parser a -> Parser a
butNotU disallowedValue err parser =
    \tokens context ->
        parser tokens context
            |> Result.andThen
                (\( value, newTokens, newContext ) ->
                    if value == disallowedValue then
                        failUnrecoverably_ tokens context err

                    else
                        Ok ( value, newTokens, newContext )
                )


{-| Unrecoverable variant of `butNot_`
-}
butNotU_ : (a -> Bool) -> ParserError -> Parser a -> Parser a
butNotU_ disallowedPred err parser =
    \tokens context ->
        parser tokens context
            |> Result.andThen
                (\( value, newTokens, newContext ) ->
                    if disallowedPred value then
                        failUnrecoverably_ tokens context err

                    else
                        Ok ( value, newTokens, newContext )
                )


disallowed : (a -> ParserError) -> Parser a -> Parser b
disallowed err parser =
    parser
        |> andThen (\a -> failUnrecoverably (err a))


peekToken : Parser Token.Type
peekToken =
    \tokens context ->
        Ok ( (Zipper.current tokens).type_, tokens, context )


peekTokenAfterEol : Parser Token.Type
peekTokenAfterEol =
    \tokens context ->
        case Zipper.find (\t -> t.type_ /= EOL) tokens of
            Nothing ->
                fail RanPastEndOfTokens tokens context

            Just newTokens ->
                Ok
                    ( (Zipper.current newTokens).type_
                    , -- intentionally the old ones:
                      tokens
                    , context
                    )


log : String -> Parser a -> Parser a
log label parser =
    map (Debug.log label) parser


logCurrent : String -> Parser ()
logCurrent label =
    succeed ()
        |> skip (peekToken |> map (Debug.log label))


logCurrentBefore : String -> Parser a -> Parser a
logCurrentBefore label parser =
    succeed identity
        |> skip (logCurrent label)
        |> keep parser


logCurrentAfter : String -> Parser a -> Parser a
logCurrentAfter label parser =
    succeed identity
        |> keep parser
        |> skip (logCurrent label)


logCurrentAround : String -> Parser a -> Parser a
logCurrentAround label parser =
    succeed identity
        |> skip (logCurrent (label ++ " - before"))
        |> keep parser
        |> skip (logCurrent (label ++ " - after"))


lazy : (() -> Parser a) -> Parser a
lazy fn =
    \tokens context ->
        fn () tokens context


inContext : ParserContext -> Parser a -> Parser a
inContext context parser =
    \tokens oldContext ->
        parser tokens (context :: oldContext)
            |> Result.map (\( a, t, _ ) -> ( a, t, oldContext ))


getTokens : Parser (Zipper Token)
getTokens =
    \tokens context -> Ok ( tokens, tokens, context )


rewindTo : Zipper Token -> Parser ()
rewindTo tokens =
    \_ context -> Ok ( (), tokens, context )
