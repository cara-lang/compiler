module Lexer exposing (lex)

import Char.Extra as Char
import Error exposing (LexerError(..))
import Loc exposing (Loc)
import ParseInt
import String.Extra as String
import Token exposing (Token, Type(..))


type TokenResult
    = GotToken Token.Type Int Int Int
    | GotNothing Int Int Int
    | GotError ( Loc, LexerError )


lex : String -> Result ( Loc, LexerError ) (List Token)
lex source =
    let
        go : Int -> Int -> Int -> List Token -> Result ( Loc, LexerError ) (List Token)
        go i row col tokens =
            case nextToken source i row col of
                GotError ( loc, e ) ->
                    Err ( loc, e )

                GotNothing i_ row_ col_ ->
                    go i_ row_ col_ tokens

                GotToken next i_ row_ col_ ->
                    let
                        newTokens : List Token
                        newTokens =
                            { type_ = next

                            -- A bit surprising on first look:
                            , loc = { row = row, col = col }
                            }
                                :: tokens
                    in
                    if next == EOF then
                        Ok (List.reverse newTokens)

                    else
                        go i_ row_ col_ newTokens
    in
    go 0 1 1 []


err : Int -> Int -> LexerError -> TokenResult
err row col error =
    GotError
        ( { row = row, col = col }
        , error
        )


nextToken : String -> Int -> Int -> Int -> TokenResult
nextToken source i row col =
    case String.at i source of
        Nothing ->
            GotToken EOF i row col

        Just c ->
            let
                newI =
                    i + 1

                newCol =
                    col + 1

                variants_ =
                    variants source newI row newCol

                token t =
                    GotToken t newI row newCol
            in
            case c of
                '+' ->
                    variants_
                        [ ( "+", PlusPlus ) ]
                        Plus

                '-' ->
                    variants_
                        [ ( ">", Arrow ) ]
                        Minus

                '*' ->
                    variants_
                        [ ( "*", Power ) ]
                        Times

                '%' ->
                    token Percent

                '<' ->
                    variants_
                        [ ( "<", Shl )
                        , ( "=", Lte )
                        ]
                        Lt

                '>' ->
                    variants_
                        [ ( ">>", Shru )
                        , ( ">", Shr )
                        , ( "=", Gte )
                        ]
                        Gt

                '^' ->
                    token Caret

                '&' ->
                    variants_
                        [ ( "&", AndAnd ) ]
                        And

                '|' ->
                    variants_
                        [ ( "|", OrOr )
                        , ( ">", Pipeline )
                        ]
                        Pipe

                '=' ->
                    variants_
                        [ ( "=", EqEq ) ]
                        Eq

                '!' ->
                    variants_
                        [ ( "=", Neq ) ]
                        Bang

                '~' ->
                    token Tilde

                '\\' ->
                    token Backslash

                '(' ->
                    token LParen

                ')' ->
                    token RParen

                '{' ->
                    token LBrace

                '}' ->
                    token RBrace

                '[' ->
                    token LBracket

                ']' ->
                    token RBracket

                ',' ->
                    token Comma

                ':' ->
                    if (match ":" source newI row newCol).matches then
                        GotToken ColonColon (newI + 1) row (newCol + 1)

                    else
                        token Colon

                '/' ->
                    if (match "/" source newI row newCol).matches then
                        lineComment source (newI + 1) row (newCol + 1)

                    else if (match "*" source newI row newCol).matches then
                        case blockComment source (newI + 1) row (newCol + 1) of
                            Ok ( i_, row_, col_ ) ->
                                nextToken source i_ row_ col_

                            Err e ->
                                GotError e

                    else
                        token Div

                '#' ->
                    case String.at newI source of
                        Just '!' ->
                            shebang source (newI + 1) row (newCol + 1)

                        Just '(' ->
                            GotToken LHole (newI + 1) row (newCol + 1)

                        _ ->
                            err row newCol <| UnexpectedChar '#'

                '\n' ->
                    eol source newI row

                -- \r
                '\u{000D}' ->
                    let
                        result =
                            match "\n" source newI row newCol

                        ( i_, row_ ) =
                            ( result.i, result.row )
                    in
                    eol source i_ row_

                '_' ->
                    case simpleInt source newI row newCol of
                        Nothing ->
                            GotToken Underscore newI row newCol

                        Just ( int, ( intI, intRow, intCol ) ) ->
                            GotToken (Hole int) intI intRow intCol

                '.' ->
                    let
                        ddd =
                            match ".." source newI row newCol
                    in
                    if ddd.matches then
                        GotToken DotDotDot ddd.i ddd.row ddd.col

                    else
                        let
                            dd =
                                match "." source newI row newCol
                        in
                        if dd.matches then
                            GotToken DotDot dd.i dd.row dd.col

                        else
                            case lowerName source newI row newCol of
                                Err _ ->
                                    err row col <| UnexpectedChar '.'

                                Ok ( lower_, ( i_, row_, col_ ) ) ->
                                    GotToken (Getter lower_) i_ row_ col_

                '\'' ->
                    char source newI row newCol

                '"' ->
                    string source newI row newCol

                '`' ->
                    multilineString source newI row newCol

                ' ' ->
                    GotNothing newI row newCol

                '\t' ->
                    GotNothing newI row newCol

                _ ->
                    if Char.isLower c then
                        lower source i row col

                    else if Char.isUpper c then
                        upper source i row col

                    else if Char.isDigit c then
                        number source i row col

                    else
                        err row col (UnexpectedChar c)


{-| TODO this would be nicer if we parameterized TokenResult over the data.
This would be TokenResult Int and others would be TokenResult Token.Type
-}
simpleInt : String -> Int -> Int -> Int -> Maybe ( Int, ( Int, Int, Int ) )
simpleInt source i row col =
    let
        ( intString, ( i_, row_, col_ ) ) =
            consumeWhile Char.isDigit source i row col
    in
    -- TODO: make sure there are no letters afterwards (throw a LexerError if there are)
    String.toInt intString
        |> Maybe.map (\int -> ( int, ( i_, row_, col_ ) ))


shebang : String -> Int -> Int -> Int -> TokenResult
shebang source i row col =
    -- i,row,col start _after_ #!
    if i /= 2 then
        err row (col - 2) ShebangIsNotFirst

    else
        let
            ( i_, row_, col_ ) =
                skipUntilNewline source i row col
        in
        GotToken EOL i_ row_ col_


lineComment : String -> Int -> Int -> Int -> TokenResult
lineComment source i row col =
    -- i,row,col start _after_ //
    let
        ( i_, row_, col_ ) =
            skipUntilNewline source i row col
    in
    GotToken EOL i_ row_ col_


skipUntilNewline : String -> Int -> Int -> Int -> ( Int, Int, Int )
skipUntilNewline source i row col =
    case String.at i source of
        Nothing ->
            ( i, row, col )

        Just c ->
            case c of
                '\n' ->
                    ( i + 1, row + 1, 1 )

                '\u{000D}' ->
                    -- \r
                    if (match "\n" source (i + 1) row (col + 1)).matches then
                        ( i + 2, row + 1, 1 )

                    else
                        ( i + 1, row + 1, 1 )

                _ ->
                    skipUntilNewline source (i + 1) row (col + 1)


blockComment : String -> Int -> Int -> Int -> Result ( Loc, LexerError ) ( Int, Int, Int )
blockComment source i row col =
    -- i,row,col start _after_ /*
    let
        go : Int -> Int -> Int -> Int -> Result ( Loc, LexerError ) ( Int, Int, Int )
        go nesting i_ row_ col_ =
            let
                continue () =
                    go nesting (i_ + 1) row_ (col_ + 1)
            in
            case String.at i_ source of
                Nothing ->
                    Err ( { row = row_, col = col_ }, UnfinishedBlockComment )

                Just '/' ->
                    if (match "*" source (i_ + 1) row_ (col_ + 1)).matches then
                        go (nesting + 1) (i_ + 2) row_ (col_ + 2)

                    else
                        continue ()

                Just '*' ->
                    if (match "/" source (i_ + 1) row_ (col_ + 1)).matches then
                        if nesting > 0 then
                            go (nesting - 1) (i_ + 2) row_ (col_ + 2)

                        else
                            Ok ( i_ + 2, row_, col_ + 2 )

                    else
                        continue ()

                Just '\u{000D}' ->
                    if (match "\n" source (i_ + 1) row_ (col_ + 1)).matches then
                        go nesting (i_ + 2) (row_ + 1) 1

                    else
                        go nesting (i_ + 1) (row_ + 1) 1

                Just '\n' ->
                    go nesting (i_ + 1) (row_ + 1) 1

                Just _ ->
                    continue ()
    in
    go 0 i row col


lower : String -> Int -> Int -> Int -> TokenResult
lower source i row col =
    case lowerName source i row col of
        Err e ->
            err row col e

        Ok ( name, ( i_, row_, col_ ) ) ->
            let
                token t =
                    GotToken t i_ row_ col_
            in
            case name of
                "case" ->
                    token Case

                "of" ->
                    token Of

                "if" ->
                    token If

                "then" ->
                    token Then

                "else" ->
                    token Else

                "test" ->
                    token Test

                "with" ->
                    token With

                "use" ->
                    token Use

                "type" ->
                    token Type

                "alias" ->
                    token Alias

                "module" ->
                    token Module

                "private" ->
                    token Private

                "opaque" ->
                    token Opaque

                "extend" ->
                    token Extend

                "intrinsic" ->
                    token Intrinsic

                _ ->
                    token (LowerName name)


upper : String -> Int -> Int -> Int -> TokenResult
upper source i row col =
    case upperName source i row col of
        Err e ->
            err row col e

        Ok ( name, ( i_, row_, col_ ) ) ->
            if (match "." source i_ row_ col_).matches then
                GotToken (Qualifier name) (i_ + 1) row_ (col_ + 1)

            else
                case name of
                    "True" ->
                        GotToken True_ i_ row_ col_

                    "False" ->
                        GotToken False_ i_ row_ col_

                    _ ->
                        GotToken (UpperName name) i_ row_ col_


number : String -> Int -> Int -> Int -> TokenResult
number source i row col =
    case String.at i source of
        Nothing ->
            err row col ExpectedNumber

        Just first ->
            if Char.isDigit first then
                case String.at (i + 1) source of
                    Nothing ->
                        GotToken
                            (Int (Char.digitToInt first))
                            (i + 1)
                            row
                            (col + 1)

                    Just second ->
                        if first == '0' && second == 'X' then
                            err row col HexIntStartedWith0X

                        else if first == '0' && second == 'B' then
                            err row col BinaryIntStartedWith0X

                        else if first == '0' && second == 'O' then
                            err row col OctalIntStartedWith0X

                        else if first == '0' && second == 'x' then
                            hexInt source (i + 2) row (col + 2)

                        else if first == '0' && second == 'b' then
                            binaryInt source (i + 2) row (col + 2)

                        else if first == '0' && second == 'o' then
                            octInt source (i + 2) row (col + 2)

                        else
                            decNumber first source (i + 1) row (col + 1)

            else
                err row col ExpectedNumber


binaryInt : String -> Int -> Int -> Int -> TokenResult
binaryInt =
    intOfRadix
        { radix = 2
        , isRadixChar = isBinaryDigit
        , invalidError = InvalidBinaryInt
        , unfinishedError = UnfinishedBinaryInt
        , unexpectedCharError = UnexpectedBinaryIntCharacter
        }


octInt : String -> Int -> Int -> Int -> TokenResult
octInt =
    intOfRadix
        { radix = 8
        , isRadixChar = Char.isOctDigit
        , invalidError = InvalidOctInt
        , unfinishedError = UnfinishedOctInt
        , unexpectedCharError = UnexpectedOctIntCharacter
        }


hexInt : String -> Int -> Int -> Int -> TokenResult
hexInt =
    intOfRadix
        { radix = 16
        , isRadixChar = Char.isHexDigit
        , invalidError = InvalidHexInt
        , unfinishedError = UnfinishedHexInt
        , unexpectedCharError = UnexpectedHexIntCharacter
        }


intOfRadix :
    { radix : Int
    , isRadixChar : Char -> Bool
    , invalidError : LexerError
    , unfinishedError : LexerError
    , unexpectedCharError : Char -> LexerError
    }
    -> String
    -> Int
    -> Int
    -> Int
    -> TokenResult
intOfRadix config source i row col =
    -- i,row,col starts after the radix prefix (`0b` etc.)
    case String.at i source of
        Just c ->
            if config.isRadixChar c then
                let
                    ( radixIntPartWithoutFirst, ( i1, row1, col1 ) ) =
                        consumeWhile (isCharOrUnderscore config.isRadixChar) source (i + 1) row (col + 1)
                            |> Tuple.mapFirst (String.replace "_" "")
                in
                case
                    String.cons c radixIntPartWithoutFirst
                        |> ParseInt.parseIntRadix config.radix
                of
                    Ok n ->
                        GotToken (Int n) i1 row1 col1

                    Err _ ->
                        err row (col - 2) config.invalidError

            else
                err row col <| config.unexpectedCharError c

        Nothing ->
            err row col config.unfinishedError


decNumber : Char -> String -> Int -> Int -> Int -> TokenResult
decNumber first source i row col =
    {-
       We're responsible for numbers like 123, 1, 1.234, 123.56
       We're NOT responsible for 1..20 - in that case we only parse the `1`.

       first is guaranteed to be a digit
       second is _not_ (0.123, 123.352, 1..20)
       i,row,col are on the _second_ character

       consume as much [0-9_] as possible
    -}
    let
        ( intPartWithoutFirst, ( i1, row1, col1 ) ) =
            consumeWhile isDigitOrUnderscore source i row col
                |> Tuple.mapFirst (String.replace "_" "")

        intPart =
            String.cons first intPartWithoutFirst

        gotInt () =
            case String.toInt intPart of
                Nothing ->
                    err row col ExpectedNumber

                Just n ->
                    GotToken (Int n) i1 row1 col1
    in
    -- Now if we find a dot and 0-9 after that, we're in a Float!
    -- Otherwise we're in an Int.
    if (match "." source i1 row1 col1).matches then
        case String.at (i1 + 1) source of
            Just '.' ->
                {- We found `1..`. Let's ignore the `..` for now and just return the `1`.
                   The `..` will get lexed as DotDot later.
                -}
                gotInt ()

            Just c ->
                if Char.isDigit c then
                    float intPart c source (i1 + 2) row1 (col1 + 2) row col

                else
                    err row1 (col1 + 1) FloatExpectedNumbersAfterDot

            Nothing ->
                err row1 col1 FloatExpectedNumbersAfterDot

    else
        -- Definitely an Int!
        gotInt ()


float : String -> Char -> String -> Int -> Int -> Int -> Int -> Int -> TokenResult
float intPart firstFloat source i row col startRow startCol =
    -- Starts after the first decimal number
    let
        ( floatPartWithoutFirstBeforeE, ( i1, row1, col1 ) ) =
            consumeWhile isDigitOrUnderscore source i row col
                |> Tuple.mapFirst (String.replace "_" "")

        restResult : Result ( Loc, LexerError ) ( String, ( Int, Int, Int ) )
        restResult =
            case Maybe.map Char.toLower <| String.at i1 source of
                Just 'e' ->
                    case String.at (i1 + 1) source of
                        Just '-' ->
                            let
                                ( eNumbersPart, ( i2, row2, col2 ) ) =
                                    consumeWhile Char.isDigit source (i1 + 2) row1 (col1 + 2)
                            in
                            Ok
                                ( "e-" ++ eNumbersPart
                                , ( i2, row2, col2 )
                                )

                        Just _ ->
                            let
                                ( eNumbersPart, ( i2, row2, col2 ) ) =
                                    consumeWhile Char.isDigit source (i1 + 1) row1 (col1 + 1)
                            in
                            Ok
                                ( String.cons 'e' eNumbersPart
                                , ( i2, row2, col2 )
                                )

                        Nothing ->
                            Err
                                ( { row = row1, col = col1 }
                                , FloatExpectedNumbersAfterE
                                )

                _ ->
                    Ok
                        ( ""
                        , ( i1, row1, col1 )
                        )
    in
    case restResult of
        Err e ->
            GotError e

        Ok ( ePart, ( i2, row2, col2 ) ) ->
            let
                floatPart =
                    String.cons firstFloat floatPartWithoutFirstBeforeE
                        ++ ePart
            in
            case String.toFloat (intPart ++ "." ++ floatPart) of
                Nothing ->
                    -- Shouldn't happen if the previous getters are correct?
                    err startRow startCol ExpectedNumber

                Just n ->
                    GotToken (Float n) i2 row2 col2


lowerName : String -> Int -> Int -> Int -> Result LexerError ( String, ( Int, Int, Int ) )
lowerName source i row col =
    case String.at i source of
        Nothing ->
            Err ExpectedLowerName

        Just first ->
            if Char.isLower first then
                consumeWhile isIdentifierSecond source (i + 1) row (col + 1)
                    |> Tuple.mapFirst (String.cons first)
                    |> Ok

            else
                Err ExpectedLowerName


upperName : String -> Int -> Int -> Int -> Result LexerError ( String, ( Int, Int, Int ) )
upperName source i row col =
    case String.at i source of
        Nothing ->
            Err ExpectedUpperName

        Just first ->
            if Char.isUpper first then
                consumeWhile isIdentifierSecond source (i + 1) row (col + 1)
                    |> Tuple.mapFirst (String.cons first)
                    |> Ok

            else
                Err ExpectedUpperName


{-| Don't use this with predicates that accept newlines. The row/col will be wrong
-}
consumeWhile : (Char -> Bool) -> String -> Int -> Int -> Int -> ( String, ( Int, Int, Int ) )
consumeWhile pred source i row col =
    let
        go : Int -> Int -> Int -> List Char -> ( String, ( Int, Int, Int ) )
        go i_ row_ col_ content =
            case String.at i_ source of
                Nothing ->
                    ( content
                        |> List.reverse
                        |> String.fromList
                    , ( i_, row_, col_ )
                    )

                Just next ->
                    if pred next then
                        {- TODO we don't handle newlines here, because it's not obvious:
                           \n and \r is fine but \r\n would add two lines, not one,
                           while we probably want only one line to be added
                        -}
                        go (i_ + 1) row_ (col_ + 1) (next :: content)

                    else
                        ( content
                            |> List.reverse
                            |> String.fromList
                        , ( i_, row, col_ )
                        )
    in
    go i row col []


isIdentifierSecond : Char -> Bool
isIdentifierSecond c =
    Char.isAlphaNum c || c == '_' || c == '\'' || c == '?'


isDigitOrUnderscore : Char -> Bool
isDigitOrUnderscore c =
    Char.isDigit c || c == '_'


isBinaryDigit : Char -> Bool
isBinaryDigit c =
    c == '0' || c == '1'


isCharOrUnderscore : (Char -> Bool) -> Char -> Bool
isCharOrUnderscore pred c =
    pred c || c == '_'


variants : String -> Int -> Int -> Int -> List ( String, Token.Type ) -> Token.Type -> TokenResult
variants source i row col continuations default =
    case continuations of
        [] ->
            GotToken default i row col

        ( wanted, continuation ) :: rest ->
            let
                result =
                    match wanted source i row col
            in
            if result.matches then
                GotToken continuation result.i result.row result.col

            else
                variants source i row col rest default


type alias MatchResult =
    { matches : Bool
    , i : Int
    , row : Int
    , col : Int
    }


match : String -> String -> Int -> Int -> Int -> MatchResult
match wanted source i row col =
    let
        length =
            String.length wanted
    in
    case String.slice i (i + length) source of
        "" ->
            { matches = False, i = i, row = row, col = col }

        slice ->
            if slice == wanted then
                { matches = True, i = i + length, row = row, col = col + length }

            else
                { matches = False, i = i, row = row, col = col }


eol : String -> Int -> Int -> TokenResult
eol source i row =
    let
        go : Int -> Int -> Int -> TokenResult
        go i_ row_ col_ =
            case String.at i_ source of
                Just c ->
                    if isWhitespace c then
                        go (i_ + 1) row_ (col_ + 1)

                    else
                        GotToken EOL i_ row_ col_

                Nothing ->
                    GotToken EOL i_ row_ col_
    in
    go i (row + 1) 1


isWhitespace : Char -> Bool
isWhitespace c =
    c == ' ' || c == '\t'


char : String -> Int -> Int -> Int -> TokenResult
char source i row col =
    let
        go : Int -> Int -> Int -> List Char -> TokenResult
        go i_ row_ col_ content =
            let
                newI =
                    i_ + 1

                newCol =
                    col_ + 1
            in
            case String.at i_ source of
                Nothing ->
                    err row_ col_ NonterminatedChar

                Just '\'' ->
                    if List.isEmpty content then
                        err row_ col_ EmptyChar

                    else
                        GotToken
                            (content
                                |> List.reverse
                                |> String.fromList
                                |> Char
                            )
                            newI
                            row_
                            newCol

                Just '\t' ->
                    err row_ col_ UnescapedTabInChar

                Just '\\' ->
                    case String.at (i_ + 1) source of
                        Nothing ->
                            err row_ col_ NonterminatedChar

                        Just second ->
                            let
                                newI_ =
                                    i_ + 2

                                newCol_ =
                                    col_ + 2
                            in
                            case second of
                                '\\' ->
                                    go newI_ row_ newCol_ ('\\' :: content)

                                'n' ->
                                    go newI_ row_ newCol_ ('\n' :: content)

                                'r' ->
                                    go newI_ row_ newCol_ ('\u{000D}' :: content)

                                't' ->
                                    go newI_ row_ newCol_ ('\t' :: content)

                                '\'' ->
                                    go newI_ row_ newCol_ ('\'' :: content)

                                -- TODO \u{....}
                                -- TODO \x{..}
                                _ ->
                                    err row_ (col_ + 1) <| UnexpectedEscapedCharacterInChar second

                Just '\u{000D}' ->
                    err row_ col_ UnescapedNewlineInChar

                Just '\n' ->
                    err row_ col_ UnescapedNewlineInChar

                Just c ->
                    go newI row_ newCol (c :: content)
    in
    go i row col []


string : String -> Int -> Int -> Int -> TokenResult
string source i row col =
    let
        go : Int -> Int -> Int -> List Char -> TokenResult
        go i_ row_ col_ content =
            let
                newI =
                    i_ + 1

                newCol =
                    col_ + 1
            in
            case String.at i_ source of
                Nothing ->
                    err row_ col_ NonterminatedString

                Just '"' ->
                    GotToken
                        (content
                            |> List.reverse
                            |> String.fromList
                            |> String
                        )
                        newI
                        row_
                        newCol

                Just '\\' ->
                    case String.at (i_ + 1) source of
                        Nothing ->
                            err row_ col_ NonterminatedString

                        Just second ->
                            let
                                newI_ =
                                    i_ + 2

                                newCol_ =
                                    col_ + 2
                            in
                            case second of
                                '\\' ->
                                    go newI_ row_ newCol_ ('\\' :: content)

                                'n' ->
                                    go newI_ row_ newCol_ ('\n' :: content)

                                'r' ->
                                    go newI_ row_ newCol_ ('\u{000D}' :: content)

                                't' ->
                                    go newI_ row_ newCol_ ('\t' :: content)

                                '"' ->
                                    go newI_ row_ newCol_ ('"' :: content)

                                -- TODO \u{....}
                                -- TODO \x{..}
                                _ ->
                                    err row_ (col_ + 1) <| UnexpectedEscapedCharacterInString second

                Just '\u{000D}' ->
                    err row_ col_ UnescapedNewlineInString

                Just '\n' ->
                    err row_ col_ UnescapedNewlineInString

                Just c ->
                    go newI row_ newCol (c :: content)
    in
    go i row col []


multilineString : String -> Int -> Int -> Int -> TokenResult
multilineString source i row col =
    let
        go : Int -> Int -> Int -> List Char -> TokenResult
        go i_ row_ col_ content =
            let
                newI =
                    i_ + 1

                newCol =
                    col_ + 1
            in
            case String.at i_ source of
                Nothing ->
                    err row col NonterminatedMultilineString

                Just '`' ->
                    GotToken
                        (content
                            |> List.reverse
                            |> String.fromList
                            |> BacktickString
                        )
                        newI
                        row_
                        newCol

                Just '\\' ->
                    case String.at (i_ + 1) source of
                        Nothing ->
                            err row_ col_ NonterminatedMultilineString

                        Just second ->
                            let
                                newI_ =
                                    i_ + 2

                                newCol_ =
                                    col_ + 2
                            in
                            case second of
                                '\\' ->
                                    go newI_ row_ newCol_ ('\\' :: content)

                                'n' ->
                                    go newI_ row_ newCol_ ('\n' :: content)

                                'r' ->
                                    go newI_ row_ newCol_ ('\u{000D}' :: content)

                                't' ->
                                    go newI_ row_ newCol_ ('\t' :: content)

                                '`' ->
                                    go newI_ row_ newCol_ ('`' :: content)

                                {- This will get picked up by the parser.
                                   The '\' will be ignored if it's right before '{'.
                                -}
                                '$' ->
                                    go newI_ row_ newCol_ ('$' :: '\\' :: content)

                                -- TODO \u{....}
                                -- TODO \x{..}
                                _ ->
                                    err row_ (col_ + 1) <| UnexpectedEscapedCharacterInMultilineString second

                Just '\u{000D}' ->
                    -- optionally read '\n' as well
                    if (match "\n" source newI row_ newCol).matches then
                        go (newI + 1) (row_ + 1) 1 ('\u{000D}' :: '\n' :: content)

                    else
                        go newI (row_ + 1) 1 ('\u{000D}' :: content)

                Just '\n' ->
                    go newI (row_ + 1) 1 ('\n' :: content)

                Just c ->
                    go newI row_ newCol (c :: content)
    in
    go i row col []
