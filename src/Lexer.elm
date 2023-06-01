module Lexer exposing (lex)

import Char.Extra as Char
import Error exposing (LexerError(..))
import String.Extra as String
import Token exposing (Token, Type(..))


type TokenResult
    = GotToken Token.Type Int Int Int
    | GotNothing Int Int Int
    | GotError LexerError


lex : String -> Result LexerError (List Token)
lex source =
    let
        go : Int -> Int -> Int -> List Token -> Result LexerError (List Token)
        go i row col tokens =
            case nextToken source i row col of
                GotError err ->
                    Err err

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

                            Err err ->
                                GotError err

                    else
                        token Div

                '#' ->
                    Debug.todo "hash"

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
                    Debug.todo "underscore"

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
                                    GotError <| UnexpectedChar '.'

                                Ok ( lower_, ( i_, row_, col_ ) ) ->
                                    GotToken (Getter lower_) i_ row_ col_

                '\'' ->
                    char source newI row newCol

                '"' ->
                    Debug.todo "double quote"

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
                        Debug.todo <| "next token fallthrough ?!?!?! : " ++ String.fromChar c


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


blockComment : String -> Int -> Int -> Int -> Result LexerError ( Int, Int, Int )
blockComment _ _ _ _ =
    -- i,row,col start _after_ /*
    Debug.todo "block comment"


lower : String -> Int -> Int -> Int -> TokenResult
lower source i row col =
    case lowerName source i row col of
        Err err ->
            GotError err

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

                "type" ->
                    token Type

                "alias" ->
                    token Alias

                "module" ->
                    token Module_

                "private" ->
                    token Private

                "opaque" ->
                    token Opaque

                "extend" ->
                    token Extend

                _ ->
                    token (LowerName name)


upper : String -> Int -> Int -> Int -> TokenResult
upper source i row col =
    case upperName source i row col of
        Err err ->
            GotError err

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
            GotError ExpectedNumber

        Just first ->
            if Char.isDigit first then
                case String.at (i + 1) source of
                    Nothing ->
                        GotToken
                            (Int_ (Char.digitToInt first))
                            (i + 1)
                            row
                            (col + 1)

                    Just second ->
                        if first == '0' && second == 'X' then
                            GotError HexIntStartedWith0X

                        else if first == '0' && second == 'B' then
                            GotError BinaryIntStartedWith0X

                        else if first == '0' && second == 'O' then
                            GotError OctalIntStartedWith0X

                        else if first == '0' && second == 'x' then
                            Debug.todo "hex int"

                        else if first == '0' && second == 'b' then
                            Debug.todo "binary int"

                        else if first == '0' && second == 'o' then
                            Debug.todo "octal int"

                        else
                            decNumber first source (i + 1) row (col + 1)

            else
                GotError ExpectedNumber


decNumber : Char -> String -> Int -> Int -> Int -> TokenResult
decNumber first source i row col =
    -- We're responsible for numbers like 123, 1, 1.234, 123.56
    -- We're NOT responsible for 1..20 - in that case we only parse the `1`.
    -----
    -- first is guaranteed to be a digit
    -- second is _not_ (0.123, 123.352, 1..20)
    -- i,row,col are on the _second_ character
    -----
    -- consume as much [0-9_] as possible
    let
        ( intPartWithoutFirst, ( i1, row1, col1 ) ) =
            consumeWhile isDigitOrUnderscore source i row col
                |> Tuple.mapFirst (String.replace "_" "")
    in
    -- Now if we find a dot and 0-9 after that, we're in a Float!
    -- Otherwise we're in an Int.
    if (match "." source i1 row1 col1).matches then
        -- TODO beware the need to inc i1 and col1 because of the match in the if
        Debug.todo "check for digit, then float, otherwise Ok Int"

    else
        -- Definitely an Int!
        case
            String.cons first intPartWithoutFirst
                |> String.toInt
        of
            Nothing ->
                GotError ExpectedNumber

            Just n ->
                GotToken (Int_ n) i1 row1 col1


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
    Char.isAlphaNum c || c == '_' || c == '\''


isDigitOrUnderscore : Char -> Bool
isDigitOrUnderscore c =
    Char.isDigit c || c == '_'


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
                    GotError NonterminatedChar

                Just '\'' ->
                    if List.isEmpty content then
                        GotError EmptyChar

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
                    GotError UnescapedTabInChar

                Just '\\' ->
                    case String.at (i_ + 1) source of
                        Nothing ->
                            GotError NonterminatedChar

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
                                    GotError <| UnexpectedEscapedCharacterInChar second

                Just '\u{000D}' ->
                    -- optionally read '\n' as well
                    Debug.todo "char \\r"

                Just '\n' ->
                    Debug.todo "char \\n"

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
                    GotError NonterminatedMultilineString

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
                            GotError NonterminatedMultilineString

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

                                -- TODO \u{....}
                                -- TODO \x{..}
                                _ ->
                                    GotError <| UnexpectedEscapedCharacterInMultilineString second

                Just '\u{000D}' ->
                    -- optionally read '\n' as well
                    Debug.todo "multiline string \\r"

                Just '\n' ->
                    Debug.todo "multiline string \\n"

                Just c ->
                    go newI row_ newCol (c :: content)
    in
    go i row col []
