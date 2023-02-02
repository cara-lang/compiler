import {CaraError} from './error.ts';
import {Token, SimpleTokenType} from './token.ts';

type State = {
    source: string,
    i: number,
    row: number, // 1-based
    col: number, // 1-based
};

export function lex(source: string): Token[] {
    let i = 0;
    let row = 1;
    let col = 1;
    const length = source.length;
    const result: Token[] = [];
    while (i < length) {
        const next = nextToken({ source, i, row, col });
        i = next.state.i;
        row = next.state.row;
        col = next.state.col;
        result.push(next.token);
    }
    const { token } = simple('EOF', { source, i, row, col }); // ignoring the final state
    result.push(token);
    return result;
}

function err(message: string, state: State): CaraError {
    const { row, col } = state;
    return { stage: 'lexer', message, loc: { row, col } };
}

function simple(type: SimpleTokenType, state: State): { token: Token, state: State } {
    const { row, col } = state;
    return { token: { type: { type }, row, col }, state };
}

function eol(state: State): { token: Token, state: State } {
    let { row, col } = state;
    state.row++;
    state.col = 1;
    return { token: { type: { type: 'EOL' }, row, col }, state };
}

function isAtEnd(state: State) {
    return state.i >= state.source.length;
}

function match(expected: string, state: State): { matches: boolean, state: State } {
    if (isAtEnd(state)) return { matches: false, state };
    const length = expected.length;
    if (length > 1) {
        // special case
        if (state.source.substring(state.i, state.i + length) != expected) return { matches: false, state };
        state.i += length;
        state.col += length;
        return { matches: true, state };
    } else {
        if (state.source[state.i] != expected) return { matches: false, state };
        state.i++;
        state.col++;
        return { matches: true, state };
    }
}

function variants(variants: { c: string, t: SimpleTokenType }[], defaultType: SimpleTokenType, state: State): { token: Token, state: State } {
    let type: SimpleTokenType | null = null;
    for (const { c, t } of variants) {
        const result = match(c, state);
        if (result.matches) {
            type = t;
            state = result.state;
            break;
        }
    }
    if (type == null) type = defaultType;
    return simple(type, state);
}

function nextToken(state: State): { token: Token, state: State } {
    const nextChar = state.source[state.i++];
    state.col++;
    switch (nextChar) {
        case '+':
            return variants(
                [{ c: '+', t: 'PLUSPLUS' }], // ++
                'PLUS',                      // +
                state
            );
        case '-':
            return variants(
                [{ c: '>', t: 'ARROW' }], // ->
                'MINUS',                  // -
                state
            );
        case '*':
            return variants(
                [{ c: '*', t: 'POWER' }], // **
                'TIMES',                  // *
                state
            );
        case '%':
            return simple('PERCENT', state);
        case '<':
            return variants(
                [ { c: '<', t: 'SHL' } // <<
                , { c: '=', t: 'LTE' } // <=
                ],
                'LT',                  // <
                state
            );
        case '>':
            return variants(
                [ { c: '>>', t: 'SHRU' } // >>>
                , { c: '>', t: 'SHR' }   // >>
                , { c: '=', t: 'GTE' }   // >=
                ],
                'GT',                   // >
                state
            );
        case '^':
            return simple('CARET', state);
        case '&':
            return variants(
                [{ c: '&', t: 'ANDAND' }], // &&
                'AND',                     // &
                state
            );
        case '|':
            return variants(
                [ { c: '|', t: 'OROR' }     // ||
                , { c: '>', t: 'PIPELINE' } // |>
                ],
                'PIPE',                     // |
                state
            );
        case '=':
            return variants(
                [{ c: '=', t: 'EQEQ' }], // ==
                'EQ',                    // =
                state
            );
        case '!':
            return variants(
                [{ c: '=', t: 'NEQ' }], // !=
                'BANG',                 // !
                state
            );
        case '~':
            return simple('TILDE', state);
        case '\\':
            return simple('BACKSLASH', state);
        case '(':
            return simple('LPAREN', state);
        case ')':
            return simple('RPAREN', state);
        case '{':
            return simple('LBRACE', state);
        case '}':
            return simple('RBRACE', state);
        case '[':
            return simple('LBRACKET', state);
        case ']':
            return simple('RBRACKET', state);
        case ',':
            return simple('COMMA', state);
        case ':':
            return simple('COLON', state);
        case '/': {
            const result = match('/', state); // //
            if (result.matches) {
                return lineComment(result.state);
            }
            else return simple('DIV', result.state); // /
        }
        case '#': {
            let result = match('!', state); // #!
            if (result.matches) return shebang(result.state);

            result = match('(', state); // #(
            if (result.matches) return simple('LHOLE', result.state);

            throw err("unespected character: '#'", result.state);
        }
        case '\n':
            return eol(state);
        case '\r':
            state = match('\n', state).state; // \r\n
            return eol(state);                // \r
        case '_': {
            const result = simpleInt(state);
            if (result.match == null) return simple('UNDERSCORE', result.state);
            const { row, col } = result.state;
            return { token: { type: { type: 'HOLE', n: result.match }, row, col }, state: result.state };
        }
        case '.': {
            let first = match('.', state);
            if (first.matches) {
                let second = match('.', first.state);
                if (second.matches) return simple('RANGE_E', second.state); // ...
                return simple('RANGE_I', first.state) // ..
            }
            let lower = lowerName(state);
            if (lower.match == null) throw err("Unexpected character: '.'", lower.state);
            const { row, col } = lower.state;
            return { token: { type: { type: 'GETTER', field: lower.match }, row, col }, state: lower.state };
        }

        case '\'': throw err("TODO: '", state); // TODO Char(string)
        case '"':  throw err('TODO: "', state); // TODO String(string)

        case ' ':  return nextToken(state);
        case '\t': return nextToken(state);

        default:
            const c = nextChar.charCodeAt(0);
            if (c >= 97 && c <= 122 /*a-z*/) {
                state.i--;
                state.col--;
                const result = lowerName(state);
                switch (result.match) {
                    case 'if': return simple('IF', result.state);
                    case 'then': return simple('THEN', result.state);
                    case 'else': return simple('ELSE', result.state);
                    case 'type': return simple('TYPE', result.state);
                    case 'alias': return simple('ALIAS', result.state);
                    case 'module': return simple('MODULE', result.state);
                    case 'private': return simple('PRIVATE', result.state);
                    case 'opaque': return simple('OPAQUE', result.state);
                    case 'extend': return simple('EXTEND', result.state);
                    case null: throw err("Bug: we definitely should have got a LOWER_NAME", result.state);
                    default: {
                        const { row, col } = result.state;
                        return { token: { type: { type: 'LOWER_NAME', name: result.match }, row, col }, state: result.state };
                    }
                }
            }
            // TODO Int(number)
            // TODO Float(number)
            // TODO UpperName(string)
            // ... 'TRUE'
            // ... 'FALSE'
            // ... Qualifier(string) = UPPER_NAME '.'
            throw err(`Unexpected character '${nextChar}'`, state);
    }
}

function lineComment(state: State): { token: Token, state: State } {
    const newState = skipUntilNewline(state);
    return simple('EOL', newState);
}

function shebang(state: State): { token: Token, state: State } {
    if (state.i != 2) throw err('Shebang not at beginning of file', state);
    const newState = skipUntilNewline(state);
    return simple('SHEBANG', newState);
}

function skipUntilNewline(state: State): State {
    let foundNewline = false;
    while (!isAtEnd(state)) {
        const nextChar = state.source[state.i++];
        state.col++;
        if (nextChar == '\r') {
            state = match('\n', state).state; // \n is optional and doesn't change the flow, but greedily eat it!
            foundNewline = true;
        }
        else if (nextChar == '\n') {
            foundNewline = true;
        }
        if (foundNewline) {
            state.row++;
            state.col = 1;
            break;
        }
    }
    return state;
}

function simpleInt(state: State): { match: number | null, state: State } {
    let origI = state.i;
    while (!isAtEnd(state)) {
        const nextCharOrd = state.source.charCodeAt(state.i);
        if (nextCharOrd < 48 /* '0' */ || nextCharOrd > 57 /* '9' */) break;
        state.i++;
        state.col++;
    }
    if (origI == state.i) return { match: null, state };
    const match = parseInt(state.source.substring(origI, state.i), 10);
    // TODO: make sure there are no letters afterwards (throw a Lexer error if there are)
    return { match, state };
}

function lowerName(state: State): { match: string | null, state: State } {
    let origI = state.i;
    // 1st can be [a-z] only
    const firstCharOrd = state.source.charCodeAt(state.i);
    if (firstCharOrd < 97 /* 'a' */ || firstCharOrd > 122 /* 'z' */) return { match: null, state };
    state.i++;
    state.col++;
    while (!isAtEnd(state)) {
        // 2nd+ can be [a-zA-Z0-9_']
        const c = state.source.charCodeAt(state.i);
        if (!((c >= 97 && c <= 122/*a-z*/) || (c >= 65 && c <= 90/*A-Z*/) || (c >= 48 && c <= 57/*0-9*/) || c == 95/*_*/ || c == 39/*'*/)) break;
        state.i++;
        state.col++;
    }
    const match = state.source.substring(origI, state.i);
    return { match, state };
}
