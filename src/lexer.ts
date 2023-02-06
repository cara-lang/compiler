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
    const result: Token[] = [];
    while (true) {
        const next = nextToken({ source, i, row, col });
        i = next.state.i;
        /* TODO: return correct row,col from nextToken instead of fixing it here?
           It would probably require all the smaller lexers to stop being
           stateful (state.col++)...
        */
        result.push({...next.token, loc: {row,col}});
        if (next.token.type.type == 'EOF') break;
        row = next.state.row;
        col = next.state.col;
    }
    return result;
}

function err(code: string, message: string, state: State): CaraError {
    const { row, col } = state;
    return { stage: 'lexer', code, message, loc: { row, col } };
}

function simple(type: SimpleTokenType, state: State): { token: Token, state: State } {
    const { row, col } = state;
    return { token: { type: { type }, loc: {row, col} }, state };
}

function eol(state: State): { token: Token, state: State } {
    let { row, col } = state;
    state.row++;
    state.col = 1;
    return { token: { type: { type: 'EOL' }, loc: {row, col} }, state };
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
    if (isAtEnd(state)) { return simple('EOF',state); }
    const c = state.source[state.i++];
    state.col++;
    switch (c) {
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
            let result = match('/', state); // //
            if (result.matches) {
                return lineComment(result.state);
            }
            result = match('*', state); // /*
            if (result.matches) {
                const state = blockComment(result.state);
                return nextToken(state);
            }
            return simple('DIV', result.state); // /
        }
        case '#': {
            let result = match('!', state); // #!
            if (result.matches) {
                return shebang(result.state);
            }

            result = match('(', state); // #(
            if (result.matches) return simple('LHOLE', result.state);

            throw err("EXXXX", "unespected character: '#'", result.state);
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
            return { token: { type: { type: 'HOLE', n: result.match }, loc: {row, col} }, state: result.state };
        }
        case '.': {
            let first = match('.', state);
            if (first.matches) {
                let second = match('.', first.state);
                if (second.matches) return simple('RANGE_E', second.state); // ...
                return simple('RANGE_I', first.state) // ..
            }
            let lower = lowerName(state);
            if (lower.match == null) throw err("EXXXX", "Unexpected character: '.'", lower.state);
            const { row, col } = lower.state;
            return { token: { type: { type: 'GETTER', field: lower.match }, loc: {row, col} }, state: lower.state };
        }

        case '\'': return char(state);
        case '"':  return string(state);
        case '`':  return multilineString(state);

        case ' ':  return nextToken(state);
        case '\t': return nextToken(state);

        default:
            if (c.match(/[a-z]/)) {
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
                    case null: throw err("EXXXX", "Bug: we definitely should have got a LOWER_NAME", result.state);
                    default: {
                        const { row, col } = result.state;
                        return { token: { type: { type: 'LOWER_NAME', name: result.match }, loc: {row, col} }, state: result.state };
                    }
                }
            } else if (c.match(/[A-Z]/)) {
                state.i--;
                state.col--;
                const result = upperName(state);
                state = result.state;
                if (state.source[state.i] == '.') {
                    state.i++;
                    state.col++;
                    const { row, col } = state;
                    return { token: { type: { type: 'QUALIFIER', name: result.match! }, loc: {row, col} }, state };
                }
                switch (result.match) {
                    case 'True':  return simple('TRUE',  state);
                    case 'False': return simple('FALSE', state);
                    default: 
                        const { row, col } = state;
                        return { token: { type: { type: 'UPPER_NAME', name: result.match! }, loc: {row, col} }, state };
                }
            } else if (c.match(/[0-9]/)) {
                state.i--;
                state.col--;
                return number(state);
            }
            throw err("EXXXX", `Unexpected character '${c}'`, state);
    }
    throw err("EXXXX", `Bug: Didn't handle '${c}' for some reason`, state);
}

function lineComment(state: State): { token: Token, state: State } {
    const newState = skipUntilNewline(state);
    return simple('EOL', newState);
}

function blockComment(state: State): State {
    let nesting = 0;
    while (!isAtEnd(state)) {
        const nextChar = state.source[state.i++];
        state.col++;
        switch (nextChar) {
            case '/': {
                const second = state.source[state.i];
                if (second == '*') {
                    state.i++;
                    state.col++;
                    nesting++;
                }
                break;
            }
            case '*': {
                const second = state.source[state.i];
                if (second == '/') {
                    state.i++;
                    state.col++;
                    if (nesting > 0) {
                        nesting--;
                    } else {
                        return state;
                    }
                }
                break;
            }
            case '\r':
                // optionally read '\n' as well
                if (state.source[state.i] == '\n') state.i++;
            case '\n':
                state.row++;
                state.col = 1;
                break;
            // default: just continue
        }
    }
    throw err('E0009','Unfinished block comment',state);
}

function shebang(state: State): {token: Token, state: State} {
    if (state.i != 2) throw err('E0015', 'Shebang comment is not first', state);
    const newState = skipUntilNewline(state);
    return simple('EOL', newState);
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
        const nextChar = state.source[state.i];
        if (!nextChar.match(/[0-9]/)) break;
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
    const firstChar = state.source[state.i];
    if (!firstChar.match(/[a-z]/)) return { match: null, state };
    state.i++;
    state.col++;
    while (!isAtEnd(state)) {
        // 2nd+ can be [a-zA-Z0-9_']
        const c = state.source[state.i];
        if (!c.match(/[a-zA-Z0-9_']/)) break;
        state.i++;
        state.col++;
    }
    const match = state.source.substring(origI, state.i);
    return { match, state };
}

function upperName(state: State): { match: string | null, state: State } {
    let origI = state.i;
    // 1st can be [A-Z] only
    const firstChar = state.source[state.i];
    if (!firstChar.match(/[A-Z]/)) return { match: null, state };
    state.i++;
    state.col++;
    while (!isAtEnd(state)) {
        // 2nd+ can be [a-zA-Z0-9_']
        const c = state.source[state.i];
        if (!c.match(/[a-zA-Z0-9_']/)) break;
        state.i++;
        state.col++;
    }
    const match = state.source.substring(origI, state.i);
    return { match, state };
}

function char(state: State): {token: Token, state: State} {
    let content = "";
    while (!isAtEnd(state)) {
        const nextChar = state.source[state.i++];
        state.col++;
        switch (nextChar) {
            case "'":
                if (content.length == 0) {
                    throw err('E0019', 'Empty character', state);
                } else {
                    const {row,col} = state;
                    return {token:{type:{type:'CHAR',char:content},loc:{row,col}},state};
                }
            case '\t':
                throw err('E0018', 'Unescaped tab in a char', state);
            case '\\':
                const second = state.source[state.i++];
                state.col++;
                switch (second) {
                    case '\\': content += '\\'; break;
                    case 'n':  content += '\n'; break;
                    case 'r':  content += '\r'; break;
                    case 't':  content += '\t'; break;
                    case "'":  content += "'";  break;
                    // TODO \u{....}
                    // TODO \x{..}
                    default: throw err('E0028', 'Unexpected escaped character in a character', state);
                }
                break;
            case '\r':
                // optionally read '\n' as well
                if (state.source[state.i] == '\n') state.i++;
            case '\n':
                state.row++;
                state.col = 1;
                throw err('E0017', 'Unescaped newline in a character', state);
            default: 
                // any other char needs to be saved!
                content += nextChar;
        }
    }
    throw err('EXXXX', 'Unterminated char at EOF', state);
}

function string(state: State): {token: Token, state: State} {
    let content = "";
    while (!isAtEnd(state)) {
        const nextChar = state.source[state.i++];
        state.col++;
        switch (nextChar) {
            case '"':
                const {row,col} = state;
                return {token:{type:{type:'STRING',string:content},loc:{row,col}},state};
            case '\\':
                const second = state.source[state.i++];
                state.col++;
                switch (second) {
                    case '\\': content += '\\'; break;
                    case 'n':  content += '\n'; break;
                    case 'r':  content += '\r'; break;
                    case 't':  content += '\t'; break;
                    case '"':  content += '"';  break;
                    // TODO \u{....}
                    // TODO \x{..}
                    default: throw err('E0014', 'Unexpected escaped character in a single-line string', state);
                }
                break;
            case '\r':
                // optionally read '\n' as well
                if (state.source[state.i] == '\n') state.i++;
            case '\n':
                state.row++;
                state.col = 1;
                throw err('E0012', 'Unescaped newline in a single-line string', state);
            default: 
                // any other char needs to be saved!
                content += nextChar;
        }
    }
    throw err('EXXXX', 'Unterminated single-line string at EOF', state);
}

function multilineString(state: State): {token: Token, state: State} {
    let content = "";
    while (!isAtEnd(state)) {
        const nextChar = state.source[state.i++];
        state.col++;
        switch (nextChar) {
            case '`':
                const {row,col} = state;
                return {token:{type:{type:'STRING',string:content},loc:{row,col}},state};
            case '\\':
                const second = state.source[state.i++];
                state.col++;
                switch (second) {
                    case '\\': content += '\\'; break;
                    case 'n':  content += '\n'; break;
                    case 'r':  content += '\r'; break;
                    case 't':  content += '\t'; break;
                    case '`':  content += '`';  break;
                    // TODO \u{....}
                    // TODO \x{..}
                    default: throw err('E0029', 'Unexpected escaped character in a multi-line string', state);
                }
                break;
            case '\r':
                // optionally read '\n' as well
                if (state.source[state.i] == '\n') state.i++;
            case '\n':
                state.row++;
                state.col = 1;
                content += "\n"
                break;
            default: 
                // any other char needs to be saved!
                content += nextChar;
        }
    }
    throw err('EXXXX', 'Unterminated multi-line string at EOF', state);
}

function number(state: State): {token: Token, state: State} {
    const c = state.source[state.i++];
    state.col++;
    const next = state.source[state.i];
    if (c == '0' && next == 'X') {
        state.i++;
        state.col++;
        throw err('E0024', 'Hexadecimal integer started with 0X', state);
    } 
    if (c == '0' && next == 'B') {
        state.i++;
        state.col++;
        throw err('E0025', 'Binary integer started with 0B', state);
    } 
    if (c == '0' && next == 'O') {
        state.i++;
        state.col++;
        throw err('E0026', 'Octal integer started with 0O', state);
    } 
    if (c == '0' && next == 'x') {
        state.i++;
        state.col++;
        const first = state.source[state.i];
        if (first.match(/[0-9a-fA-F]/)) {
            state.i++;
            state.col++;
            const regex = /[0-9a-fA-F_]/y;
            regex.lastIndex = state.i;
            const rest = state.source.match(regex)!;
            const int = parseInt(rest[0].replace('_',''),16);
            const { row, col } = state;
            state.col += regex.lastIndex - state.i;
            state.i = regex.lastIndex;
            return { token: { type: { type: 'INT', int }, loc: {row, col} }, state };
        } else {
            throw err('EXXXX', `Hexadecimal integer: unexpected character ${first}`, state);
        }
    } 
    if (c == '0' && next == 'o') {
        state.i++;
        state.col++;
        const first = state.source[state.i];
        if (first.match(/[0-7]/)) {
            state.i++;
            state.col++;
            const regex = /[0-7_]/y;
            regex.lastIndex = state.i;
            const rest = state.source.match(regex)!;
            const int = parseInt(rest[0].replace('_',''),8);
            const { row, col } = state;
            state.col += regex.lastIndex - state.i;
            state.i = regex.lastIndex;
            return { token: { type: { type: 'INT', int }, loc: {row, col} }, state };
        } else {
            throw err('EXXXX', `Octal integer: unexpected character ${first}`, state);
        }
    } 
    if (c == '0' && next == 'b') {
        state.i++;
        state.col++;
        const first = state.source[state.i];
        if (first.match(/[0-1]/)) {
            const regex = /[0-1_]/y;
            regex.lastIndex = state.i;
            const rest = state.source.match(regex)!;
            const int = parseInt(rest[0].replace('_',''),2);
            const { row, col } = state;
            state.col += regex.lastIndex - state.i;
            state.i = regex.lastIndex;
            return { token: { type: { type: 'INT', int }, loc: {row, col} }, state };
        } else {
            throw err('EXXXX', `Binary integer: unexpected character ${first}`, state);
        }
    } 
    // Base 10!
    state.i--;
    state.col--;
    const regex = /[0-9_]+/y; // by being in this function we're guaranteed the first char isn't an _
    regex.lastIndex = state.i;
    const intString = state.source.match(regex)![0].replace('_','');
    state.col += regex.lastIndex - state.i;
    state.i = regex.lastIndex;

    // we also could have had 1..20, hence the check for [0-9] here:
    if (state.source[state.i] == '.' && state.source[state.i+1].match(/[0-9]/)) {
        state.i++;
        state.col++;
        const regex = /[0-9][0-9_]*([eE]-?[0-9]+)?/y;
        regex.lastIndex = state.i;
        const restMatch = state.source.match(regex);
        if (restMatch == null) {
            throw err('EXXXX','Float: expected numbers after decimal dot',state);
        }
        const restString = restMatch[0].replace('_','');
        state.col += regex.lastIndex - state.i;
        state.i = regex.lastIndex;
        const float = parseFloat(`${intString}.${restString}`);
        const { row, col } = state;
        return {token: {type: {type: 'FLOAT', float},loc:{row,col}},state};
    } else {
        const int = parseInt(intString,10);
        const { row, col } = state;
        return {token: {type: {type: 'INT', int},loc:{row,col}},state};
    }
}