import {Token, TokenTag} from './token.ts';
import {Decl, Type} from './ast.ts';
import {CaraError} from './error.ts';
import {Loc} from './loc.ts';

type State = { tokens: Token[], i: number };
type Parser<T> = (state: State) => {i: number} & T; // Parser<{decl: Decl}> = (state: State) => {i: number, decl: Decl}

function tagIs(tag: TokenTag, i:number, tokens: Token[]): boolean {
    return tokens[i].type.type == tag;
}

function err(code: string, message: string, loc: Loc): CaraError {
    return { stage: 'parser', code, message, loc };
}

function isAtEnd(state: State): boolean {
    return tagIs('EOF',state.i,state.tokens);
}

export function parse(tokens: Token[]): Decl[] {
    console.log(tokens);
    let state = {tokens, i: 0};
    const decls: Decl[] = [];
    while (!isAtEnd(state)) {
        const {i, decl} = declaration(state);
        state = {...state, i};
        decls.push(decl);
    }
    return decls;
}

function declaration(state: State): {i: number, decl: Decl} {
    const i = skipEol(state);
    state = {...state, i};
    return oneOf(
        [
            {prefix: ['TYPE','ALIAS'], parser: typeAliasDecl},
            /*
            typeDecl,
            moduleDecl,
            extendModuleDecl,
            functionDecl, // f(a,b) = expr
            statementDecl,
            blockDecl, // handles block, block fn, effect block, effect block fn
                    // x[(a,b)] = [IO] { ... }
            valueAnnotationDecl,
            */
        ],
        'declaration',
        state
    );
}

function typeAliasDecl(state: State): {i: number, decl: Decl} {
    // we already skipped EOLs in declaration()
    let {i} = state;
    let isPrivate = false;
    // PRIVATE?
    if (tagIs('PRIVATE',i,state.tokens)) {
        isPrivate = true;
        i++;
    }
    // TYPE ALIAS
    i = expect('TYPE', 'type alias',i,state.tokens);
    i = expect('ALIAS','type alias',i,state.tokens);
    // UPPER_NAME
    const nameToken = state.tokens[i];
    if (nameToken.type.type !== 'UPPER_NAME') {
        throw err('EXXXX','Expected UPPER_NAME for a `type alias`',state.tokens[i].loc);
    }
    const name = nameToken.type.name;
    i++;
    // LBRACKET typevar (COMMA typevar)* RBRACKET
    let vars = [];
    if (tagIs('LBRACKET',i,state.tokens)) {
        i++;
        let result = typevar({...state,i});
        i = result.i;
        vars.push(result.typevar);
        let endedCorrectly = false;
        while (!isAtEnd({...state,i})) {
            if (tagIs('RBRACKET',i,state.tokens)) {
                endedCorrectly = true;
                i++;
                break;
            } else if (tagIs('COMMA',i,state.tokens)) {
                i++;
                const result = typevar({...state,i});
                i = result.i;
                vars.push(result.typevar);
            } else {
                throw err('EXXXX','Expected ] or , in the type alias typevar list',state.tokens[i].loc);
            }
        }
        if (!endedCorrectly) {
            throw err('EXXXX','Unterminated typevar list in type alias',state.tokens[i].loc);
        }
    }
    // EQ
    i = expect('EQ','type alias',i,state.tokens);
    // type
    const typeResult = type({...state, i});
    i = typeResult.i;
    // Done!
    return {
        i,
        decl: {
            decl: 'type-alias',
            mod: isPrivate ? 'Private' : 'NoModifier',
            name,
            vars,
            body: typeResult.type,
        }
    }
}

function expect(tag: TokenTag, parsedItem: string, i: number, tokens: Token[]) {
    if (!tagIs(tag,i,tokens)) {
        throw err('EXXXX',`Expected ${tag} for a ${parsedItem}`,tokens[i].loc);
    }
    return i + 1;
}

function typevar(state: State): {i: number, typevar: string} {
    throw err('EXXXX','TODO typevar',{row:0,col:0});
}

function type(state: State): {i: number, type: Type} {
    return oneOf(
        [
            {prefix: ['LPAREN','RPAREN'], parser: unitType},
            {prefix: ['LPAREN'],          parser: tupleType},
            /*
            | {type:'named',  name:string}              // Int
            | {type:'var',    var:string}               // a
            | {type:'call',   name:string, args:Type[]} // List[a]
            | {type:'fn',     from:Type, to:Type}       // x -> y
            | {type:'tuple',  elements:Type[]}          // (Int, Bool)
            | {type:'record', fields:TypeRecordField[]} // {a:Int,b:Bool}
            | {type:'unit'}                             // ()
            */
        ],
        'type',
        state
    );
}

function unitType(state: State): {i: number, type: Type} {
    return {i: state.i, type: {type: 'unit'}};
}

function tupleType(state: State): {i: number, type: Type} {
    throw err('EXXXX','TODO tuple type',{row:0,col:0});
}

function skipEol(state: State): number {
    let {i} = state;
    while (tagIs('EOL',i,state.tokens)) {
        i++;
    }
    return i;
}

type Option<T> = { 
    prefix: TokenTag[] | null,
    parser: Parser<T>,
}

function oneOf<T>(options: Option<T>[], parsedItem: string, state: State): {i: number} & T {
    for (const option of options) {
        if (option.prefix == null) {
            return option.parser(state);
            // TODO should we try all non-prefix options instead? What error to report then?
        } else {
            if (arrayEquals(option.prefix,getTags(option.prefix.length,state))) {
                return option.parser(state);
            }
        }
    }
    throw err('EXXXX',`Expected ${parsedItem}`,state.tokens[state.i].loc);
}

function getTags(n: number, state: State): TokenTag[] {
    return state.tokens.slice(state.i,state.i+n).map(t => t.type.type);
}


function arrayEquals<T>(a: T[], b: T[]): boolean {
    return Array.isArray(a) &&
        Array.isArray(b) &&
        a.length === b.length &&
        a.every((val, index) => val === b[index]);
}