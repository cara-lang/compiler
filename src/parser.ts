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
            {prefix: ['TYPE'],         parser: typeDecl},
            /*
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

//: PRIVATE? TYPE ALIAS UPPER_NAME (LBRACKET typevar (COMMA typevar)* RBRACKET)? EQ type
function typeAliasDecl(state: State): {i: number, decl: Decl} {
    // we already skipped EOLs in declaration()
    let {i} = state;
    let isPrivate = false;
    //: PRIVATE?
    if (tagIs('PRIVATE',i,state.tokens)) {
        isPrivate = true;
        i++;
    }
    //: TYPE ALIAS
    i = expect('TYPE', 'type alias',i,state.tokens);
    i = expect('ALIAS','type alias',i,state.tokens);
    //: UPPER_NAME
    let nameResult = getUpperName('type alias',i,state.tokens);
    i = nameResult.i;
    const {name} = nameResult;
    //: (LBRACKET typevar (COMMA typevar)* RBRACKET)?
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
    //: EQ
    i = expect('EQ','type alias',i,state.tokens);
    //: type
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

//: (PRIVATE | OPAQUE)? TYPE UPPER_NAME LBRACKET typevar (COMMA typevar)* RBRACKET EQ constructorList
function typeDecl(state: State): {i: number, decl: Decl} {
    throw err('EXXXX','TODO type decl',{row:0,col:0});
}


function expect(tag: TokenTag, parsedItem: string, i: number, tokens: Token[]) {
    if (!tagIs(tag,i,tokens)) {
        throw err('EXXXX',`Expected ${tag} for a ${parsedItem}`,tokens[i].loc);
    }
    return i + 1;
}

//: LOWER_NAME
function typevar(state: State): {i: number, typevar: string} {
    const {name,i} = getLowerName('typevar',state.i,state.tokens);
    return {i, typevar: name};
}

function type(state: State): {i: number, type: Type} {
    return oneOf(
        [
            {prefix: ['LPAREN','RPAREN'], parser: unitType},
            {prefix: ['LPAREN'],          parser: tupleOrParenthesizedType},
            /*
            | {type:'named',  name:string}              // Int
            | {type:'var',    var:string}               // a
            | {type:'call',   name:string, args:Type[]} // List[a]
            | {type:'fn',     from:Type, to:Type}       // x -> y
            | {type:'record', fields:TypeRecordField[]} // {a:Int,b:Bool}
            */
        ],
        'type',
        state
    );
}

//: LPAREN RPAREN
function unitType(state: State): {i: number, type: Type} {
    let {i} = state;
    i = expect('LPAREN','unit type',i,state.tokens);
    i = expect('RPAREN','unit type',i,state.tokens);
    return {i, type: {type: 'unit'}};
}

//: LPAREN type (COMMA type)* RPAREN
function tupleOrParenthesizedType(state: State): {i: number, type: Type} {
    let {i} = state;
    i = expect('LPAREN','unit type',i,state.tokens);
    const firstType = type({...state, i});
    i = firstType.i;
    const types: Type[] = [firstType.type];
    while (!isAtEnd({...state,i})) {
        if (tagIs('RPAREN',i,state.tokens)) {
            i++;
            if (types.length == 1) {
                return {i, type: types[0]}; // parenthesized type: return the child
            } else {
                return {i, type: {type: 'tuple', elements: types}};
            }
        } else if (tagIs('COMMA',i,state.tokens)) {
            i++;
            const newType = type({...state, i});
            i = newType.i;
            types.push(newType.type);
        }
    }
    throw err('EXXXX','Expected RPAREN or `COMMA type` for tuple type or parenthesized type',state.tokens[i].loc);
}

//: EOL*
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

function getLowerName(parsedItem: string, i: number, tokens: Token[]): {i: number, name: string} {
    const nameToken = tokens[i];
    if (nameToken.type.type !== 'LOWER_NAME') {
        throw err('EXXXX',`Expected LOWER_NAME for a ${parsedItem}`,tokens[i].loc);
    }
    return {
        i: i + 1, 
        name: nameToken.type.name,
    };
}

function getUpperName(parsedItem: string, i: number, tokens: Token[]): {i: number, name: string} {
    const nameToken = tokens[i];
    if (nameToken.type.type !== 'UPPER_NAME') {
        throw err('EXXXX',`Expected UPPER_NAME for a ${parsedItem}`,tokens[i].loc);
    }
    return {
        i: i + 1, 
        name: nameToken.type.name,
    };
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