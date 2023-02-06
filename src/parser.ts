import {Token, TokenTag} from './token.ts';
import {Decl, Type, Constructor, Typevar, TypeAliasModifier, TypeModifier} from './ast.ts';
import {CaraError} from './error.ts';
import {Loc} from './loc.ts';

type State = { tokens: Token[], i: number };
type Parser<T> = (state: State) => {i: number, match: T}; // Parser<Decl> = (state: State) => {i: number, match: Decl}

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
        const {i, match} = declaration(state);
        state = {...state, i};
        decls.push(match);
    }
    return decls;
}

function declaration(state: State): {i: number, match: Decl} {
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
//= type alias Foo = Int
//= private type alias Bar[a,b] = Result[a,b]
function typeAliasDecl(state: State): {i: number, match: Decl} {
    const desc = 'type alias';
    let {i} = state;
    //: PRIVATE?
    let mod: TypeAliasModifier = 'NoModifier';
    if (tagIs('PRIVATE',i,state.tokens)) {
        mod = 'Private';
        i++;
    }
    //: TYPE ALIAS
    i = expect('TYPE', desc,i,state.tokens);
    i = expect('ALIAS',desc,i,state.tokens);
    //: UPPER_NAME
    let nameResult = getUpperName(desc,i,state.tokens);
    i = nameResult.i;
    //: (LBRACKET typevar (COMMA typevar)* RBRACKET)?
    let vars: Typevar[] = [];
    if (tagIs('LBRACKET',i,state.tokens)) {
        const listResult = nonemptyList({
            left:  'LBRACKET',
            right: 'RBRACKET',
            sep:   'COMMA',
            item:  typevar,
            state: {...state, i},
            parsedItem: `${desc} typevar list`,
        });
        i = listResult.i;
        vars = listResult.match;
    }
    //: EQ
    i = expect('EQ',desc,i,state.tokens);
    //: type
    const typeResult = type({...state, i});
    i = typeResult.i;
    // Done!
    return {
        i,
        match: {
            decl: 'type-alias',
            mod,
            name: nameResult.match,
            vars,
            body: typeResult.match,
        }
    }
}

//: (PRIVATE | OPAQUE)? TYPE UPPER_NAME (LBRACKET typevar (COMMA typevar)* RBRACKET)? EQ constructorList
//= type Unit = Unit
//= private type MyList[a] = Empty | Cons(a,MyList[a])
//= opaque type Html[msg] = Inert(InertHtml) | Eventful(EventfulHtml[msg])
function typeDecl(state: State): {i: number, match: Decl} {
    const desc = 'type declaration';
    let {i} = state;
    //: (PRIVATE | OPAQUE)?
    let mod: TypeModifier = 'NoModifier';
    if (tagIs('PRIVATE',i,state.tokens)) {
        mod = 'Private';
        i++;
    } else if (tagIs('OPAQUE',i,state.tokens)) {
        mod = 'Opaque';
        i++;
    }
    //: TYPE
    i = expect('TYPE',desc,i,state.tokens);
    //: UPPER_NAME
    let nameResult = getUpperName(desc,i,state.tokens);
    i = nameResult.i;
    //: (LBRACKET typevar (COMMA typevar)* RBRACKET)?
    let vars: Typevar[] = [];
    if (tagIs('LBRACKET',i,state.tokens)) {
        const listResult = nonemptyList({
            left:  'LBRACKET',
            right: 'RBRACKET',
            sep:   'COMMA',
            item:  typevar,
            state: {...state, i},
            parsedItem: `${desc} typevar list`,
        });
        i = listResult.i;
        vars = listResult.match;
    }
    //: EQ
    i = expect('EQ',desc,i,state.tokens);
    //: constructorList
    const constructorsResult = constructorList({...state,i});
    i = constructorsResult.i;
    // Done!
    return {
        i,
        match: {
            decl: 'type',
            mod,
            name: nameResult.match,
            vars,
            constructors: constructorsResult.match,
        }
    }
}

function constructorList(state: State): {i: number, match: Constructor[]} {
    throw err('EXXXX','TODO constructor list',state.tokens[state.i].loc);
}

//: LOWER_NAME
//= a
//= comparable123
function typevar(state: State): {i: number, match: Typevar} {
    return getLowerName('typevar',state.i,state.tokens);
}

function type(state: State): {i: number, match: Type} {
    return oneOf(
        [
            {prefix: ['LPAREN','RPAREN'],       parser: unitType},
            {prefix: ['LPAREN'],                parser: tupleOrParenthesizedType},
            {prefix: ['UPPER_NAME','LBRACKET'], parser: callType},
            /*
            | {type:'named',  name:string}              //= Int
            | {type:'var',    var:string}               //= a
            | {type:'fn',     from:Type, to:Type}       //= x -> y
            | {type:'record', fields:TypeRecordField[]} //= {a:Int,b:Bool}
            */
        ],
        'type',
        state
    );
}

//: UPPER_NAME LBRACKET type (COMMA type)* RBRACKET
//= List[a]
//= Result[(),(Int,String)]
function callType(state: State): {i: number, match: Type} {
    let {i} = state;
    const desc = 'call type';
    //: UPPER_NAME
    let nameResult = getUpperName(desc,i,state.tokens);
    i = nameResult.i;
    //: LBRACKET type (COMMA type)* RBRACKET
    const argsResult = nonemptyList({
        left:  'LBRACKET',
        right: 'RBRACKET',
        sep:   'COMMA',
        item:  type,
        state: {...state, i},
        parsedItem: `${desc} arg list`,
    });
    i = argsResult.i;
    return {
        i,
        match: {
            type:'call',
            name: nameResult.match,
            args: argsResult.match,
        },
    };
}

//: LPAREN RPAREN
//= ()
function unitType(state: State): {i: number, match: Type} {
    let {i} = state;
    i = expect('LPAREN','unit type',i,state.tokens);
    i = expect('RPAREN','unit type',i,state.tokens);
    return {i, match: {type: 'unit'}};
}

//: LPAREN type (COMMA type)* RPAREN
//= (Int)
//= (Int,Float,String)
function tupleOrParenthesizedType(state: State): {i: number, match: Type} {
    const listResult = nonemptyList({
        left:  'LPAREN',
        right: 'RPAREN',
        sep:   'COMMA',
        item:  type,
        state: state,
        parsedItem: 'tuple type or parenthesized type',
    });

    const match: Type = listResult.match.length == 1
        ?  listResult.match[0] // parenthesized type: return the child
        :  {type: 'tuple', elements: listResult.match};

    return {i: listResult.i, match};
}

//: EOL*
function skipEol(state: State): number {
    let {i} = state;
    while (tagIs('EOL',i,state.tokens)) {
        i++;
    }
    return i;
}

type ListConfig<T> = {
    left:  TokenTag,
    right: TokenTag,
    sep:   TokenTag,
    item:  Parser<T>,
    state: State,
    parsedItem: string,
};

//: left item (sep item)* right
function nonemptyList<T>(c: ListConfig<T>): {i: number, match: T[]} {
    let {i} = c.state;
    //: left
    i = expect(c.left,c.parsedItem,i,c.state.tokens);
    //: item
    let firstItem = c.item({...c.state,i});
    i = firstItem.i;
    const items: T[] = [firstItem.match];
    let endedCorrectly = false;
    while (!isAtEnd({...c.state,i})) {
        if (tagIs(c.right,i,c.state.tokens)) {
            endedCorrectly = true;
            i++;
            break;
        } else if (tagIs(c.sep,i,c.state.tokens)) {
            i++;
            const nextItem = c.item({...c.state,i});
            i = nextItem.i;
            items.push(nextItem.match);
        } else {
            throw err('EXXXX',`Expected ${c.right} or ${c.sep} in the ${c.parsedItem}`,c.state.tokens[i].loc);
        }
    }
    if (!endedCorrectly) {
        throw err('EXXXX','Unterminated typevar list in type alias',c.state.tokens[i].loc);
    }
    return {i, match: items};
}

type Option<T> = { 
    prefix: TokenTag[] | null,
    parser: Parser<T>,
}

function oneOf<T>(options: Option<T>[], parsedItem: string, state: State): {i: number, match: T} {
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

function getLowerName(parsedItem: string, i: number, tokens: Token[]): {i: number, match: string} {
    const nameToken = tokens[i];
    if (nameToken.type.type !== 'LOWER_NAME') {
        throw err('EXXXX',`Expected LOWER_NAME for a ${parsedItem}`,tokens[i].loc);
    }
    return {
        i: i + 1, 
        match: nameToken.type.name,
    };
}

function getUpperName(parsedItem: string, i: number, tokens: Token[]): {i: number, match: string} {
    const nameToken = tokens[i];
    if (nameToken.type.type !== 'UPPER_NAME') {
        throw err('EXXXX',`Expected UPPER_NAME for a ${parsedItem}`,tokens[i].loc);
    }
    return {
        i: i + 1, 
        match: nameToken.type.name,
    };
}

function getTags(n: number, state: State): TokenTag[] {
    return state.tokens.slice(state.i,state.i+n).map(t => t.type.type);
}

function expect(tag: TokenTag, parsedItem: string, i: number, tokens: Token[]) {
    if (!tagIs(tag,i,tokens)) {
        throw err('EXXXX',`Expected ${tag} for a ${parsedItem}`,tokens[i].loc);
    }
    return i + 1;
}

function arrayEquals<T>(a: T[], b: T[]): boolean {
    return Array.isArray(a) &&
        Array.isArray(b) &&
        a.length === b.length &&
        a.every((val, index) => val === b[index]);
}