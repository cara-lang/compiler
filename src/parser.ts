import {Token, TokenTag} from './token.ts';
import {Decl, Type, Constructor, Typevar, TypeAliasModifier, TypeModifier, RecordTypeField, Stmt, LetModifier, Bang, Expr, Identifier} from './ast.ts';
import {CaraError} from './error.ts';
import {Loc} from './loc.ts';

type State = { tokens: Token[], i: number };
type Parser<T> = (state: State) => {i: number, match: T}; // Parser<Decl> = (state: State) => {i: number, match: Decl}

export function parse(tokens: Token[]): Decl[] {
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
            {prefix: ['EXTEND','MODULE'], parser: extendModuleDecl},
            {prefix: null,                parser: statementDecl},
            //{prefix: ['LOWER_NAME','EQ','LBRACE'], parser: blockDecl}, // TODO does this mean we'll have blockFnDecl, effectBlockDecl, effectBlockFnDecl?
            /*
            moduleDecl,
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

//: EXTEND MODULE moduleName LBRACE declaration* RBRACE
//= extend module Foo.Bar { x = 1 }
function extendModuleDecl(state: State): {i: number, match: Decl} {
    let {i} = state;
    const desc = 'extend module declaration';
    //: EXTEND MODULE
    i = expect('EXTEND',desc,i,state.tokens);
    i = expect('MODULE',desc,i,state.tokens);
    //: moduleName
    const moduleNameResult = moduleName({...state, i});
    i = moduleNameResult.i;
    //: LBRACE declaration* RBRACE
    const declsResult = nonemptyList({
        left:  'LBRACE',
        right: 'RBRACE',
        sep:   null,
        item:  declaration,
        state: {...state, i},
        parsedItem: `${desc}`,
        skipEol: true,
    });
    i = declsResult.i;
    // Done!
    return {
        i,
        match: {
            decl: 'extend-module',
            id: moduleNameResult.match,
            decls: declsResult.match,
        },
    }
}

//: QUALIFIER* UPPER_NAME
//= Foo
//= Foo.Bar
//= Foo.Bar.Baz
function moduleName(state: State): {i: number, match: Identifier} {
    throw todo('module name',state);
}

function statementDecl(state: State): {i: number, match: Decl} {
    const stmtResult = statement(state);
    return {
        i: stmtResult.i,
        match: {
            decl: 'statement',
            stmt: stmtResult.match,
        },
    }
}

//: LOWER_NAME EQ bang //= x = Foo.bar!(1,False)
//: LOWER_NAME EQ expr //= x = 123
//: bang               //= Foo.bar!(1,False)
function statement(state: State): {i: number, match: Stmt} {
    return oneOf(
        [
            {prefix: null, parser: letBangStatement},
            {prefix: null, parser: letStatement},
            {prefix: null, parser: bangStatement},
        ],
        'statement',
        state
    );
}

//: PRIVATE? LOWER_NAME EQ expr
//= x = 123
function letStatement(state: State): {i: number, match: Stmt} {
    let {i} = state;
    const desc = 'let statement';
    //: PRIVATE?
    let mod: LetModifier = 'NoModifier';
    if (tagIs('PRIVATE',i,state.tokens)) {
        mod = 'Private';
        i++;
    }
    //: LOWER_NAME
    const nameResult = getLowerName(desc,i,state.tokens);
    i = nameResult.i;
    //: EQ
    i = expect('EQ',desc,i,state.tokens);
    //: expr
    const exprResult = expr({...state, i});
    i = exprResult.i;
    // Done!
    return {
        i,
        match: {
            stmt: 'let',
            mod,
            name: nameResult.match,
            body: exprResult.match,
        }
    };
}

//: PRIVATE? LOWER_NAME EQ bang
//= x = Foo.bar!(1,False)
function letBangStatement(state: State): {i: number, match: Stmt} {
    let {i} = state;
    const desc = 'let-bang statement';
    //: PRIVATE?
    let mod: LetModifier = 'NoModifier';
    if (tagIs('PRIVATE',i,state.tokens)) {
        mod = 'Private';
        i++;
    }
    //: LOWER_NAME
    const nameResult = getLowerName(desc,i,state.tokens);
    i = nameResult.i;
    //: EQ
    i = expect('EQ',desc,i,state.tokens);
    //: bang
    const bangResult = bang({...state, i});
    i = bangResult.i;
    // Done!
    return {
        i,
        match: {
            stmt: 'let-bang',
            mod,
            name: nameResult.match,
            body: bangResult.match,
        }
    };
}

//: bang
//= Foo.bar!(1,False)
function bangStatement(state: State): {i: number, match: Stmt} {
    //: bang
    const bangResult = bang(state);
    // Done!
    return {
        i: bangResult.i,
        match: {
            stmt: 'bang',
            bang: bangResult.match,
        }
    };
}

function expr(state: State): {i: number, match: Expr} {
    throw todo('expr', state);
}

function bang(state: State): {i: number, match: Bang} {
    throw todo('bang', state);
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
            skipEol: false,
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
            skipEol: false,
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
    throw todo('constructor list', state);
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
            {prefix: ['LBRACE'],                parser: recordType},
            /*
            | {type:'named',  name:string}              //= Int
            | {type:'var',    var:string}               //= a
            | {type:'fn',     from:Type, to:Type}       //= x -> y
            */
        ],
        'type',
        state
    );
}

//: LBRACE (recordTypeField (COMMA recordTypeField)*)? RBRACE
//= {a:Int,b:Bool}
function recordType(state: State): {i: number, match: Type} {
    const desc = 'record type';
    const listResult = list({
        left:  'LBRACE',
        right: 'RBRACE',
        sep:   'COMMA',
        item:  recordTypeField,
        state,
        parsedItem: desc,
        skipEol: true,
    });
    return {
        i: listResult.i,
        match: {type: 'record', fields: listResult.match},
    }
}

//: LOWER_NAME COLON type
//= a: Int
function recordTypeField(state: State): {i: number, match: RecordTypeField} {
    throw todo('record type field', state);
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
        skipEol: false,
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
        skipEol: true,
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
    sep:   TokenTag | null,
    item:  Parser<T>,
    state: State,
    parsedItem: string,
    skipEol:    boolean,
    // TODO: allow trailing separators by default in list() and nonemptyList()... probably don't even make it configurable
};

//: left (item (sep item)*)? right
function list<T>(c: ListConfig<T>): {i: number, match: T[]} {
    return (c.sep == null)
        ? nonseparatedList(c)
        : separatedList(c);
}

//: left item* right
function nonseparatedList<T>(c: ListConfig<T>): {i: number, match: T[]} {
    throw todo('nonseparated list', c.state);
}

//: left (item (sep item)*)? right
function separatedList<T>(c: ListConfig<T>): {i: number, match: T[]} {
    let {i} = c.state;
    const sep = c.sep!; // we're guaranteed this by the condition in list()
    //: left
    i = expect(c.left,c.parsedItem,i,c.state.tokens);
    //: EOL*
    i = skipEol({...c.state,i});
    //: (item (sep item)*)?
    const items: T[] = [];
    let iBeforeItems = i;
    try {
        let firstItem = c.item({...c.state,i});
        i = firstItem.i;
        items.push(firstItem.match);
        i = skipEol({...c.state,i});
        while (!isAtEnd({...c.state,i})) {
            if (tagIs(c.right,i,c.state.tokens)) {
                break; // we'll consume `right` outside this try{}catch{} block
            } else if (tagIs(sep,i,c.state.tokens)) {
                i++;
                i = skipEol({...c.state,i});
                const nextItem = c.item({...c.state,i});
                i = nextItem.i;
                items.push(nextItem.match);
                i = skipEol({...c.state,i});
            } else {
                throw err('EXXXX',`Expected ${c.right} or ${sep} in the ${c.parsedItem}`,c.state.tokens[i].loc);
            }
        }
    } catch (e) {
        i = iBeforeItems;
        // items = []; // TODO is this needed?
    }
    //: right
    i = expect(c.right,c.parsedItem,i,c.state.tokens);
    return {i, match: items};
}

function nonemptyList<T>(c: ListConfig<T>): {i: number, match: T[]} {
    return (c.sep == null) 
        ? nonemptyNonseparatedList(c) 
        : nonemptySeparatedList(c);
}

//: left item (sep item)* right
function nonemptySeparatedList<T>(c: ListConfig<T>): {i: number, match: T[]} {
    let {i} = c.state;
    const sep = c.sep!; // we're guaranteed this by the condition in nonemptyList()
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
        } else if (tagIs(sep,i,c.state.tokens)) {
            i++;
            const nextItem = c.item({...c.state,i});
            i = nextItem.i;
            items.push(nextItem.match);
        } else {
            throw err('EXXXX',`Expected ${c.right} or ${sep} in the ${c.parsedItem}`,c.state.tokens[i].loc);
        }
    }
    if (!endedCorrectly) {
        throw err('EXXXX',`Unterminated list in ${c.parsedItem}`,c.state.tokens[i].loc);
    }
    return {i, match: items};
}

//: left item+ right
function nonemptyNonseparatedList<T>(c: ListConfig<T>): {i: number, match: T[]} {
    let {i} = c.state;
    //: left
    i = expect(c.left,c.parsedItem,i,c.state.tokens);
    //: item
    let firstItem = c.item({...c.state,i});
    i = firstItem.i;
    const items: T[] = [firstItem.match];
    //: item*
    let endedCorrectly = false;
    while (!isAtEnd({...c.state,i})) {
        //: right
        if (tagIs(c.right,i,c.state.tokens)) {
            endedCorrectly = true;
            i++;
            break;
        } else {
            const nextItem = c.item({...c.state,i});
            i = nextItem.i;
            items.push(nextItem.match);
        }
    }
    if (!endedCorrectly) {
        throw err('EXXXX',`Unterminated list in ${c.parsedItem}`,c.state.tokens[i].loc);
    }
    return {i, match: items};
}

type Option<T> = { 
    prefix: TokenTag[] | null,
    parser: Parser<T>,
}

function oneOf<T>(options: Option<T>[], parsedItem: string, state: State): {i: number, match: T} {
    const iBefore = state.i;
    for (const option of options) {
        if (option.prefix == null) {
            // try with backtracking #lazymode
            try {
                return option.parser(state);
            } catch (e) {
                state.i = iBefore;
            }
        } else {
            // if the prefix agrees, commit!
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

function tagIs(tag: TokenTag, i:number, tokens: Token[]): boolean {
    return tokens[i].type.type == tag;
}

function err(code: string, message: string, loc: Loc): CaraError {
    return { stage: 'parser', code, message, loc };
}

function todo(what: string, state: State): CaraError {
    return err('E9999',`TODO ${what}`,state.tokens[state.i].loc);
}

function isAtEnd(state: State): boolean {
    return tagIs('EOF',state.i,state.tokens);
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