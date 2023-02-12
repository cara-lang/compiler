import {Token, TokenTag} from './token.ts';
import {Decl, Type, Constructor, Typevar, TypeAliasModifier, TypeModifier, RecordTypeField, Stmt, LetModifier, Bang, Expr, LowerIdentifier, UpperIdentifier, ConstructorArg} from './ast.ts';
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

function exprPrecedence(tag: TokenTag): number | null {
    switch (tag) {
        case 'ANDAND':   return  1; // &&
        case 'OROR':     return  2; // ||
        case 'PLUSPLUS': return  3; // ++
        case 'PIPELINE': return  4; // |>
        case 'RANGE_I':  return  5; // ..
        case 'RANGE_E':  return  5; // ...
        case 'PIPE':     return  6; // |
        case 'CARET':    return  7; // ^
        case 'AND':      return  8; // &
        case 'EQEQ':     return  9; // ==
        case 'NEQ':      return  9; // !=
        case 'LTE':      return 10; // <=
        case 'LT':       return 10; // <
        case 'GT':       return 10; // >
        case 'GTE':      return 10; // >=
        case 'SHL':      return 11; // <<
        case 'SHR':      return 11; // >>
        case 'SHRU':     return 11; // >>>
        case 'PLUS':     return 12; // +
        case 'MINUS':    return 12; // -
        case 'TIMES':    return 13; // *
        case 'DIV':      return 13; // /
        case 'PERCENT':  return 13; // %
        case 'POWER':    return 14; // **
        case 'LPAREN':   return 15; // (
        default:         return null;
    }
};

function typePrecedence(tag: TokenTag): number | null {
    switch (tag) {
        case 'ARROW': return 1; // -> 
        default:      return null;
    }
};

function declaration(state: State): {i: number, match: Decl} {
    const i = skipEol(state);
    state = {...state, i};
    return oneOf(
        [
            {prefix: ['PRIVATE','TYPE','ALIAS'], parser: typeAliasDecl},
            {prefix: ['TYPE','ALIAS'],           parser: typeAliasDecl},

            {prefix: ['PRIVATE','TYPE'], parser: typeDecl},
            {prefix: ['OPAQUE','TYPE'],  parser: typeDecl},
            {prefix: ['TYPE'],           parser: typeDecl},

            {prefix: ['EXTEND','MODULE'], parser: extendModuleDecl},

            {prefix: null, parser: statementDecl},
            //{prefix: ['LOWER_NAME','EQ','LBRACE'], parser: blockDecl}, // TODO does this mean we'll have blockFnDecl, effectBlockDecl, effectBlockFnDecl?
            /*
            moduleDecl,
            functionDecl, // f(a,b) = expr
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
            module: moduleNameResult.match,
            decls: declsResult.match,
        },
    }
}

function moduleName(state: State): {i: number, match: UpperIdentifier} {
    return upperIdentifier(state);
}

//: QUALIFIER* UPPER_NAME
//= Foo
//= Foo.Bar
//= Foo.Bar.Baz
function upperIdentifier(state: State): {i: number, match: UpperIdentifier} {
    let {i} = state;
    const desc = 'module name';
    //: QUALIFIER*
    const qualifiers = [];
    while (!isAtEnd({...state, i})) {
        const iBeforeLoop = i;
        try {
            //: QUALIFIER
            const qualifierResult = getQualifier(desc,i,state.tokens);
            i = qualifierResult.i;
            qualifiers.push(qualifierResult.match);
        } catch (e) {
            i = iBeforeLoop;
            break;
        }
    }
    //: UPPER_NAME
    let nameResult = getUpperName(desc,i,state.tokens);
    i = nameResult.i;
    // Done!
    return {
        i, 
        match: {
            qualifiers, 
            name: nameResult.match,
        }
    };
}

//: QUALIFIER* LOWER_NAME
//= foo
//= Foo.bar
//= Foo.Bar.baz
function lowerIdentifier(state: State): {i: number, match: LowerIdentifier} {
    let {i} = state;
    const desc = 'identifier (lowercase)';
    //: QUALIFIER*
    const qualifiers = [];
    while (!isAtEnd({...state, i})) {
        const iBeforeLoop = i;
        try {
            //: QUALIFIER
            const qualifierResult = getQualifier(desc,i,state.tokens);
            i = qualifierResult.i;
            qualifiers.push(qualifierResult.match);
        } catch (e) {
            i = iBeforeLoop;
            break;
        }
    }
    //: LOWER_NAME
    let nameResult = getLowerName(desc,i,state.tokens);
    i = nameResult.i;
    // Done!
    return {
        i, 
        match: {
            qualifiers, 
            name: nameResult.match,
        }
    };
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

//: lowerIdentifier BANG (LPAREN expr (COMMA expr)* RPAREN)?
//= foo!
//= foo!()
//= Bar.foo!(123,456)
function bang(state: State): {i: number, match: Bang} {
    let {i} = state;
    const desc = 'bang';
    //: lowerIdentifier
    const idResult = lowerIdentifier(state);
    i = idResult.i;
    const id = idResult.match;
    //: BANG
    i = expect('BANG',desc,i,state.tokens);
    //: (LPAREN expr (COMMA expr)* RPAREN)?
    let args: Expr[] = [];
    if (tagIs('LPAREN',i,state.tokens)) {
        const listResult = list({
            left:  'LPAREN',
            right: 'RPAREN',
            sep:   'COMMA',
            item:  expr,
            state: {...state, i},
            parsedItem: `${desc} typevar list`,
            skipEol: false,
        });
        i = listResult.i;
        args = listResult.match;
    }
    // Done!
    return {
        i,
        match: (args.length == 0)
                ? {bang: 'value', val: id}
                : {bang: 'call', fn: id, args}
    };
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
    return oneOf(
        [
            {prefix: ['EOL'], parser: pipeFirstConstructorList},
            {prefix: null,    parser: shortConstructorList},
        ],
        'constructor list',
        state
    );
}

//: (EOL+ PIPE constructor)+
function pipeFirstConstructorList(state: State): {i: number, match: Constructor[]} {
    let {i} = state;
    const desc = 'constructor list';
    const constructors: Constructor[] = [];
    while (!isAtEnd({...state, i})) {
        const iBeforeLoop = i;
        try {
            //: EOL
            i = expect('EOL',desc,i,state.tokens);
            //: EOL*
            i = skipEol({...state,i});
            //: PIPE
            i = expect('PIPE',desc,i,state.tokens);
            //: constructor
            const constructorResult = constructor({...state, i});
            i = constructorResult.i;
            constructors.push(constructorResult.match);
        } catch (e) {
            i = iBeforeLoop;
            break;
        }
    }
    // Done!
    if (constructors.length == 0) {
        throw err('EXXXX','Expected non-empty list of constructors',i,state.tokens);
    }
    return {i, match: constructors};
}

//: constructor (PIPE constructor)*
//= Foo | Bar(Bool) | Baz(Int,String)
function shortConstructorList(state: State): {i: number, match: Constructor[]} {
    let {i} = state;
    const desc = 'constructor list';
    //: constructor
    const firstConstructorResult = constructor(state);
    i = firstConstructorResult.i;
    const constructors = [firstConstructorResult.match];
    //: (PIPE constructor)*
    while (!isAtEnd({...state, i})) {
        const iBeforeLoop = i;
        try {
            //: PIPE
            i = expect('PIPE',desc,i,state.tokens);
            //: constructor
            const constructorResult = constructor({...state, i});
            i = constructorResult.i;
            constructors.push(constructorResult.match);
        } catch (e) {
            i = iBeforeLoop;
            break;
        }
    }
    // Done!
    return {i, match: constructors};
}

//: UPPER_NAME (LPAREN constructorArg (COMMA constructorArg)* RPAREN)?
//= Foo
//= Bar(Int)
//= Bar(n: Int, verbose: Bool)
function constructor(state: State): {i: number, match: Constructor} {
    let {i} = state;
    const desc = 'constructor';
    //: UPPER_NAME
    const nameResult = getUpperName(desc,i,state.tokens);
    i = nameResult.i;
    //: (LPAREN constructorArg (COMMA constructorArg)* RPAREN)?
    let args: ConstructorArg[] = [];
    if (tagIs('LPAREN',i,state.tokens)) {
        const argsResult = nonemptyList({
            left:  'LPAREN',
            right: 'RPAREN',
            sep:   'COMMA',
            item:  constructorArg,
            state: {...state, i},
            parsedItem: desc,
            skipEol: false,
        });
        i = argsResult.i;
        args = argsResult.match;
    }
    // Done!
    return {
        i,
        match: {
            name: nameResult.match,
            args,
        },
    };
}

//: (LOWER_NAME COLON)? type
//= Int
//= n: Int
function constructorArg(state: State): {i: number, match: ConstructorArg} {
    let {i} = state;
    const desc = 'constructor argument';
    //: (LOWER_NAME COLON)?
    // Note: if both are not present, the LOWER_NAME can still be a part of the `type` afterwards
    let name: string|null = null;
    if (tagsAre(['LOWER_NAME','COLON'],i,state.tokens)) {
        const nameResult = getLowerName(desc,i,state.tokens);
        i = nameResult.i;
        name = nameResult.match;
        i = expect('COLON',desc,i,state.tokens);
    }
    //: type
    const typeResult = type({...state, i});
    i = typeResult.i;
    // Done!
    return {
        i,
        match: {
            name,
            type: typeResult.match,
        },
    };
}

//: LOWER_NAME
//= a
//= comparable123
function typevar(state: State): {i: number, match: Typevar} {
    return getLowerName('typevar',state.i,state.tokens);
}

function prefixType(state: State): {i: number, match: Type} {
    return oneOf(
        [
            {prefix: ['LPAREN','RPAREN'], parser: unitType},
            {prefix: ['LPAREN'],          parser: tupleOrParenthesizedType},
            {prefix: ['LBRACE'],          parser: recordType},
            {prefix: ['UPPER_NAME'],      parser: namedOrCallType},
            {prefix: ['QUALIFIER'],       parser: namedOrCallType},
            {prefix: ['LOWER_NAME'],      parser: varType},
        ],
        'type',
        state
    );
}

function type(state: State): {i: number, match: Type} {
    return typeAux(0, state);
}

function typeAux(precedence: number, state: State): {i: number, match: Type} {
    let {i} = state;

    // prefix or literal
    const prefixTypeResult = prefixType(state);
    i = prefixTypeResult.i;
    let left = prefixTypeResult.match;

    // infix or postfix
    let nextToken = state.tokens[i];
    let nextPrecedence: number | null = typePrecedence(nextToken.type.type);
    while (nextPrecedence != null && precedence < nextPrecedence) {
        switch (nextToken.type.type) {
            case 'ARROW':
                i++;
                const fnTypeResult = fnType(left, {...state, i});
                // ^ receives i pointing to the token _after_ the operator token
                i = fnTypeResult.i;
                left = fnTypeResult.match;
                nextToken = state.tokens[i];
                nextPrecedence = typePrecedence(nextToken.type.type);
                break;
            default:
                throw todo('Parser bug? Type parser infix default case', {...state, i});
        }
    }

    // Done!
    return {i, match: left};
}

//: type ARROW type
//  ^^^^^^^^^^ already parsed
//= x -> y
function fnType(left: Type, state: State): {i: number, match: Type} {
    const typeResult = type(state);
    const i = typeResult.i;
    const right = typeResult.match;
    return {i, match: {type: 'fn', from: left, to: right}};
}

//: typevar
//= a
//= comparable123
function varType(state: State): {i: number, match: Type} {
    const varResult = typevar(state);
    return {
        i: varResult.i,
        match: { type:'var', var: varResult.match },
    };
}

//: QUALIFIER* UPPER_NAME (LBRACKET type (COMMA type)* RBRACKET)?
//= Int
//= Base.Maybe
//= List.Internal.Step
//= Maybe[Int]
//= Base.List[Int]
function namedOrCallType(state: State): {i: number, match: Type} {
    let {i} = state;
    const desc = 'named or call type';
    //: QUALIFIER*
    const qualifiers = [];
    while (!isAtEnd({...state, i})) {
        const iBeforeLoop = i;
        try {
            //: QUALIFIER
            const qualifierResult = getQualifier(desc,i,state.tokens);
            i = qualifierResult.i;
            qualifiers.push(qualifierResult.match);
        } catch (e) {
            i = iBeforeLoop;
            break;
        }
    }
    //: UPPER_NAME
    let nameResult = getUpperName(desc,i,state.tokens);
    i = nameResult.i;
    const name = nameResult.match;
    //: (LBRACKET type (COMMA type)* RBRACKET)?
    let args: Type[] = [];
    if (tagIs('LBRACKET',i,state.tokens)) {
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
        args = argsResult.match;
    }
    // Done!
    return {
        i,
        match: (args.length == 0)
                ? { type: 'named', qualifiers, name }
                : { type: 'call',  qualifiers, name, args }
    }
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
    let {i} = state;
    const desc = 'record type field';
    //: LOWER_NAME
    const lowerNameResult = getLowerName(desc, i, state.tokens);
    i = lowerNameResult.i;
    //: COLON
    i = expect('COLON',desc,i,state.tokens);
    //: type
    const typeResult = type({...state, i});
    i = typeResult.i;
    // Done!
    return {i, match: {field: lowerNameResult.match, type: typeResult.match}};
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
                throw err('EXXXX',`Expected ${c.right} or ${sep} in the ${c.parsedItem}`,i,c.state.tokens);
            }
        }
    } catch (e) {
        i = iBeforeItems;
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
            throw err('EXXXX',`Expected ${c.right} or ${sep} in the ${c.parsedItem}`,i,c.state.tokens);
        }
    }
    if (!endedCorrectly) {
        throw err('EXXXX',`Unterminated list in ${c.parsedItem}`,i,c.state.tokens);
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
        throw err('EXXXX',`Unterminated list in ${c.parsedItem}`,i,c.state.tokens);
    }
    return {i, match: items};
}

type Option<T> = { 
    prefix: TokenTag[] | null,
    parser: Parser<T>,
}

function oneOf<T>(options: Option<T>[], parsedItem: string, state: State): {i: number, match: T} {
    const iBefore = state.i;
    let loc = state.tokens[state.i].loc;
    let furthestRow = loc.row;
    let furthestCol = loc.col;
    let furthestErr = null;
    for (const option of options) {
        if (option.prefix == null) {
            // try with backtracking #lazymode
            try {
                return option.parser(state);
            } catch (e) {
                if (compareLoc(e.loc,{row:furthestRow,col:furthestCol}) > 0) {
                    furthestErr = e;
                    furthestRow = e.loc.row;
                    furthestCol = e.loc.col;
                }
                state.i = iBefore;
            }
        } else {
            // if the prefix agrees, commit!
            if (tagsAre(option.prefix,state.i,state.tokens)) {
                return option.parser(state);
            }
        }
    }
    if (furthestErr == null) {
        throw err('EXXXX',`Expected ${parsedItem}`,state.i,state.tokens);
    } else {
        throw furthestErr;
    }
}

function getLowerName(parsedItem: string, i: number, tokens: Token[]): {i: number, match: string} {
    const nameToken = tokens[i];
    if (nameToken.type.type !== 'LOWER_NAME') {
        throw err('EXXXX',`Expected LOWER_NAME for a ${parsedItem}`,i,tokens);
    }
    return {
        i: i + 1, 
        match: nameToken.type.name,
    };
}

function getUpperName(parsedItem: string, i: number, tokens: Token[]): {i: number, match: string} {
    const nameToken = tokens[i];
    if (nameToken.type.type !== 'UPPER_NAME') {
        throw err('EXXXX',`Expected UPPER_NAME for a ${parsedItem}`,i,tokens);
    }
    return {
        i: i + 1, 
        match: nameToken.type.name,
    };
}

function getQualifier(parsedItem: string, i: number, tokens: Token[]): {i: number, match: string} {
    const qualifierToken = tokens[i];
    if (qualifierToken.type.type !== 'QUALIFIER') {
        throw err('EXXXX',`Expected QUALIFIER for a ${parsedItem}`,i,tokens);
    }
    return {
        i: i + 1, 
        match: qualifierToken.type.name,
    };
}

function getTags(n: number, state: State): TokenTag[] {
    return state.tokens.slice(state.i,state.i+n).map(t => t.type.type);
}

function tagIs(tag: TokenTag, i:number, tokens: Token[]): boolean {
    return tokens[i].type.type == tag;
}

function tagsAre(tags: TokenTag[], i:number, tokens: Token[]): boolean {
    return arrayEquals(tags,getTags(tags.length,{tokens,i}));
}

function rawErr(code: string, message: string, loc: Loc): CaraError {
    return { stage: 'parser', code, message, loc };
}

function err(code: string, message: string, i: number, tokens: Token[]): CaraError {
    const token = tokens[i];
    const tokenTag = token.type.type;
    const loc = token.loc;
    return rawErr(code,`Unexpected ${tokenTag}. ${message}`,loc);
}

function todo(what: string, state: State): CaraError {
    return rawErr('E9999',`TODO ${what}`,state.tokens[state.i].loc);
}

function isAtEnd(state: State): boolean {
    return tagIs('EOF',state.i,state.tokens);
}


function expect(tag: TokenTag, parsedItem: string, i: number, tokens: Token[]) {
    if (!tagIs(tag,i,tokens)) {
        throw err('EXXXX',`Expected ${tag} for a ${parsedItem}`,i,tokens);
    }
    return i + 1;
}

function compareLoc(a:Loc, b:Loc): number {
    if (a.row < b.row) return -1;
    if (a.row > b.row) return 1;
    if (a.col < b.col) return -1;
    if (a.col > b.col) return 1;
    return 0;
}

function arrayEquals<T>(a: T[], b: T[]): boolean {
    return Array.isArray(a) &&
        Array.isArray(b) &&
        a.length === b.length &&
        a.every((val, index) => val === b[index]);
}