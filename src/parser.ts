import {Token, TokenTag} from './token.ts';
import {Decl, Type, Block, Pattern, UnaryOp, BinaryOp, Constructor, Typevar, TypeAliasModifier, TypeModifier, ModuleModifier, RecordTypeField, RecordExprContent, Stmt, LetModifier, Bang, Expr, LowerIdentifier, UpperIdentifier, ConstructorArg, FnArg, FnTypeArg, CaseBranch} from './ast.ts';
import {Loc} from './loc.ts';
import {hasDuplicates, arrayEquals} from './util.ts';

type State = { tokens: Token[], i: number };
type Parser<T> = (state: State) => {i: number, match: T}; // Parser<Decl> = (state: State) => {i: number, match: Decl}
type InfixParser<T> = (left: T, precedence: number, isRight: boolean, state: State) => {i: number, match: T};
type InfixParserTable<T> = (tag: TokenTag) => {precedence: number, isRight: boolean, parser: InfixParser<T>} | null;

export function parse(tokens: Token[]): Decl[] {
    let i = 0;
    const decls: Decl[] = [];
    i = skipEol({tokens,i});
    while (!isAtEnd({tokens,i})) {
        try {
            const declResult = declaration({tokens,i});
            i = declResult.i;
            decls.push(declResult.match);
            i = skipEol({tokens,i});
        } catch (e) {
            throw e.message;
        }
    }
    return decls;
}

function infixExpr(tag: TokenTag): {precedence: number, isRight: boolean, parser: InfixParser<Expr>} | null {
    switch (tag) {
        case 'ANDAND':     return {precedence:  1, isRight: false, parser: binaryOpExpr('AndBool')};        // &&

        case 'OROR':       return {precedence:  2, isRight: false, parser: binaryOpExpr('OrBool')};         // ||

        case 'PLUSPLUS':   return {precedence:  3, isRight: false, parser: binaryOpExpr('Append')};         // ++

        case 'PIPELINE':   return {precedence:  4, isRight: false, parser: pipelineExpr};                   // |>

        case 'DOTDOT':     return {precedence:  5, isRight: false, parser: rangeInclusiveExpr};             // ..
        case 'DOTDOTDOT':  return {precedence:  5, isRight: false, parser: binaryOpExpr('RangeExclusive')}; // ...

        case 'PIPE':       return {precedence:  6, isRight: false, parser: binaryOpExpr('OrBin')};          // |

        case 'CARET':      return {precedence:  7, isRight: false, parser: binaryOpExpr('XorBin')};         // ^

        case 'AND':        return {precedence:  8, isRight: false, parser: binaryOpExpr('AndBin')};         // &

        case 'EQEQ':       return {precedence:  9, isRight: false, parser: binaryOpExpr('Eq')};             // ==
        case 'NEQ':        return {precedence:  9, isRight: false, parser: binaryOpExpr('Neq')};            // !=

        case 'LTE':        return {precedence: 10, isRight: false, parser: binaryOpExpr('Lte')};            // <=
        case 'LT':         return {precedence: 10, isRight: false, parser: binaryOpExpr('Lt')};             // <
        case 'GT':         return {precedence: 10, isRight: false, parser: binaryOpExpr('Gt')};             // >
        case 'GTE':        return {precedence: 10, isRight: false, parser: binaryOpExpr('Gte')};            // >=

        case 'SHL':        return {precedence: 11, isRight: false, parser: binaryOpExpr('ShiftL')};         // <<
        case 'SHR':        return {precedence: 11, isRight: false, parser: binaryOpExpr('ShiftR')};         // >>
        case 'SHRU':       return {precedence: 11, isRight: false, parser: binaryOpExpr('ShiftRU')};        // >>>

        case 'PLUS':       return {precedence: 12, isRight: false, parser: binaryOpExpr('Plus')};           // +
        case 'MINUS':      return {precedence: 12, isRight: false, parser: binaryOpExpr('Minus')};          // -

        case 'TIMES':      return {precedence: 13, isRight: false, parser: binaryOpExpr('Times')};          // *
        case 'DIV':        return {precedence: 13, isRight: false, parser: binaryOpExpr('Div')};            // /
        case 'PERCENT':    return {precedence: 13, isRight: false, parser: binaryOpExpr('Mod')};            // %

        case 'POWER':      return {precedence: 14, isRight: true,  parser: binaryOpExpr('Pow')};            // **

        case 'LPAREN':     return {precedence: 15, isRight: true,  parser: callExpr};                       // (

        case 'GETTER':     return {precedence: 16, isRight: false, parser: recordGetExpr};                  // .abc

        default:           return null;
    }
}

function infixType(tag: TokenTag): {precedence: number, isRight: boolean, parser: InfixParser<Type>} | null {
    switch (tag) {
        case 'ARROW':    return {precedence: 1, isRight: true, parser: fnType};   // -> 
        case 'LBRACKET': return {precedence: 2, isRight: true, parser: callType}; // [
        default:         return null;
    }
}

function declaration(state: State): {i: number, match: Decl} {
    return oneOf(
        [
            // type alias
            {prefix: ['PRIVATE','TYPE','ALIAS'], parser: typeAliasDecl},
            {prefix: ['TYPE','ALIAS'],           parser: typeAliasDecl},

            // type
            {prefix: ['PRIVATE','TYPE'], parser: typeDecl},
            {prefix: ['OPAQUE','TYPE'],  parser: typeDecl},
            {prefix: ['TYPE'],           parser: typeDecl},

            // module
            {prefix: ['EXTEND','MODULE'],  parser: extendModuleDecl},
            {prefix: ['PRIVATE','MODULE'], parser: moduleDecl},
            {prefix: ['MODULE'],           parser: moduleDecl},

            // x = 123
            // x = foo!(123)
            // foo!(123)
            // private x = 123
            // x: Int = 123
            {prefix: null, parser: statementDecl}, // This needs to be before the valueAnnotationDecl to parse `x: Int = 123`

            // x : Int
            {prefix: null, parser: valueAnnotationDecl}, // can't prefix it because `x:Int = 123` is also possible and needs to be handled inside statementDecl (because of `private`!)

            // f(a,b) = expr
            // private f(a,b) = expr
            // `-`(a,b) = expr
            // `-`(a) = expr
            {prefix: null, parser: functionDecl},
            {prefix: null, parser: binaryOperatorDecl},
            {prefix: null, parser: unaryOperatorDecl},

            // f(a:Int, b:Int): Bool
            // private f(a:Int, b:Int): Bool
            {prefix: null, parser: functionAnnotationDecl},
        ],
        'declaration',
        state
    );
}

//: PRIVATE? LOWER_NAME LPAREN (fnArg (COMMA fnArg)*)? RPAREN (COLON type)? EQ EOL* expr
//= f(a,b) = a + b
//= f(a,b): Int = a + b
//= f(a: Int, b: Int) = a + b
//= f(a: Int, b: Int): Int = a + b
//= private f(a,b) = a + b
function functionDecl(state: State): {i: number, match: Decl} {
    let {i} = state;
    const desc = 'function decl';
    //: PRIVATE?
    let mod: LetModifier = 'NoModifier';
    if (tagIs('PRIVATE',i,state.tokens)) {
        mod = 'Private';
        i++;
    }
    //: LOWER_NAME
    const nameResult = getLowerName(desc,i,state.tokens);
    i = nameResult.i;
    //: LPAREN (fnArg (COMMA fnArg)*)? RPAREN
    const argsResult = list({
        left:  'LPAREN',
        right: 'RPAREN',
        sep:   'COMMA',
        item:  fnArg,
        state: {...state, i},
        parsedItem: `${desc} argument list`,
        skipEol: false,
        allowTrailingSep: false,
    });
    i = argsResult.i;
    //: (COLON type)?
    let typeVal: Type|null = null;
    if (tagIs('COLON',i,state.tokens)) {
        //: COLON
        i = expect('COLON',desc,i,state.tokens);
        //: type
        const typeResult = type({...state, i});
        i = typeResult.i;
        typeVal = typeResult.match;
    }
    //: EQ
    i = expect('EQ',desc,i,state.tokens);
    //: EOL*
    i = skipEol({...state, i});
    //: expr
    const bodyResult = expr({...state, i});
    i = bodyResult.i;
    // Done!
    return {
        i,
        match: {
            decl: 'function',
            mod,
            name: nameResult.match,
            args: argsResult.match,
            resultType: typeVal,
            body: bodyResult.match,
        },
    };
}

const unaryOps: Map<string,UnaryOp> = new Map([
    ["-","NegateNum"],
    ["!","NegateBool"],
    ["~","NegateBin"],
    ["..","InfiniteRangeInclusive"],
]);

//: PRIVATE? BACKTICK_STRING LPAREN fnArg RPAREN (COLON type)? EQ EOL* expr
//           ^^^^^^^^^^^^^^^ needs to be UnaryOp
//= `-`(a) = a + 5
//= `-`(a): Int = a + 5
//= `-`(a: Int) = a + 5
//= private `-`(a: Int) = a + 5
function unaryOperatorDecl(state: State): {i: number, match: Decl} {
    let {i} = state;
    const desc = 'unary operator decl';
    //: PRIVATE?
    let mod: LetModifier = 'NoModifier';
    if (tagIs('PRIVATE',i,state.tokens)) {
        mod = 'Private';
        i++;
    }
    //: BACKTICK_STRING
    const nameResult = getBacktickString(desc,i,state.tokens);
    i = nameResult.i;
    const op = unaryOps.get(nameResult.match);
    if (op == null) {
        throw error('Unsupported unary operator',i,state.tokens);
    }
    //: LPAREN
    i = expect('LPAREN',desc,i,state.tokens);
    //: fnArg
    const argResult = fnArg({...state,i});
    i = argResult.i;
    //: RPAREN
    i = expect('RPAREN',desc,i,state.tokens);
    //: (COLON type)?
    let typeVal: Type|null = null;
    if (tagIs('COLON',i,state.tokens)) {
        //: COLON
        i = expect('COLON',desc,i,state.tokens);
        //: type
        const typeResult = type({...state, i});
        i = typeResult.i;
        typeVal = typeResult.match;
    }
    //: EQ
    i = expect('EQ',desc,i,state.tokens);
    //: EOL*
    i = skipEol({...state, i});
    //: expr
    const bodyResult = expr({...state, i});
    i = bodyResult.i;
    // Done!
    return {
        i,
        match: {
            decl: 'unary-operator',
            mod,
            op,
            arg: argResult.match,
            resultType: typeVal,
            body: bodyResult.match,
        },
    };
}

const binaryOps: Map<string,BinaryOp> = new Map([
    ['+','Plus'],
    ['-','Minus'],
    ['*','Times'],
    ['/','Div'],
    ['%','Mod'],
    ['**','Pow'],
    ['|','OrBin'],
    ['&','AndBin'],
    ['^','XorBin'],
    ['<<','ShiftL'],
    ['>>','ShiftR'],
    ['>>>','ShiftRU'],
    ['<=','Lte'],
    ['<','Lt'],
    ['==','Eq'],
    ['!=','Neq'],
    ['>','Gt'],
    ['>=','Gte'],
    ['||','OrBool'],
    ['&&','AndBool'],
    ['++','Append'],
    ['..','RangeInclusive'],
    ['...','RangeExclusive'],
]);

//: PRIVATE? BACKTICK_STRING LPAREN fnArg COMMA fnArg RPAREN (COLON type)? EQ EOL* expr
//           ^^^^^^^^^^^^^^^ needs to be BinaryOp
//= `-`(a,b) = a * 2 + b
//= `-`(a,b): Int = a * 2 + b
//= `-`(a: Int, b: Int) = a * 2 + b
//= private `-`(a: Int, b: Int) = a * 2 + b
function binaryOperatorDecl(state: State): {i: number, match: Decl} {
    let {i} = state;
    const desc = 'binary operator decl';
    //: PRIVATE?
    let mod: LetModifier = 'NoModifier';
    if (tagIs('PRIVATE',i,state.tokens)) {
        mod = 'Private';
        i++;
    }
    //: BACKTICK_STRING
    const nameResult = getBacktickString(desc,i,state.tokens);
    i = nameResult.i;
    const op = binaryOps.get(nameResult.match);
    if (op == null) {
        throw error('Unsupported binary operator',i,state.tokens);
    }
    //: LPAREN
    i = expect('LPAREN',desc,i,state.tokens);
    //: fnArg
    const leftResult = fnArg({...state,i});
    i = leftResult.i;
    //: COMMA
    i = expect('COMMA',desc,i,state.tokens);
    //: fnArg
    const rightResult = fnArg({...state,i});
    i = rightResult.i;
    //: RPAREN
    i = expect('RPAREN',desc,i,state.tokens);
    //: (COLON type)?
    let typeVal: Type|null = null;
    if (tagIs('COLON',i,state.tokens)) {
        //: COLON
        i = expect('COLON',desc,i,state.tokens);
        //: type
        const typeResult = type({...state, i});
        i = typeResult.i;
        typeVal = typeResult.match;
    }
    //: EQ
    i = expect('EQ',desc,i,state.tokens);
    //: EOL*
    i = skipEol({...state, i});
    //: expr
    const bodyResult = expr({...state, i});
    i = bodyResult.i;
    // Done!
    return {
        i,
        match: {
            decl: 'binary-operator',
            mod,
            op,
            left: leftResult.match,
            right: rightResult.match,
            resultType: typeVal,
            body: bodyResult.match,
        },
    };
}

//: pattern (COLON type)?
//= a
//= a: Int
function fnArg(state: State): {i: number, match: FnArg} {
    let {i} = state;
    const desc = 'function argument';
    //: pattern
    const patternResult = pattern({...state,i});
    i = patternResult.i;
    //: (COLON type)?
    let typeVal: Type | null = null;
    if (tagIs('COLON',i,state.tokens)) {
        //: COLON
        i = expect('COLON',desc,i,state.tokens);
        //: type
        const typeResult = type({...state, i});
        i = typeResult.i;
        typeVal = typeResult.match;
    }
    // Done!
    return {
        i,
        match: {
            pattern: patternResult.match,
            type: typeVal,
        },
    };
}

//: PRIVATE? LOWER_NAME LPAREN (fnTypeArg (COMMA fnTypeArg)*)? RPAREN (COLON type)?
//= f(a: Int, b: Int): Bool
//= f(a: Int, b: Int)
//= f(a, b): Bool
//= f(Int, Float): Bool
//= f(Int, Float)
// note we don't allow f(a,b)
function functionAnnotationDecl(state: State): {i: number, match: Decl} {
    let {i} = state;
    const desc = 'function annotation decl';
    //: PRIVATE?
    let mod: LetModifier = 'NoModifier';
    if (tagIs('PRIVATE',i,state.tokens)) {
        mod = 'Private';
        i++;
    }
    //: LOWER_NAME
    const nameResult = getLowerName(desc,i,state.tokens);
    i = nameResult.i;
    //: LPAREN (fnTypeArg (COMMA fnTypeArg)*)? RPAREN
    const argsResult = list({
        left:  'LPAREN',
        right: 'RPAREN',
        sep:   'COMMA',
        item:  fnTypeArg,
        state: {...state, i},
        parsedItem: `${desc} argument list`,
        skipEol: false,
        allowTrailingSep: false,
    });
    i = argsResult.i;
    const args = argsResult.match;
    //: (COLON type)?
    let resultType: Type|null = null;
    const iBeforeResult = i;
    try {
        i = expect('COLON',desc,i,state.tokens);
        const resultTypeResult = type({...state,i});
        i = resultTypeResult.i;
        resultType = resultTypeResult.match;
    } catch (_) {
        i = iBeforeResult;
    }
    // Done!
    if (resultType == null && args.every((arg: FnTypeArg) => arg.type == null)) {
        throw error('Function annotation needs more types',i,state.tokens);
    }
    return {
        i,
        match: {
            decl: 'function-annotation',
            mod,
            name: nameResult.match,
            args,
            resultType,
        },
    };
}

//: LOWER_NAME COLON type
//= x : Int
function valueAnnotationDecl(state: State): {i: number, match: Decl} {
    let {i} = state;
    const desc = 'value annotation decl';
    //: LOWER_NAME
    const nameResult = getLowerName(desc,i,state.tokens);
    i = nameResult.i;
    //: COLON
    i = expect('COLON',desc,i,state.tokens);
    //: type
    const typeResult = type({...state, i});
    i = typeResult.i;
    // Done!
    return {
        i,
        match: {
            decl: 'value-annotation',
            name: nameResult.match,
            type: typeResult.match,
        },
    };
}

//: EXTEND MODULE moduleName LBRACE (EOL+ declaration)* EOL+ RBRACE
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
    //: LBRACE (EOL+ declaration*) EOL+ RBRACE
    const declsResult = nonemptyList({
        left:  'LBRACE',
        right: 'RBRACE',
        sep:   null,
        item:  declaration,
        state: {...state, i},
        parsedItem: `${desc}`,
        skipEol: true,
        allowTrailingSep: false,
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

//: PRIVATE? MODULE UPPER_NAME LBRACE (EOL+ declaration)+ EOL+ RBRACE
function moduleDecl(state: State): {i: number, match: Decl} {
    let {i} = state;
    const desc = 'module decl';
    //: PRIVATE?
    let mod: ModuleModifier = 'NoModifier';
    if (tagIs('PRIVATE',i,state.tokens)) {
        mod = 'Private';
        i++;
    }
    //: MODULE
    i = expect('MODULE',desc,i,state.tokens);
    //: UPPER_NAME
    const nameResult = getUpperName(desc,i,state.tokens);
    i = nameResult.i;
    //: LBRACE
    i = expect('LBRACE', desc,i,state.tokens);
    //: (EOL+ declaration)+
    const decls: Decl[] = [];
    while (!isAtEnd({...state, i})) {
        const iBeforeLoop = i;
        try {
            //: EOL
            i = expect('EOL',desc,i,state.tokens);
            //: EOL*
            i = skipEol({...state,i});
            //: declaration
            const declResult = declaration({...state, i});
            i = declResult.i;
            decls.push(declResult.match);
        } catch (_) {
            i = iBeforeLoop;
            break;
        }
    }
    //: EOL
    i = expect('EOL',desc,i,state.tokens);
    //: EOL*
    i = skipEol({...state,i});
    //: RBRACE
    i = expect('RBRACE', desc,i,state.tokens);
    // Done!
    return {
        i,
        match: {
            decl: 'module',
            mod,
            name: nameResult.match,
            decls,
        },
    };
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
    const desc = 'upper identifier';
    //: QUALIFIER*
    const qualifiers = [];
    while (!isAtEnd({...state, i})) {
        const iBeforeLoop = i;
        try {
            //: QUALIFIER
            const qualifierResult = getQualifier(desc,i,state.tokens);
            i = qualifierResult.i;
            qualifiers.push(qualifierResult.match);
        } catch (_) {
            i = iBeforeLoop;
            break;
        }
    }
    //: UPPER_NAME
    const nameResult = getUpperName(desc,i,state.tokens);
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
        } catch (_) {
            i = iBeforeLoop;
            break;
        }
    }
    //: LOWER_NAME
    const nameResult = getLowerName(desc,i,state.tokens);
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

//: PRIVATE? pattern (COLON type)? EQ expr
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
    // prevent _
    // TODO maybe we could just parse the pattern and then check it's not the wildcard pattern?
    if (tagIs('UNDERSCORE',i,state.tokens)) {
        throw stopTheWorldError('E0013: Assignment of expression to underscore');
    }
    //: pattern
    const lhsResult = pattern({...state, i});
    i = lhsResult.i;
    //: (COLON type)?
    let typeVal: Type | null = null;
    if (tagIs('COLON',i,state.tokens)) {
        //: COLON
        i = expect('COLON',desc,i,state.tokens);
        //: type
        const typeResult = type({...state, i});
        i = typeResult.i;
        typeVal = typeResult.match;
    }
    //: EQ
    i = expect('EQ',desc,i,state.tokens);
    //: EOL*
    i = skipEolBeforeIndented({...state,i});
    //: expr
    const exprResult = expr({...state, i});
    i = exprResult.i;
    // Done!
    return {
        i,
        match: {
            stmt: 'let',
            mod,
            type: typeVal,
            lhs: lhsResult.match,
            body: exprResult.match,
        }
    };
}

//: PRIVATE? pattern (COLON type)? EQ bang
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
    // prevent _
    // TODO maybe we could just parse the pattern and then check it's not the wildcard pattern?
    if (tagIs('UNDERSCORE',i,state.tokens)) {
        throw stopTheWorldError('E0013: Assignment of bang expression to underscore');
    }
    //: pattern
    const lhsResult = pattern({...state,i});
    i = lhsResult.i;
    //: (COLON type)?
    let typeVal: Type | null = null;
    if (tagIs('COLON',i,state.tokens)) {
        //: COLON
        i = expect('COLON',desc,i,state.tokens);
        //: type
        const typeResult = type({...state, i});
        i = typeResult.i;
        typeVal = typeResult.match;
    }
    //: EQ
    i = expect('EQ',desc,i,state.tokens);
    //: EOL*
    i = skipEolBeforeIndented({...state,i});
    //: bang
    const bangResult = bang({...state, i});
    i = bangResult.i;
    // Done!
    return {
        i,
        match: {
            stmt: 'let-bang',
            mod,
            type: typeVal,
            lhs: lhsResult.match,
            body: bangResult.match,
        }
    };
}

//: bang
//= Foo.bar!(1,False)
//= 1 |> IO.println!
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
    return exprAux(0, false, state);
}

function exprAux(precedence: number, isRight: boolean, state: State): {i: number, match: Expr} {
    return pratt({
        skipEolBeforeIndented: true,
        isRight,
        precedence,
        prefix: prefixExpr,
        infix: infixExpr,
        state,
    });
}

function prefixExpr(state: State): {i: number, match: Expr} {
    return oneOf(
        [
            {prefix: ['INT'],             parser: intExpr},
            {prefix: ['FLOAT'],           parser: floatExpr},
            {prefix: ['CHAR'],            parser: charExpr},
            {prefix: ['STRING'],          parser: stringExpr},
            {prefix: ['BACKTICK_STRING'], parser: backtickStringExpr},
            {prefix: ['GETTER'],          parser: recordGetterExpr},
            {prefix: ['TRUE'],            parser: boolExpr},
            {prefix: ['FALSE'],           parser: boolExpr},
            {prefix: ['LPAREN','RPAREN'], parser: unitExpr},
            {prefix: ['LPAREN'],          parser: tupleOrParenthesizedExpr},
            {prefix: ['LBRACKET'],        parser: listExpr},
            {prefix: ['IF'],              parser: ifExpr},
            {prefix: ['CASE'],            parser: caseExpr},
            {prefix: ['BACKSLASH'],       parser: lambdaExpr},
            {prefix: ['LHOLE'],           parser: holeLambdaExpr},
            {prefix: ['UNDERSCORE'],      parser: holeExpr},
            {prefix: ['HOLE'],            parser: holeExpr},

            // unary-op
            {prefix: ['MINUS'], parser: unaryOpExpr('number negation expr','MINUS','NegateNum')},
            {prefix: ['BANG'],  parser: unaryOpExpr('bool negation expr',  'BANG', 'NegateBool')},
            {prefix: ['TILDE'], parser: unaryOpExpr('binary negation expr','TILDE','NegateBin')},

            {prefix: null, parser: blockExpr},
            {prefix: null, parser: constructorExpr},
            {prefix: null, parser: identifierExpr},
            {prefix: null, parser: recordExpr},
        ],
        'expr',
        state
    );
}

//: expr BANG (LPAREN expr (COMMA expr)* RPAREN)?
//= foo!
//= foo!()
//= Bar.foo!(123,456)
//= x |> IO.println!
//= x |> f(1) |> Foo.bar!(1,2,3)
function bang(state: State): {i: number, match: Bang} {
    let {i} = state;
    const desc = 'bang';
    //: expr
    const exprResult = expr({...state,i});
    i = exprResult.i;
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
            parsedItem: `${desc} argument list`,
            skipEol: false,
            allowTrailingSep: false,
        });
        i = listResult.i;
        args = listResult.match;
    }
    // Done!
    const finalBang: Bang =
            (args.length == 0)
                ? {bang: 'value', val: exprResult.match}
                : {bang: 'call', fn: exprResult.match, args};
    return { i, match: finalBang };
}

//: expr ${tokenTag} expr
//  ^^^^^^^^^^^^^^^^ already parsed
function binaryOpExpr(op: BinaryOp): InfixParser<Expr> {
    return function(left: Expr, precedence: number, isRight: boolean, state: State): {i: number, match: Expr} {
        //: expr
        const exprResult = exprAux(precedence, isRight, state);
        // Done!
        return {
            i: exprResult.i,
            match: {
                expr: 'binary-op',
                op,
                left,
                right: exprResult.match,
            }
        };
    }
}

//: expr PIPELINE expr
//  ^^^^^^^^^^^^^ already parsed
//= a |> b
function pipelineExpr(left: Expr, precedence: number, isRight: boolean, state: State): {i: number, match: Expr} {
    const rightResult = exprAux(precedence, isRight, state);
    const right = rightResult.match;
    const match: Expr = pipeline({left,right});
    return { i: rightResult.i, match };
}

type PipelineOptions = {
    left: Expr,
    right: Expr,
}

function pipeline({left,right}:PipelineOptions): Expr {
    return right.expr == 'call'
            ? { ...right, args: right.args.concat(left) } // 3 |> f(1,2) ==> f(1,2,3) (special case, we inline!)
            : { expr: 'call', fn: right,args: [left] };   // 3 |> f      ==> f(3)
}

//: expr DOTDOT expr?
//  ^^^^^^^^^^^ already parsed
//= a..b
//= a..
function rangeInclusiveExpr(left: Expr, precedence: number, isRight: boolean, state: State): {i: number, match: Expr} {
    let {i} = state;
    // expr?
    let right: Expr|null = null;
    try {
        const rightResult = exprAux(precedence, isRight, state);
        i = rightResult.i;
        right = rightResult.match;
    } catch (_) {/**/}
    // Done!
    const match: Expr = (right == null)
                            ? {expr: 'unary-op', op: 'InfiniteRangeInclusive', arg: left}
                            : {expr: 'binary-op', op: 'RangeInclusive', left, right};
    return { i, match };
}

//: expr LPAREN (expr (COMMA expr)*)? RPAREN
//  ^^^^^^^^^^^ already parsed
//= x()
//= x(1)
//= x(1,2)
function callExpr(left: Expr, _precedence: number, _isRight: boolean, state: State): {i: number, match: Expr} {
    let {i} = state;
    const desc = 'call expr'
    //: LPAREN (expr (COMMA expr)*)? RPAREN
    i--; // we'll parse LPAREN as part of the list()
    const argsResult = list({
        left:  'LPAREN',
        right: 'RPAREN',
        sep:   'COMMA',
        item:  expr,
        state: {...state, i},
        parsedItem: `${desc} argument list`,
        skipEol: true,
        allowTrailingSep: false,
    });
    i = argsResult.i;
    const args = argsResult.match;
    // Done!
    return {
        i,
        match: {
            expr: 'call',
            fn: left,
            args,
        },
    };
}

//: expr GETTER
//  ^^^^^^^^^^^ already parsed, we need to i-- to access the GETTER!
//= foo.abc
//= getRecord(123).abc
function recordGetExpr(left: Expr, _precedence: number, _isRight: boolean, state: State): {i: number, match: Expr} {
    let {i} = state;
    const desc = 'record access';
    //: GETTER
    i--;
    const getterResult = getRecordGetter(desc,i,state.tokens);
    i = getterResult.i;
    // Done!
    return {
        i,
        match: {
            expr: 'record-get',
            record: left,
            field: getterResult.match,
        },
    };
}

//: INT
//= 123
function intExpr(state: State): {i: number, match: Expr} {
    const intResult = getInt('int expr',state.i,state.tokens);
    return {i: intResult.i, match: {expr: 'int', int: intResult.match}};
}

//: FLOAT
//= 123.45
function floatExpr(state: State): {i: number, match: Expr} {
    const floatResult = getFloat('float expr',state.i,state.tokens);
    return {i: floatResult.i, match: {expr: 'float', float: floatResult.match}};
}

//: CHAR
//= 'a'
function charExpr(state: State): {i: number, match: Expr} {
    const charResult = getChar('char expr',state.i,state.tokens);
    return {i: charResult.i, match: {expr: 'char', char: charResult.match}};
}

//: STRING
//= "abc"
function stringExpr(state: State): {i: number, match: Expr} {
    const stringResult = getString('string expr',state.i,state.tokens);
    return {i: stringResult.i, match: {expr: 'string', string: stringResult.match}};
}

//: BACKTICK_STRING
//= `abc`
function backtickStringExpr(state: State): {i: number, match: Expr} {
    const stringResult = getBacktickString('backtick string expr',state.i,state.tokens);
    return {i: stringResult.i, match: {expr: 'string', string: stringResult.match}};
}

//: GETTER
//= .abc
function recordGetterExpr(state: State): {i: number, match: Expr} {
    const getterResult = getRecordGetter('record getter expr',state.i,state.tokens);
    return {i: getterResult.i, match: {expr: 'record-getter', field: getterResult.match}};
}

//: TRUE|FALSE
//= True
//= False
function boolExpr(state: State): {i: number, match: Expr} {
    const tag = state.tokens[state.i].type.type;
    switch (tag) {
        case 'TRUE':  return {i: state.i + 1, match: {expr: 'bool', bool: true}};
        case 'FALSE': return {i: state.i + 1, match: {expr: 'bool', bool: false}};
        default:      throw error('Expected TRUE or FALSE',state.i,state.tokens);
    }
}

//: QUALIFIER* UPPER_NAME (LPAREN (expr (COMMA expr)*)? RPAREN)?
//= Bar
//= Foo.Bar(1,2,3)
function constructorExpr(state: State): {i: number, match: Expr} {
    let {i} = state;
    //: QUALIFIER* UPPER_NAME
    const upperIdentifierResult = upperIdentifier({...state, i});
    i = upperIdentifierResult.i;
    //: (LPAREN (expr (COMMA expr)*)? RPAREN)?
    let args: Expr[] = [];
    try {
        const argsResult = list({
            left:  'LPAREN',
            right: 'RPAREN',
            sep:   'COMMA',
            item:  expr,
            state: {...state, i},
            parsedItem: 'constructor arg',
            skipEol: true,
            allowTrailingSep: false,
        });
        args = argsResult.match;
        i = argsResult.i;
    } catch (_) {/**/}
    // Done!
    return {
        i,
        match: {
            expr: 'constructor',
            id: upperIdentifierResult.match,
            args,
        }
    };
}

//: QUALIFIER* LOWER_NAME
//= Foo.bar
function identifierExpr(state: State): {i: number, match: Expr} {
    const lowerIdentifierResult = lowerIdentifier(state);
    return {
        i: lowerIdentifierResult.i,
        match: {
            expr: 'identifier',
            id: lowerIdentifierResult.match,
        }
    };
}

//: LPAREN RPAREN
//= ()
function unitExpr(state: State): {i: number, match: Expr} {
    let {i} = state;
    const desc = 'unit expr';
    i = expect('LPAREN',desc,i,state.tokens);
    i = expect('RPAREN',desc,i,state.tokens);
    return {i, match: {expr: 'unit'}};
}

//: LPAREN expr (COMMA expr)* RPAREN
//= (1)
//= (1,True,foo)
function tupleOrParenthesizedExpr(state: State): {i: number, match: Expr} {
    const listResult = nonemptyList({
        left:  'LPAREN',
        right: 'RPAREN',
        sep:   'COMMA',
        item:  expr,
        state: state,
        parsedItem: 'tuple expr or parenthesized expr',
        skipEol: true,
        allowTrailingSep: false,
    });

    const match: Expr = listResult.match.length == 1
        ?  listResult.match[0] // parenthesized expr: return the child
        :  {expr: 'tuple', elements: listResult.match};

    return {i: listResult.i, match};
}

//: LBRACKET expr (COMMA expr)* RBRACKET
//= []
//= [1]
//= [1,2]
function listExpr(state: State): {i: number, match: Expr} {
    const listResult = list({
        left:  'LBRACKET',
        right: 'RBRACKET',
        sep:   'COMMA',
        item:  expr,
        state: state,
        parsedItem: 'list expr',
        skipEol: true,
        allowTrailingSep: false,
    });
    return {i: listResult.i, match: {expr:'list', elements: listResult.match}};
}

//: LBRACE (recordExprContent (COMMA recordExprContent)*)? RBRACE
//= {a:1, b:True}
//= {...a, x:123}
function recordExpr(state: State): {i: number, match: Expr} {
    const desc = 'record expr';
    const listResult = list({
        left:  'LBRACE',
        right: 'RBRACE',
        sep:   'COMMA',
        item:  recordExprContent,
        state,
        parsedItem: desc,
        skipEol: true,
        allowTrailingSep: true,
    });
    if (hasDuplicates(listResult.match.map((c) => c.recordContent == 'spread' ? '...' : c.field).filter((f) => f !== '...'))) {
        throw stopTheWorldError('E0006: Record created with duplicate fields');
    }
    return {
        i: listResult.i,
        match: {expr: 'record', contents: listResult.match},
    }
}

function recordExprContent(state: State): {i: number, match: RecordExprContent} {
    return oneOf(
        [
            {prefix: ['LOWER_NAME','COLON'], parser: recordExprFieldContent},
            {prefix: ['LOWER_NAME'],         parser: recordExprPunContent},
            {prefix: ['DOTDOTDOT'],          parser: recordExprSpreadContent},
        ],
        'record expr content',
        state,
    );
}

//: LOWER_NAME COLON expr
//= a: 1
function recordExprFieldContent(state: State): {i: number, match: RecordExprContent} {
    let {i} = state;
    const desc = 'record expr field content';
    //: LOWER_NAME
    const lowerNameResult = getLowerName(desc, i, state.tokens);
    i = lowerNameResult.i;
    //: COLON
    i = expect('COLON',desc,i,state.tokens);
    //: expr
    const exprResult = expr({...state, i});
    i = exprResult.i;
    // Done!
    return {i, match: {recordContent:'field', field: lowerNameResult.match, value: exprResult.match}};
}

//: LOWER_NAME
//= a
function recordExprPunContent(state: State): {i: number, match: RecordExprContent} {
    let {i} = state;
    const desc = 'record expr pun content';
    //: LOWER_NAME
    const lowerNameResult = getLowerName(desc, i, state.tokens);
    i = lowerNameResult.i;
    // Done!
    return {i, match: {recordContent:'pun', field: lowerNameResult.match}};
}

//: DOTDOTDOT lowerIdentifier
//= ...a, ...Foo.a
function recordExprSpreadContent(state: State): {i: number, match: RecordExprContent} {
    let {i} = state;
    const desc = 'record expr spread content';
    //: DOTDOTDOT
    i = expect('DOTDOTDOT',desc,i,state.tokens);
    //: lowerIdentifier
    const idResult = lowerIdentifier({...state, i});
    i = idResult.i;
    // Done!
    return {i, match: {recordContent:'spread', recordId: idResult.match}};
}

//: moduleName? block
//= { 
//    x = 1
//    y = 1 + x
//    (x,y)
//  }
//= Maybe { 
//    head = doc.head!
//    title = head.title!
//    title != ""
//  }
function blockExpr(state: State): {i: number, match: Expr} {
    let {i} = state;
    //: moduleName?
    let monadModule: UpperIdentifier|null = null;
    const iBeforeMonadModule = i;
    try {
        const moduleResult = moduleName({...state, i});
        i = moduleResult.i;
        monadModule = moduleResult.match;
    } catch (_) {
        i = iBeforeMonadModule;
    }
    //: block
    const blockResult = block({...state,i});
    i = blockResult.i;
    // Done!
    if (monadModule == null) {
        return {
            i,
            match: {
                expr: 'block',
                block: blockResult.match,
            },
        };
    } else {
        return {
            i,
            match: {
                expr: 'effect-block',
                monadModule,
                block: blockResult.match,
            },
        };
    }
}

//: LBRACE (EOL+ stmt)+ (EOL+ expr)? EOL+ RBRACE
function block(state: State): {i: number, match: Block} {
    let {i} = state;
    const desc = 'block';
    //: LBRACE
    i = expect('LBRACE',desc,i,state.tokens);
    //: (EOL+ stmt)+
    i = expect('EOL',desc,i,state.tokens);
    i = skipEol({...state, i});
    const firstStmtResult = statement({...state, i});
    i = firstStmtResult.i;
    const stmts = [firstStmtResult.match];
    while (!isAtEnd({...state, i})) {
        const iBeforeLoop = i;
        try {
            i = expect('EOL',desc,i,state.tokens);
            i = skipEol({...state, i});
            const stmtResult = statement({...state, i});
            i = stmtResult.i;
            stmts.push(stmtResult.match);
        } catch (_) {
            i = iBeforeLoop;
            break;
        }
    }
    //: (EOL+ expr)?
    let ret: Expr|null = null;
    const iBeforeRet = i;
    try {
        //: EOL
        i = expect('EOL',desc,i,state.tokens);
        //: EOL*
        i = skipEol({...state, i});
        //: expr
        const exprResult = expr({...state, i});
        i = exprResult.i;
        ret = exprResult.match;
    } catch (_) {
        i = iBeforeRet;
    }
    //: EOL
    i = expect('EOL',desc,i,state.tokens);
    //: EOL*
    i = skipEol({...state, i});
    //: RBRACE
    i = expect('RBRACE',desc,i,state.tokens);
    // Done!
    return {
        i,
        match: {
            stmts,
            ret,
        },
    };
}

//: IF expr THEN expr ELSE expr
//= if 1 == 2 then foo() else bar()
function ifExpr(state: State): {i: number, match: Expr} {
    let {i} = state;
    const desc = 'if expr';
    //: IF
    i = expect('IF',desc,i,state.tokens);
    i = skipEolBeforeIndented({...state,i});
    //: expr
    const conditionResult = expr({...state,i});
    i = conditionResult.i;
    i = skipEolBeforeIndented({...state,i});
    //: THEN
    i = expect('THEN',desc,i,state.tokens);
    i = skipEolBeforeIndented({...state,i});
    //: expr
    const thenResult = expr({...state,i});
    i = thenResult.i;
    i = skipEolBeforeIndented({...state,i});
    //: ELSE
    if (!tagIs('ELSE',i,state.tokens)) {
        throw stopTheWorldError('E0021: If expression without an else branch');
    }
    i = expect('ELSE',desc,i,state.tokens);
    i = skipEolBeforeIndented({...state,i});
    //: expr
    const elseResult = expr({...state,i});
    i = elseResult.i;
    // Done!
    return {
        i,
        match: {
            expr: 'if',
            cond: conditionResult.match,
            then: thenResult.match,
            else: elseResult.match,
        },
    };
}

//: CASE expr OF (EOL+ caseBranch)*
//= case foo of
//    1 -> "Hello"
//    2 -> "World"
//    _ -> "!"
//= case foo of
//    1 | 2 -> "Hello"
//    _ -> "!"
function caseExpr(state: State): {i: number, match: Expr} {
    let {i} = state;
    const desc = 'case expr';
    //: CASE
    i = expect('CASE',desc,i,state.tokens);
    i = skipEolBeforeIndented({...state,i});
    //: expr
    const subjectResult = expr({...state,i});
    i = subjectResult.i;
    i = skipEolBeforeIndented({...state,i});
    //: OF
    i = expect('OF',desc,i,state.tokens);
    //: (EOL+ caseBranch)*
    const branches: CaseBranch[] = [];
    while (!isAtEnd({...state,i})) {
        const iBeforeLoop = i;
        try {
            //: EOL
            i = expect('EOL',desc,i,state.tokens);
            //: EOL*
            i = skipEolBeforeIndented({...state,i});
            //: caseBranch
            const branchResult = caseBranch({...state,i});
            i = branchResult.i;
            branches.push(branchResult.match);
        } catch (_) {
            i = iBeforeLoop;
            break;
        }
    }
    // Done!
    return {
        i,
        match: {
            expr: 'case',
            subject: subjectResult.match,
            branches,
        },
    };
}

//: pattern (PIPE pattern)* ARROW expr
//= 1 -> "Hello"
//= 1 | 2 -> "Hello"
function caseBranch(state: State): {i: number, match: CaseBranch} {
    let {i} = state;
    const desc = 'case branch';
    //: pattern
    const patternResult = pattern({...state, i});
    i = patternResult.i;
    const orPatterns: Pattern[] = [patternResult.match];
    //: (PIPE pattern)*
    while (!isAtEnd({...state,i})) {
        const iBeforeLoop = i;
        try {
            i = skipEolBeforeIndented({...state,i});
            //: PIPE
            i = expect('PIPE',desc,i,state.tokens);
            i = skipEolBeforeIndented({...state,i});
            //: pattern
            const nextPatternResult = pattern({...state, i});
            i = nextPatternResult.i;
            orPatterns.push(nextPatternResult.match);
        } catch (_) {
            i = iBeforeLoop;
            break;
        }
    }
    i = skipEolBeforeIndented({...state,i});
    //: ARROW
    i = expect('ARROW',desc,i,state.tokens);
    i = skipEolBeforeIndented({...state,i});
    //: expr
    const bodyResult = expr({...state,i});
    i = bodyResult.i;
    // Done!
    return {
        i,
        match: {
            orPatterns,
            body: bodyResult.match,
        }
    }
}

//: BACKSLASH LOWER_NAME (COMMA LOWER_NAME)* ARROW expr
function lambdaExpr(state: State): {i: number, match: Expr} {
    let {i} = state;
    const desc = 'lambda expr';
    //: BACKSLASH LOWER_NAME (COMMA LOWER_NAME)* ARROW
    const argsResult = list({
        left:  'BACKSLASH',
        right: 'ARROW',
        sep:   'COMMA',
        item:  pattern,
        state,
        parsedItem: `${desc} argument list`,
        skipEol: true,
        allowTrailingSep: false,
    });
    i = argsResult.i;
    //: expr
    const bodyResult = expr({...state,i});
    i = bodyResult.i;
    // Done!
    return {
        i,
        match: {
            expr: 'lambda',
            args: argsResult.match,
            body: bodyResult.match,
        },
    };
}

//: HOLE|UNDERSCORE
//= _
//= _1
function holeExpr(state: State): {i: number, match: Expr} {
    let {i} = state;
    const desc = 'hole';
    if (tagIs('UNDERSCORE',i,state.tokens)) {
        i = expect('UNDERSCORE',desc,i,state.tokens);
        return {
            i,
            match: {
                expr: 'identifier',
                id: {
                    qualifiers: [],
                    name: '_',
                },
            },
        };
    } else {
        const holeResult = getHole(desc,i,state.tokens);
        i = holeResult.i;
        return {
            i,
            match: {
                expr: 'identifier',
                id: {
                    qualifiers: [],
                    name: `_${holeResult.match}`,
                },
            },
        };
    }
}

//: LHOLE expr RPAREN
//= #(1 + _)
//= #(1 + _1 - _2)
function holeLambdaExpr(state: State): {i: number, match: Expr} {
    let {i} = state;
    const desc = 'hole lambda expr';
    //: LHOLE
    i = expect('LHOLE',desc,i,state.tokens);
    //: expr
    const bodyResult = expr({...state,i});
    i = bodyResult.i;
    //: RPAREN
    i = expect('RPAREN',desc,i,state.tokens);
    // Done!
    return {
        i,
        match: lambdaWithHoles(bodyResult.match)
    };
}

type HoleAnalysis =
    | {type: 'no-holes'}
    | {type: 'only-underscore'}
    | {type: 'only-numbered', maxHole: number}
    | {type: 'mixed'};

function lambdaWithHoles(body: Expr): Expr {
    const analyzed = analyzeHoles(body);
    switch (analyzed.type) {
        case 'no-holes':
            return {
                expr: 'lambda',
                args: [],
                body,
            };
        case 'only-underscore':
            return {
                expr: 'lambda',
                args: [{pattern: 'var', var: '_'}],
                body,
            };
        case 'only-numbered': {
            const args: Pattern[] = 
                [...Array(analyzed.maxHole).keys()]
                    .map(n => ({pattern: 'var', var: `_${n+1}`}));
            return {
                expr: 'lambda',
                args,
                body,
            };
        }
        case 'mixed':
            throw stopTheWorldError('E0020: Anonymous function shorthand with mixed holes');
    }
}

function analyzeHoles(expr: Expr): HoleAnalysis {
    switch (expr.expr) {
        case 'identifier':
            if (expr.id.qualifiers.length == 0 && expr.id.name.startsWith('_')) {
                if (expr.id.name.length == 1) {
                    return {type:'only-underscore'};
                } else {
                    const holeNumber = parseInt(expr.id.name.substring(1),10);
                    return {type:'only-numbered', maxHole: holeNumber};
                }
            } else {
                return {type:'no-holes'};
            }
        case 'int':
        case 'float':
        case 'char':
        case 'string':
        case 'bool':
        case 'unit':
        case 'record-getter':
            return {type:'no-holes'};
        case 'constructor':
            return analyzeHolesList(expr.args.map(analyzeHoles));
        case 'lambda':
        case 'closure':
            return analyzeHoles(expr.body);
        case 'unary-op':
            return analyzeHoles(expr.arg);
        case 'record-get':
            return analyzeHoles(expr.record);
        case 'tuple':
        case 'list':
            return analyzeHolesList(expr.elements.map(analyzeHoles));
        case 'record':
            return analyzeHolesList(expr.contents.map(analyzeHolesRecordContent));
        case 'call':
            return combineHoles(
                analyzeHoles(expr.fn),
                analyzeHolesList(expr.args.map(analyzeHoles)),
            );
        case 'if':
            return analyzeHolesList([expr.cond,expr.then,expr.else].map(analyzeHoles));
        case 'case':
            return combineHoles(
                analyzeHoles(expr.subject),
                analyzeHolesList(expr.branches.map((branch: CaseBranch) => analyzeHoles(branch.body))),
            );
        case 'binary-op':
            return analyzeHolesList([expr.left,expr.right].map(analyzeHoles));
        case 'block':
        case 'effect-block':
            return combineHoles(
                expr.block.ret ? analyzeHoles(expr.block.ret) : {type:'no-holes'},
                analyzeHolesList(expr.block.stmts.map(analyzeHolesStmt)),
            );
    }
}

function analyzeHolesStmt(stmt: Stmt): HoleAnalysis {
    switch (stmt.stmt) {
        case 'let':      return analyzeHoles(stmt.body);
        case 'let-bang': return analyzeHolesBang(stmt.body);
        case 'bang':     return analyzeHolesBang(stmt.bang);
    }
}

function analyzeHolesBang(bang: Bang): HoleAnalysis {
    switch (bang.bang) {
        case 'value': return {type:'no-holes'};
        case 'call':  return analyzeHolesList(bang.args.map(analyzeHoles));
    }
}

function analyzeHolesRecordContent(content: RecordExprContent): HoleAnalysis {
    switch (content.recordContent) {
        case 'field':  return analyzeHoles(content.value);
        case 'pun':    return {type:'no-holes'};
        case 'spread': return {type:'no-holes'};
    }
}

function analyzeHolesList(holes: HoleAnalysis[]): HoleAnalysis {
    return holes.reduce(combineHoles, {type:'no-holes'})
}

function combineHoles(a: HoleAnalysis, b: HoleAnalysis): HoleAnalysis {
    if (a.type == 'no-holes') return b;
    if (b.type == 'no-holes') return a;
    if (a.type == 'mixed')    return a;
    if (b.type == 'mixed')    return b;
    if (a.type == 'only-numbered' && b.type == 'only-underscore') {
        return {type: 'mixed'};
    }
    if (b.type == 'only-numbered' && a.type == 'only-underscore') {
        return {type: 'mixed'};
    }
    if (a.type == 'only-underscore' && b.type == 'only-underscore') {
        return {type: 'only-underscore'};
    }
    if (a.type == 'only-numbered' && b.type == 'only-numbered') {
        return {
            type: 'only-numbered',
            maxHole: Math.max(a.maxHole,b.maxHole)
        };
    }
    bug('combineHoles: fell through all the options', {a,b});
}

function pattern(state: State): {i: number, match: Pattern} {
    return oneOf(
        [
            {prefix: ['LPAREN','RPAREN'],     parser: unitPattern},
            {prefix: ['LOWER_NAME'],          parser: varPattern},
            {prefix: ['UPPER_NAME'],          parser: constructorPattern},
            {prefix: ['QUALIFIER'],           parser: constructorPattern},
            {prefix: ['INT'],                 parser: intPattern},
            {prefix: ['FLOAT'],               parser: floatPattern},
            {prefix: ['LPAREN'],              parser: tuplePattern},
            {prefix: ['LBRACKET'],            parser: listPattern},
            {prefix: ['MINUS'],               parser: negatedPattern},
            {prefix: ['UNDERSCORE'],          parser: wildcardPattern},
            {prefix: ['DOTDOTDOT'],           parser: spreadPattern},
            {prefix: ['LBRACE','DOTDOT'],     parser: recordSpreadPattern},
            {prefix: ['LBRACE','LOWER_NAME'], parser: recordFieldsPattern},
            // TODO other patterns
        ],
        'pattern',
        state,
    )
}

//: LOWER_NAME
//= abc
function varPattern(state: State): {i: number, match: Pattern} {
    const desc = 'var pattern';
    const varResult = getLowerName(desc,state.i,state.tokens);
    return {
        i: varResult.i,
        match: {
            pattern: 'var',
            var: varResult.match,
        },
    };
}

//: QUALIFIER* UPPER_NAME (LPAREN pattern (COMMA pattern)* RPAREN)?
//= Foo
//= Base.Foo
//= Foo(1)
//= Foo(_)
function constructorPattern(state: State): {i: number, match: Pattern} {
    let {i} = state;
    const desc = 'constructor pattern';
    //: QUALIFIER* UPPER_NAME
    const idResult = upperIdentifier({...state,i});
    i = idResult.i;
    //: (LPAREN pattern (COMMA pattern)* RPAREN)?
    let args: Pattern[] = [];
    try {
        const argsResult = nonemptyList({
            left:  'LPAREN',
            right: 'RPAREN',
            sep:   'COMMA',
            item:  pattern,
            state: {...state, i},
            parsedItem: `${desc} arguments`,
            skipEol: false,
            allowTrailingSep: false,
        });
        i = argsResult.i;
        args = argsResult.match;
    } catch (_) {/**/}
    // Done!
    return {
        i,
        match: {
            pattern: 'constructor',
            id: idResult.match,
            args,
        },
    };
}

//: INT
//= 123
function intPattern(state: State): {i: number, match: Pattern} {
    const desc = 'int pattern';
    const intResult = getInt(desc,state.i,state.tokens);
    return {
        i: intResult.i,
        match: {
            pattern: 'int',
            int: intResult.match,
        },
    };
}

//: FLOAT
//= 123.45
function floatPattern(state: State): {i: number, match: Pattern} {
    const desc = 'float pattern';
    const floatResult = getFloat(desc,state.i,state.tokens);
    return {
        i: floatResult.i,
        match: {
            pattern: 'float',
            float: floatResult.match,
        },
    };
}

//: LBRACKET (pattern (COMMA pattern)*)? RBRACKET
//= []
//= [a]
//= [1,a]
function listPattern(state: State): {i: number, match: Pattern} {
    const desc = 'list pattern';
    const listResult = list({
        left:  'LBRACKET',
        right: 'RBRACKET',
        sep:   'COMMA',
        item:  pattern,
        state,
        parsedItem: `${desc} elements`,
        skipEol: false,
        allowTrailingSep: false,
    });
    return {
        i: listResult.i,
        match: {
            pattern: 'list',
            elements: listResult.match,
        },
    };
}

//: LPAREN RPAREN
//= ()
function unitPattern(state: State): {i: number, match: Pattern} {
    const desc = 'unit pattern';
    let {i} = state;
    i = expect('LPAREN',desc,i,state.tokens);
    i = expect('RPAREN',desc,i,state.tokens);
    return { i, match: { pattern: 'unit' } };
}

//: LPAREN (pattern (COMMA pattern)*)? RPAREN
//= (a)
//= (1,a)
function tuplePattern(state: State): {i: number, match: Pattern} {
    const desc = 'tuple pattern';
    const listResult = nonemptyList({
        left:  'LPAREN',
        right: 'RPAREN',
        sep:   'COMMA',
        item:  pattern,
        state,
        parsedItem: `${desc} elements`,
        skipEol: false,
        allowTrailingSep: false,
    });
    return {
        i: listResult.i,
        match: {
            pattern: 'tuple',
            elements: listResult.match,
        },
    };
}

//: MINUS (intPattern | floatPattern)
//= -123
//= -123.45
function negatedPattern(state: State): {i: number, match: Pattern} {
    let {i} = state;
    const desc = 'negated pattern';
    i = expect('MINUS',desc,i,state.tokens);
    const numResult = oneOf(
        [
            {prefix: ['INT'],   parser: intPattern},
            {prefix: ['FLOAT'], parser: floatPattern},
        ],
        desc,
        {...state,i},
    );
    i = numResult.i;
    switch (numResult.match.pattern) {
        case 'int':   numResult.match.int   = -numResult.match.int;   break;
        case 'float': numResult.match.float = -numResult.match.float; break;
    }
    return {i, match: numResult.match};
}

//: UNDERSCORE
//= _
function wildcardPattern(state: State): {i: number, match: Pattern} {
    const desc = 'wildcard pattern';
    const i = expect('UNDERSCORE',desc,state.i,state.tokens);
    return {i, match: {pattern:'wildcard'}};
}

//: DOTDOTDOT (varPattern | wildcardPattern)
//= ...a
//= ..._
function spreadPattern(state: State): {i: number, match: Pattern} {
    let {i} = state;
    const desc = 'spread pattern';
    i = expect('DOTDOTDOT',desc,i,state.tokens);
    const varResult = oneOf(
        [
            {prefix: ['LOWER_NAME'], parser: varPattern},
            {prefix: ['UNDERSCORE'], parser: wildcardPattern},
        ],
        desc,
        {...state,i},
    );
    i = varResult.i;
    const varName = 
        varResult.match.pattern == 'var'
            ? varResult.match.var
            : null;
    return {i, match: {pattern:'spread', var:varName}};
}

//: LBRACE DOTDOT RBRACE
//= {..}
function recordSpreadPattern(state: State): {i: number, match: Pattern} {
    let {i} = state;
    const desc = 'record spread pattern';
    i = expect('LBRACE',desc,i,state.tokens);
    i = expect('DOTDOT',desc,i,state.tokens);
    i = expect('RBRACE',desc,i,state.tokens);
    return {i, match: {pattern:'record-spread'}};
}

//: LBRACE LOWER_NAME (COMMA LOWER_NAME)* RBRACE
//= {..}
function recordFieldsPattern(state: State): {i: number, match: Pattern} {
    const desc = 'record fields pattern';
    const fieldsResult = nonemptyList({
        left:  'LBRACE',
        right: 'RBRACE',
        sep:   'COMMA',
        item:  lowerName,
        state: state,
        parsedItem: `${desc} field list`,
        skipEol: false,
        allowTrailingSep: false,
    });
    return {
        i: fieldsResult.i,
        match: {
            pattern: 'record-fields',
            fields: fieldsResult.match,
        },
    };
}

//: ${tokenTag} expr
function unaryOpExpr(desc: string, tokenTag: TokenTag, op: UnaryOp): Parser<Expr> {
    return function(state: State): {i: number, match: Expr} {
        let {i} = state;
        //: ${tokenTag}
        i = expect(tokenTag,desc,i,state.tokens);
        //: expr
        const exprResult = expr({...state, i});
        i = exprResult.i;
        // Done!
        return {
            i,
            match: {
                expr: 'unary-op',
                op,
                arg: exprResult.match,
            }
        };
    }
}

//: PRIVATE? TYPE ALIAS UPPER_NAME (LBRACKET typevar (COMMA typevar)* RBRACKET)? EQ EOL* type
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
    const nameResult = getUpperName(desc,i,state.tokens);
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
            allowTrailingSep: false,
        });
        i = listResult.i;
        vars = listResult.match;
    }
    //: EQ
    i = expect('EQ',desc,i,state.tokens);
    //: EOL*
    i = skipEolBeforeIndented({...state,i});
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
    const nameResult = getUpperName(desc,i,state.tokens);
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
            allowTrailingSep: false,
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

//: (EOL+ PIPE)? EOL* constructor (EOL* PIPE EOL* constructor)*
//= Foo | Bar(Bool) | Baz(Int,String)
//= | Foo | Bar(Bool) | Baz(Int,String)
function constructorList(state: State): {i: number, match: Constructor[]} {
    let {i} = state;
    const desc = 'constructor list';
    //: (EOL* PIPE)?
    const iBeforeOptional = i;
    try {
        //: EOL*
        i = skipEol({...state, i});
        //: PIPE
        i = expect('PIPE',desc,i,state.tokens);
    } catch (_) {
        i = iBeforeOptional;
    }
    //: EOL*
    i = skipEol({...state, i});
    //: constructor
    const firstConstructorResult = constructor({...state, i});
    i = firstConstructorResult.i;
    const constructors = [firstConstructorResult.match];
    //: (EOL* PIPE EOL* constructor)*
    while (!isAtEnd({...state, i})) {
        const iBeforeLoop = i;
        try {
            //: EOL*
            i = skipEol({...state, i});
            //: PIPE
            i = expect('PIPE',desc,i,state.tokens);
            //: EOL*
            i = skipEol({...state, i});
            //: constructor
            const constructorResult = constructor({...state, i});
            i = constructorResult.i;
            constructors.push(constructorResult.match);
        } catch (_) {
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
            allowTrailingSep: false,
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

//: (LOWER_NAME | type | (LOWER_NAME COLON type))
//= n
//= Int
//= n: Int
function fnTypeArg(state: State): {i: number, match: FnTypeArg} {
    let {i} = state;
    const desc = 'typed function argument';
    let name: string|null = null;
    let typeVal: Type|null = null;
    let needsType = false;
    //: (LOWER_NAME COLON?)?
    if (tagIs('LOWER_NAME',i,state.tokens)) {
        const nameResult = getLowerName(desc,i,state.tokens);
        i = nameResult.i;
        name = nameResult.match;
        if (tagIs('COLON',i,state.tokens)) {
            i++;
            needsType = true;
        }
    }
    // type
    if (needsType) {
        const typeResult = type({...state, i});
        i = typeResult.i;
        typeVal = typeResult.match;
    } else {
        try {
            const typeResult = type({...state, i});
            i = typeResult.i;
            typeVal = typeResult.match;
        } catch (_) {/**/}
    }
    // Done!
    if (name == null && typeVal == null) {
        bug('fnTypeArg',null,{...state,i});
    }
    return { i, match: { name, type: typeVal } };
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
            {prefix: ['UPPER_NAME'],      parser: namedType},
            {prefix: ['QUALIFIER'],       parser: namedType},
            {prefix: ['LOWER_NAME'],      parser: varType},
        ],
        'type',
        state
    );
}

function type(state: State): {i: number, match: Type} {
    return typeAux(0, false, state);
}

function typeAux(precedence: number, isRight: boolean, state: State): {i: number, match: Type} {
    return pratt({
        skipEolBeforeIndented: false,
        isRight,
        precedence,
        prefix: prefixType,
        infix: infixType,
        state,
    });
}

type PrattConfig<T> = {
    skipEolBeforeIndented: boolean,
    isRight: boolean,
    precedence: number, 
    prefix: Parser<T>, 
    infix: InfixParserTable<T>, 
    state: State,
}

function pratt<T>(c: PrattConfig<T>): {i: number, match: T} {
    let {i} = c.state;
    c.precedence = c.isRight ? c.precedence - 1 : c.precedence;

    // prefix or literal
    const prefixResult = c.prefix(c.state);
    i = prefixResult.i;
    let left = prefixResult.match;

    // infix or postfix
    const iBeforeFirstSkip = i;
    if (c.skipEolBeforeIndented) i = skipEolBeforeIndented({...c.state,i});
    let nextToken = c.state.tokens[i];
    let next: {precedence: number, isRight: boolean, parser: InfixParser<T>} | null = c.infix(nextToken.type.type);
    if (next == null) i = iBeforeFirstSkip; // Don't eat EOL prematurely, this would throw off (EOL+ declaration)* in moduleDecl() etc.

    while (next != null && c.precedence < next.precedence) {
        i++;
        const nextResult = next.parser(left, next.precedence, next.isRight, {...c.state, i});
        i = nextResult.i;
        left = nextResult.match;
        const iBeforeLastSkip = i;
        if (c.skipEolBeforeIndented) i = skipEolBeforeIndented({...c.state,i});
        nextToken = c.state.tokens[i];
        next = c.infix(nextToken.type.type);
        if (next == null) i = iBeforeLastSkip; // Don't eat EOL prematurely, this would throw off (EOL+ declaration)* in moduleDecl() etc.
    }

    // Done!
    return {i, match: left};
}

//: type ARROW type
//  ^^^^^^^^^^ already parsed
//= x -> y
function fnType(left: Type, precedence: number, isRight: boolean, state: State): {i: number, match: Type} {
    const typeResult = typeAux(precedence, isRight, state);
    const i = typeResult.i;
    const right = typeResult.match;
    return {i, match: {type: 'fn', from: left, to: right}};
}

//: type LBRACKET type (COMMA type)* RBRACKET
//  ^^^^^^^^^^^^^ already parsed
//= List[a]
function callType(left: Type, _precedence: number, _isRight: boolean, state: State): {i: number, match: Type} {
    let {i} = state;
    const desc = 'call type';
    i--; // we'll parse LBRACKET as part of the list
    //: LBRACKET type (COMMA type)* RBRACKET
    const argsResult = nonemptyList({
        left:  'LBRACKET',
        right: 'RBRACKET',
        sep:   'COMMA',
        item:  type,
        state: {...state, i},
        parsedItem: `${desc} arg list`,
        skipEol: false,
        allowTrailingSep: false,
    });
    i = argsResult.i;
    const args = argsResult.match;
    return {i, match: {type: 'call', fn: left, args}};
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

//: QUALIFIER* UPPER_NAME
//= Int
//= Base.Maybe
//= List.Internal.Step
function namedType(state: State): {i: number, match: Type} {
    let {i} = state;
    const desc = 'named type';
    //: QUALIFIER*
    const qualifiers = [];
    while (!isAtEnd({...state, i})) {
        const iBeforeLoop = i;
        try {
            //: QUALIFIER
            const qualifierResult = getQualifier(desc,i,state.tokens);
            i = qualifierResult.i;
            qualifiers.push(qualifierResult.match);
        } catch (_) {
            i = iBeforeLoop;
            break;
        }
    }
    //: UPPER_NAME
    const nameResult = getUpperName(desc,i,state.tokens);
    i = nameResult.i;
    const name = nameResult.match;
    // Done!
    return {
        i,
        match: {
            type: 'named',
            qualifiers,
            name,
        }
    };
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
        allowTrailingSep: true,
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
    const desc = 'unit type';
    i = expect('LPAREN',desc,i,state.tokens);
    i = expect('RPAREN',desc,i,state.tokens);
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
        allowTrailingSep: false,
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

//: EOL*
function skipEolBeforeIndented(state: State): number {
    let {i} = state;
    while (tagIs('EOL',i,state.tokens) && state.tokens[i+1].loc.col > 1) {
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
    allowTrailingSep: boolean,
};

//: left (item (sep item)*)? right
function list<T>(c: ListConfig<T>): {i: number, match: T[]} {
    return (c.sep == null)
        ? nonseparatedList(c)
        : separatedList(c);
}

//: left item* right
function nonseparatedList<T>(c: ListConfig<T>): {i: number, match: T[]} {
    let {i} = c.state;
    //: left
    i = expect(c.left,c.parsedItem,i,c.state.tokens);
    if (c.skipEol) i = skipEol({...c.state,i});
    //: item*
    const items: T[] = [];
    const iBeforeItems = i;
    try {
        while (!isAtEnd({...c.state,i})) {
            if (tagIs(c.right,i,c.state.tokens)) {
                break; // we'll consume `right` outside this try{}catch{} block
            } else {
                const nextItem = c.item({...c.state,i});
                i = nextItem.i;
                items.push(nextItem.match);
                if (c.skipEol) i = skipEol({...c.state,i});
            }
        }
    } catch (_) {
        i = iBeforeItems;
    }
    //: right
    i = expect(c.right,c.parsedItem,i,c.state.tokens);
    return {i, match: items};
}

//: left (item (sep item)*)? right
function separatedList<T>(c: ListConfig<T>): {i: number, match: T[]} {
    let {i} = c.state;
    const sep = c.sep!; // we're guaranteed this by the condition in list()
    //: left
    i = expect(c.left,c.parsedItem,i,c.state.tokens);
    if (c.skipEol) i = skipEol({...c.state,i});
    //: (item (sep item)*)?
    const items: T[] = [];
    const iBeforeItems = i;
    try {
        const firstItem = c.item({...c.state,i});
        i = firstItem.i;
        items.push(firstItem.match);
        if (c.skipEol) i = skipEol({...c.state,i});
        while (!isAtEnd({...c.state,i})) {
            if (tagIs(c.right,i,c.state.tokens)) {
                break; // we'll consume `right` outside this try{}catch{} block
            } else if (tagIs(sep,i,c.state.tokens)) {
                i++;
                if (c.allowTrailingSep) {
                    const iBeforeItem = i;
                    try {
                        if (c.skipEol) i = skipEol({...c.state,i});
                        const nextItem = c.item({...c.state,i});
                        i = nextItem.i;
                        items.push(nextItem.match);
                        if (c.skipEol) i = skipEol({...c.state,i});
                    } catch (_) {
                        i = iBeforeItem;
                        if (c.skipEol) i = skipEol({...c.state,i});
                        break;
                    }
                } else {
                    if (c.skipEol) i = skipEol({...c.state,i});
                    const nextItem = c.item({...c.state,i});
                    i = nextItem.i;
                    items.push(nextItem.match);
                    if (c.skipEol) i = skipEol({...c.state,i});
                }
            } else {
                throw error(`Expected ${c.right} or ${sep} in the ${c.parsedItem}`,i,c.state.tokens);
            }
        }
    } catch (_) {
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
    if (c.skipEol) i = skipEol({...c.state, i});
    //: item
    const firstItem = c.item({...c.state,i});
    i = firstItem.i;
    const items: T[] = [firstItem.match];
    let endedCorrectly = false;
    if (c.skipEol) i = skipEol({...c.state, i});
    while (!isAtEnd({...c.state,i})) {
        if (tagIs(c.right,i,c.state.tokens)) {
            endedCorrectly = true;
            i++;
            break;
        } else if (tagIs(sep,i,c.state.tokens)) {
            i++;
            if (c.allowTrailingSep) {
                const iBeforeItem = i;
                try {
                    if (c.skipEol) i = skipEol({...c.state, i});
                    const nextItem = c.item({...c.state,i});
                    i = nextItem.i;
                    items.push(nextItem.match);
                    if (c.skipEol) i = skipEol({...c.state, i});
                } catch (_) {
                    i = iBeforeItem;
                    if (c.skipEol) i = skipEol({...c.state, i});
                    break;
                }
            } else {
                if (c.skipEol) i = skipEol({...c.state, i});
                const nextItem = c.item({...c.state,i});
                i = nextItem.i;
                items.push(nextItem.match);
                if (c.skipEol) i = skipEol({...c.state, i});
            }
        } else {
            throw error(`Expected ${c.right} or ${sep} in the ${c.parsedItem}`,i,c.state.tokens);
        }
    }
    if (!endedCorrectly) {
        throw error(`Unterminated list in ${c.parsedItem}`,i,c.state.tokens);
    }
    return {i, match: items};
}

//: left item+ right
function nonemptyNonseparatedList<T>(c: ListConfig<T>): {i: number, match: T[]} {
    let {i} = c.state;
    //: left
    i = expect(c.left,c.parsedItem,i,c.state.tokens);
    if (c.skipEol) i = skipEol({...c.state, i});
    //: item
    const firstItem = c.item({...c.state,i});
    i = firstItem.i;
    const items: T[] = [firstItem.match];
    //: item*
    let endedCorrectly = false;
    if (c.skipEol) i = skipEol({...c.state, i});
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
            if (c.skipEol) i = skipEol({...c.state, i});
        }
    }
    if (!endedCorrectly) {
        throw error(`Unterminated list in ${c.parsedItem}`,i,c.state.tokens);
    }
    return {i, match: items};
}

type OneOfOption<T> = { 
    prefix: TokenTag[] | null,
    parser: Parser<T>,
}

// Throws {e | message: string}
function oneOf<T>(options: OneOfOption<T>[], parsedItem: string, state: State): {i: number, match: T} {
    const iBefore = state.i;
    const loc = state.tokens[state.i].loc;
    let furthestRow = loc.row;
    let furthestCol = loc.col;
    let furthestErr = null;
    for (const option of options) {
        if (option.prefix == null) {
            // try with backtracking #lazymode
            try {
                return option.parser(state);
            } catch (e) {
                switch (e.type) {
                    case 'stop-the-world-error': throw e;
                    case 'parser-error': {
                        if (compareLoc(e.loc,{row:furthestRow,col:furthestCol}) > 0) {
                            furthestErr = e;
                            furthestRow = e.loc.row;
                            furthestCol = e.loc.col;
                        }
                        state.i = iBefore;
                        break;
                    }
                    default: throw e;
                }
            }
        } else {
            // if the prefix agrees, commit!
            if (tagsAre(option.prefix,state.i,state.tokens)) {
                return option.parser(state);
            }
        }
    }
    if (furthestErr == null) {
        throw error(`Expected ${parsedItem}`,state.i,state.tokens);
    } else {
        throw furthestErr;
    }
}

function getInt(parsedItem: string, i: number, tokens: Token[]): {i: number, match: bigint} {
    const intToken = tokens[i];
    if (intToken.type.type !== 'INT') {
        throw error(`Expected INT for a ${parsedItem}`,i,tokens);
    }
    return {
        i: i + 1, 
        match: intToken.type.int,
    };
}

function getFloat(parsedItem: string, i: number, tokens: Token[]): {i: number, match: number} {
    const floatToken = tokens[i];
    if (floatToken.type.type !== 'FLOAT') {
        throw error(`Expected FLOAT for a ${parsedItem}`,i,tokens);
    }
    return {
        i: i + 1, 
        match: floatToken.type.float,
    };
}

function getChar(parsedItem: string, i: number, tokens: Token[]): {i: number, match: string} {
    const charToken = tokens[i];
    if (charToken.type.type !== 'CHAR') {
        throw error(`Expected CHAR for a ${parsedItem}`,i,tokens);
    }
    return {
        i: i + 1, 
        match: charToken.type.char,
    };
}

function getString(parsedItem: string, i: number, tokens: Token[]): {i: number, match: string} {
    const stringToken = tokens[i];
    if (stringToken.type.type !== 'STRING') {
        throw error(`Expected STRING for a ${parsedItem}`,i,tokens);
    }
    return {
        i: i + 1, 
        match: stringToken.type.string,
    };
}

function getBacktickString(parsedItem: string, i: number, tokens: Token[]): {i: number, match: string} {
    const stringToken = tokens[i];
    if (stringToken.type.type !== 'BACKTICK_STRING') {
        throw error(`Expected BACKTICK_STRING for a ${parsedItem}`,i,tokens);
    }
    return {
        i: i + 1, 
        match: stringToken.type.string,
    };
}

function getRecordGetter(parsedItem: string, i: number, tokens: Token[]): {i: number, match: string} {
    const getterToken = tokens[i];
    if (getterToken.type.type !== 'GETTER') {
        throw error(`Expected GETTER for a ${parsedItem}`,i,tokens);
    }
    return {
        i: i + 1, 
        match: getterToken.type.field,
    };
}

function lowerName(state:State): {i: number, match: string} {
    return getLowerName('lower name', state.i, state.tokens);
}


function getLowerName(parsedItem: string, i: number, tokens: Token[]): {i: number, match: string} {
    const nameToken = tokens[i];
    if (nameToken.type.type !== 'LOWER_NAME') {
        throw error(`Expected LOWER_NAME for a ${parsedItem}`,i,tokens);
    }
    return {
        i: i + 1, 
        match: nameToken.type.name,
    };
}

function getUpperName(parsedItem: string, i: number, tokens: Token[]): {i: number, match: string} {
    const nameToken = tokens[i];
    if (nameToken.type.type !== 'UPPER_NAME') {
        throw error(`Expected UPPER_NAME for a ${parsedItem}`,i,tokens);
    }
    return {
        i: i + 1, 
        match: nameToken.type.name,
    };
}

function getQualifier(parsedItem: string, i: number, tokens: Token[]): {i: number, match: string} {
    const qualifierToken = tokens[i];
    if (qualifierToken.type.type !== 'QUALIFIER') {
        throw error(`Expected QUALIFIER for a ${parsedItem}`,i,tokens);
    }
    return {
        i: i + 1, 
        match: qualifierToken.type.name,
    };
}

function getHole(parsedItem: string, i: number, tokens: Token[]): {i: number, match: number} {
    const holeToken = tokens[i];
    if (holeToken.type.type !== 'HOLE') {
        throw error(`Expected HOLE for a ${parsedItem}`,i,tokens);
    }
    return {
        i: i + 1, 
        match: holeToken.type.n,
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

type Error =
    | {type:'parser-error',message:string,loc:Loc}
    | {type:'stop-the-world-error',message:string}

function parserError(message: string, loc: Loc): Error {
    return {type: 'parser-error', message, loc};
}

function stopTheWorldError(message: string): Error {
    return {type: 'stop-the-world-error', message};
}

function error(message: string, i: number, tokens: Token[]): Error {
    const token = tokens[i];
    const tokenTag = token.type.type;
    const loc = token.loc;
    return parserError(`Unexpected ${tokenTag}. ${message}`,loc);
}

function showLoc(loc: Loc): string {
    return `${loc.row}:${loc.col}`;
}

function bug(message: string, extra: Record<string,unknown>|null, state?: State): never {
    const shownLoc = state ? ` @ ${showLoc(state.tokens[state.i].loc)}` : '';
    const shownExtra = extra ? `, ${extra}` : '';
    throw `BUG: ${message}${shownLoc}${shownExtra}`;
}

function isAtEnd(state: State): boolean {
    return tagIs('EOF',state.i,state.tokens);
}

function expect(tag: TokenTag, parsedItem: string, i: number, tokens: Token[]) {
    if (!tagIs(tag,i,tokens)) {
        throw error(`Expected ${tag} for a ${parsedItem}`,i,tokens);
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