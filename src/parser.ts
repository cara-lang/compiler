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
            throw `${e.message} @ ${showLoc(e.loc)}`;
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
            {prefix: ['COLONCOLON'],      parser: rootIdentifierExpr},

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

// Throws {e | message: string, loc: Loc}
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
