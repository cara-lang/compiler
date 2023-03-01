import {Decl, Stmt, Bang, Typevar, Constructor, Expr, Identifier, LetModifier, Type, Pattern, BinaryOp, RecordExprContent, TypeModifier} from './ast.ts';
import {stringify,arrayEquals,print,eprintln,addToMap} from './util.ts';

type Env = Map<string,Expr>;
type Envs = {
    public: Env,
    private: Env,
};

type State = {
    envs: Envs,
    stdout: string,
    stderr: string,
};

let typeConstructorI = 0;

function combineEnvs(envs: Envs): Env {
    return envAdd(envs.public, envs.private);
}

// `a` with items from `b` added. On collision, `b` wins.
function envAdd(a: Env, b: Env): Env {
    return new Map([
        ...a,
        ...b,
    ]);
}

function envGet(id: Identifier, env: Env): Expr | undefined {
    const str = idToString(id);
    return env.get(str);
}

function idToString(id: Identifier): string {
    return id.qualifiers.join('.') + id.name;
}

export async function interpret(ast: Decl[]): Promise<{code: number, stdout: string, stderr: string}> {
    const envs: Envs = {public: new Map(), private: new Map()};
    let state: State = {envs, stdout: '', stderr: ''};
    for (const decl of ast) {
        try {
            state = await interpretDecl(state, decl);
        } catch (e) {
            await eprintln(e);
            return {code: 1, stdout: state.stdout, stderr: state.stderr + e};
        }
    }
    return {code: 0, stdout: state.stdout, stderr: state.stderr};
}

async function interpretDecl(state: State, decl: Decl): Promise<State> {
    switch (decl.decl) {
        case 'statement': return await interpretStmt(state, decl.stmt);
        case 'type': return Promise.resolve(interpretType(state, decl.name, decl.mod, decl.vars, decl.constructors));
        case 'type-alias': {
            // TODO do something about the type alias. We can ignore it in the meantime while we have no type checking.
            return Promise.resolve(state);
        }
        case 'value-annotation': {
            // TODO do something about the value annotation. We can ignore it in the meantime while we have no type checking.
            return Promise.resolve(state);
        }
        case 'function-annotation': {
            // TODO do something about the function annotation. We can ignore it in the meantime while we have no type checking.
            return Promise.resolve(state);
        }
        default: throw `interpretDecl ${decl.decl}`;
    }
}
function interpretType(state: State, _name: string, mod: TypeModifier, _vars: Typevar[], constructors: Constructor[]): State {
    // TODO register the type somewhere
    let newPublicEnv  = state.envs.public;
    let newPrivateEnv = state.envs.private;
    switch (mod) {
        case 'NoModifier': {
            newPublicEnv = constructors.reduce(interpretTypeConstructor,newPublicEnv);
            break;
        }
        case 'Private': {
            newPrivateEnv = constructors.reduce(interpretTypeConstructor,newPrivateEnv);
            break;
        }
        case 'Opaque': break;
    }
    const newEnvs = {public: newPublicEnv, private: newPrivateEnv};
    return {...state, envs: newEnvs};
}

function interpretTypeConstructor(env: Env, constructor: Constructor): Env {
    const constructorId = {qualifiers:[], name: constructor.name};
    if (constructor.args.length == 0) {
        const expr: Expr = {expr:'constructor', id: constructorId, args: []};
        return addToMap(env,constructor.name,expr);
    } else {
        const argNames: string[] = constructor.args.map(() => `#${typeConstructorI++}`);
        const lambdaArgs: Pattern[] = argNames.map((argName) => ({pattern:'var', var: argName}));
        const constructorArgs: Expr[] = argNames.map((argName) => ({expr:'identifier', id: {qualifiers:[], name: argName}}));
        const expr: Expr = {
            expr: 'lambda',
            args: lambdaArgs,
            body: {expr:'constructor', id: constructorId, args: constructorArgs},
        };
        return addToMap(env,constructor.name,expr);
    }
}

async function interpretStmt(state: State, stmt: Stmt): Promise<State> {
    switch (stmt.stmt) {
        case 'bang': {
            const result = await interpretBang(state, stmt.bang);
            return result.state; // ignore the expr, it's going to be ()
        }
        case 'let-bang': {
            const result = await interpretBang(state, stmt.body);
            const newEnvs = interpretLet(result.state.envs, stmt.mod, stmt.type, stmt.lhs, result.expr);
            return {...result.state, envs: newEnvs};
        }
        case 'let': {
            const newEnvs = interpretLet(state.envs, stmt.mod, stmt.type, stmt.lhs, stmt.body);
            return {...state, envs: newEnvs};
        }
    }
}

async function interpretBang(state: State, bang: Bang): Promise<{state:State,expr:Expr}> {
    const env = combineEnvs(state.envs);
    switch (bang.bang) {
        case 'value': {
            const expr = interpretExpr(env, bang.val);
            return {state,expr};
        }
        case 'call': {
            const fn = interpretExpr(env, bang.fn);
            if (fn.expr == 'identifier') {
                if (idEquals(fn.id,ioPrintlnId)) {
                    if (bang.args.length == 1) {
                        const arg = bang.args[0];
                        const newStdout = await ioPrintln(env,arg);
                        const returnExpr: Expr = {expr:'unit'};
                        return {
                            state: {...state, stdout: state.stdout + newStdout},
                            expr: returnExpr,
                        };
                    } else {
                        throw `interpretBang IO.println with ${bang.args.length} args`;
                    }
                } else {
                    throw `interpretBang id ${fn.id}`;
                }
            } else {
                throw `interpretBang ${bang.bang} ${bang.fn}`;
            }
        }
    }
}

function interpretLet(envs: Envs, mod: LetModifier, type: Type|null, lhs: Pattern, body: Expr): Envs {
    if (type != null) {
        // TODO typecheck the env against the type
    }
    const env = combineEnvs(envs);
    switch (mod) {
        case 'Private': {
            // TODO do something else than NoModifier does?
            throw `interpretLet 1 ${mod} ${type} ${stringify(lhs)} ${stringify(body)}`;
        }
        case 'NoModifier': {
            const publicEnvAdditions = interpretPattern(lhs, interpretExpr(env,body));
            if (publicEnvAdditions == null) {
                throw `Pattern didn't match the expr: ${showPattern(lhs)} = ${show(body)}`;
            }
            return {...envs, public: envAdd(envs.public, publicEnvAdditions)};
        }
    }
}

// Returns (NEW) env additions, instead of the whole env.
// null is returned if the pattern doesn't match the expr
function interpretPattern(pattern: Pattern, expr: Expr): Env|null {
    switch (pattern.pattern) {
        case 'var': {
            return new Map([
                [idToString({qualifiers: [], name: pattern.var}), expr],
            ]);
        }
        case 'int': {
            if (expr.expr != 'int') return null;
            if (expr.int != pattern.int) return null;
            return new Map();
        }
        case 'wildcard': {
            return new Map();
        }
        case 'record-fields': {
            if (expr.expr != 'record') return null;
            const exprFields = ensureRecordFieldsOnly(expr.contents);
            const additions: Env = new Map();
            for (const field of pattern.fields) {
                let found = false;
                for (const exprField of exprFields) {
                    if (exprField.field == field) {
                        found = true;
                        additions.set(field, exprField.value);
                        break;
                    }
                }
                if (!found) return null;
            }
            return additions;
        }
        case 'record-spread': {
            if (expr.expr != 'record') return null;
            const fields = ensureRecordFieldsOnly(expr.contents);
            return new Map(fields.map(({field,value}) => [field,value]));
        }
        default: throw `interpretPattern ${stringify(pattern.pattern)}`;
    }
}

function show(e: Expr): string {
    switch (e.expr) {
        case 'int':         return e.int.toString();
        case 'float':       return e.float.toString();
        case 'char':        return e.char;
        case 'string':      return e.string;
        case 'bool':        return e.bool ? 'True' : 'False';
        case 'unit':        return '()';
        case 'tuple':       return `(${e.elements.map(show).join(',')})`;
        case 'list':        return `[${e.elements.map(show).join(',')}]`;
        case 'record':      return `{${e.contents.map(showRecordExprContent).join(',')}}`;
        case 'call':        return `${show(e.fn)}(${e.args.map(show).join(',')})`;
        case 'lambda':      return `\\${e.args.map(showPattern).join(',')} -> ${show(e.body)}`;
        case 'binary-op':   return `(${show(e.left)} ${showBinaryOp(e.op)} ${show(e.right)})`;
        case 'identifier':  return showIdentifier(e.id);
        case 'constructor': return showConstructor(e.id, e.args);
        case 'closure':     return '<function>';
        default: throw `show(${e.expr})`;
    }
}

function showRecordExprContent(c: RecordExprContent): string {
    switch (c.recordContent) {
        case 'field':  return `${c.field}:${show(c.value)}`;
        case 'pun':    return c.field;
        case 'spread': return `...${showIdentifier(c.recordId)}`;
    }
}

function showIdentifier(id: Identifier): string {
    if (id.qualifiers.length == 0) return id.name;
    return `${id.qualifiers.join('.')}.${id.name}`;
}

function showConstructor(id: Identifier, args: Expr[]): string {
    const idString = showIdentifier(id);
    return args.length == 0
        ? idString
        : `${idString}(${args.map(show).join(',')})`;
}

function showPattern(p: Pattern): string {
    switch (p.pattern) {
        case 'int':   return p.int.toString();
        case 'float': return p.float.toString();
        case 'unit':  return '()';
        case 'tuple': return `(${p.elements.map(showPattern).join(',')})`;
        case 'list':  return `[${p.elements.map(showPattern).join(',')}]`;
        case 'var':   return p.var;
        default: throw `showPattern(${p.pattern})`;
    }
}

function showBinaryOp(op: BinaryOp): string {
    switch (op) {
        case 'Plus':           return '+';
        case 'Minus':          return '-';
        case 'Times':          return '*';
        case 'Div':            return '/';
        case 'Mod':            return '%';
        case 'Pow':            return '**';
        case 'OrBin':          return '|  ';
        case 'AndBin':         return '&  ';
        case 'XorBin':         return '^  ';
        case 'ShiftL':         return '<< ';
        case 'ShiftR':         return '>> ';
        case 'ShiftRU':        return '>>>';
        case 'Lte':            return '<=';
        case 'Lt':             return '<';
        case 'Eq':             return '==';
        case 'Neq':            return '!=';
        case 'Gt':             return '>';
        case 'Gte':            return '>=';
        case 'OrBool':         return '||';
        case 'AndBool':        return '&&';
        case 'Append':         return '++';
        case 'RangeInclusive': return '..';
        case 'RangeExclusive': return '...';
    }
}

const ioPrintlnId: Identifier = {qualifiers:['IO'],name:'println'};
async function ioPrintln(env: Env, expr: Expr): Promise<string> {
    const e = interpretExpr(env,expr);
    const str = show(e) + "\n";
    await print(str);
    return str;
}

const specialIds: Identifier[] = [
    ioPrintlnId,
];

function idEquals(a:Identifier,b:Identifier): boolean {
    return arrayEquals(a.qualifiers,b.qualifiers) && a.name == b.name;
}

function isSpecial(id: Identifier): boolean {
    return specialIds.some(specialId => idEquals(id,specialId));
}

function interpretExpr(env: Env, expr: Expr): Expr {
    switch (expr.expr) {
        case 'int':
        case 'float':
        case 'char': 
        case 'string': 
        case 'unit': 
        case 'bool': 
        case 'closure': 
        case 'record-getter': 
            return expr;
        case 'constructor':
            return {...expr, args: expr.args.map((e) => interpretExpr(env,e))};
        case 'identifier': {
            if (isSpecial(expr.id)) return expr;
            const item = envGet(expr.id, env);
            if (item == null) {
                throw `Unknown identifier: ${show(expr)}. Env: ${stringify([...env.entries()])}`;
            }
            return item;
        }
        case 'tuple': {
            return {...expr, elements: expr.elements.map((e) => interpretExpr(env,e))};
        }
        case 'list': {
            return {...expr, elements: expr.elements.map((e) => interpretExpr(env,e))};
        }
        case 'record': {
            return {...expr, contents: expr.contents.flatMap((c) => interpretRecordExprContent(env,c))};
        }
        case 'lambda': {
            return {...expr, expr:'closure', env};
        }
        case 'unary-op': {
            const arg = interpretExpr(env,expr.arg);
            switch (expr.op) {
                case 'NegateNum': {
                    switch (arg.expr) {
                        case 'int': {
                            arg.int = -arg.int;
                            return arg;
                        }
                        case 'float': {
                            arg.float = -arg.float;
                            return arg;
                        }
                        default: throw "Can't NegateNum something that isn't int or float";
                    }
                }
                default: throw `interpretExpr unary-op ${expr.op}`;
            }
        }
        case 'binary-op': {
            const left = interpretExpr(env,expr.left);
            const right = interpretExpr(env,expr.right);
            switch (expr.op) {
                case 'Plus': {
                    if (left.expr == 'int'   && right.expr == 'int')   return {expr: 'int',   int:   left.int + right.int};
                    if (left.expr == 'float' && right.expr == 'float') return {expr: 'float', float: left.float + right.float};
                    if (left.expr == 'int'   && right.expr == 'float') return {expr: 'float', float: Number(left.int) + right.float}; // TODO not ideal
                    if (left.expr == 'float' && right.expr == 'int')   return {expr: 'float', float: left.float + Number(right.int)}; // TODO not ideal
                    throw `interpretExpr binary-op Plus ${left.expr} ${right.expr}`;
                }
                case 'Times': {
                    if (left.expr == 'int'   && right.expr == 'int')   return {expr: 'int',   int:   left.int * right.int};
                    if (left.expr == 'float' && right.expr == 'float') return {expr: 'float', float: left.float * right.float};
                    if (left.expr == 'int'   && right.expr == 'float') return {expr: 'float', float: Number(left.int) * right.float}; // TODO not ideal
                    if (left.expr == 'float' && right.expr == 'int')   return {expr: 'float', float: left.float * Number(right.int)}; // TODO not ideal
                    throw `interpretExpr binary-op Times ${left.expr} ${right.expr}`;
                }
                case 'Append': {
                    if (left.expr == 'list' && right.expr == 'list') return {expr: 'list', elements: left.elements.concat(right.elements)};
                    if (                       right.expr == 'list') return {expr: 'list', elements: [left].concat(right.elements)};
                    if (left.expr == 'list'                        ) return {expr: 'list', elements: left.elements.concat(right)};
                    throw `interpretExpr binary-op Append ${left.expr} ${right.expr}`;
                }
                case 'Eq': {
                    if (left.expr != right.expr) throw 'Tried to compare values of different types (typechecker should have caught this!)';
                    if (left.expr == 'int' && right.expr == 'int') return {expr: 'bool', bool: left.int == right.int};
                    throw `interpretExpr binary-op Eq for two ${left.expr}s`;
                }
                default: throw `interpretExpr binary-op ${expr.op}`;
            }
        }
        case 'if': {
            const cond = interpretExpr(env,expr.cond);
            if (cond.expr != 'bool') throw 'E0025: If expression with a non-bool condition';
            if (cond.bool) return interpretExpr(env,expr.then);
            else           return interpretExpr(env,expr.else);
        }
        case 'case': {
            const subject = interpretExpr(env, expr.subject);
            for (const branch of expr.branches) {
                const envAddition: Env|null = matchAnyPattern(subject, branch.orPatterns);
                if (envAddition != null) {
                    const newEnv = envAdd(env, envAddition);
                    return interpretExpr(newEnv, branch.body);
                }
            }
            throw `interpretExpr case - nothing matched! ${show(expr.subject)} ${stringify(expr.branches)}`;
        }
        case 'record-get': {
            return recordGet(env,expr.record,expr.field);
        }
        case 'call': {
            const fn = interpretExpr(env,expr.fn);
            const args = expr.args.map((e) => interpretExpr(env,e));
            switch (fn.expr) {
                case 'record-getter': {
                    if (args.length !== 1) {
                        throw `interpretExpr BUG: record getter called with more than onen argument`;
                    }
                    return recordGet(env,args[0],fn.field);
                }
                case 'closure': {
                    const innerEnv: Env = applyClosureArgumentPatterns(
                        fn.env, // lexical closure baby!
                        fn.args,
                        args
                    ); 
                    return interpretExpr(innerEnv, fn.body);
                }
                default: throw `interpretExpr call ${fn.expr} ${stringify(args)}`;
            }
        }
        default: throw `interpretExpr ${expr.expr}`;
    }
}

function matchAnyPattern(subject: Expr, patterns: Pattern[]): Env|null {
    for (const pattern of patterns) {
        const result = interpretPattern(pattern, subject);
        if (result != null) return result;
    }
    return null;
}

function applyClosureArgumentPatterns(env:Env, argPatterns:Pattern[], argValues:Expr[]): Env {
    if (argPatterns.length !== argValues.length) {
        throw `Wrong number of arguments provided: ${argPatterns.length} needed, ${argValues.length} provided`;
    }
    const envAdditions: Env[] = 
        argPatterns
            .flatMap((pattern,i) => {
                const envAddition = interpretPattern(pattern,argValues[i]);
                return envAddition == null ? [] : [envAddition];
            });
    return envAdditions.reduce(envAdd,env);
}

function interpretRecordExprContent(env: Env, content: RecordExprContent): RecordExprContent[] {
    switch (content.recordContent) {
        case 'field': return [content];
        case 'pun':   return [{
                                recordContent: 'field',
                                field: content.field,
                                value: interpretExpr(
                                    env,
                                    {
                                        expr: 'identifier',
                                        id: {qualifiers: [], name: content.field}
                                    }
                                )
                            }];
        case 'spread': {
            const record = interpretExpr(env, {expr: 'identifier', id: content.recordId});
            if (record.expr != 'record') throw `Tried to spread a non-record value when creating a record`;
            return record.contents;
        }
    }
}

function ensureRecordFieldsOnly(contents: RecordExprContent[]): {field:string, value:Expr}[] {
    return contents.map((content) => {
        if (content.recordContent == 'field') return {field: content.field, value: content.value};
        throw "BUG: Record didn't exclusively contain fields. Did we forget to interpretExpr it first?";
    });
}

function recordGet(env: Env, record: Expr, field: string): Expr {
    const rec = interpretExpr(env,record);
    switch (rec.expr) {
        case 'tuple': {
            const index = tupleIndex(field);
            if (index == null)                throw 'E0023: Trying to access a missing tuple field';
            if (index >= rec.elements.length) throw 'E0023: Trying to access a missing tuple field';
            return rec.elements[index];
        }
        case 'record': {
            for (const c of rec.contents) {
                switch (c.recordContent) {
                    case 'field': {
                        if (c.field == field) return c.value;
                        break;
                    }
                    case 'pun':    throw `recordGet BUG: got pun`;
                    case 'spread': throw `recordGet BUG: got spread`;
                }
            }
            throw `Unknown record field ${field}`;
        }
        default: throw `recordGet ${rec.expr} ${field}`;
    }
}

function tupleIndex(field: string): number|null {
    switch (field) {
        case 'first':   return 0;
        case 'second':  return 1;
        case 'third':   return 2;
        case 'fourth':  return 3;
        case 'fifth':   return 4;
        case 'sixth':   return 5;
        case 'seventh': return 6;
        case 'eighth':  return 7;
        case 'ninth':   return 8;
        case 'tenth':   return 9;
        default: {
            const match = field.match(/el(\d+)/);
            if (match == null) return null;
            const num = parseInt(match[1],10);
            if (num == 0) {
                throw `Tuple index el0 is unsupported: they start at el1`;
            }
            return num - 1;
        }
    }
}
