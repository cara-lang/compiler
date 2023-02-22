import {inspect} from 'node:util';
import {Decl, Stmt, Bang, Expr, Identifier, LetModifier, Type, Pattern} from './ast.ts';
import {arrayEquals,println} from './util.ts';
import {err} from './error.ts';

type Env = Map<string,Expr>;
type Envs = {
    public: Env,
    private: Env,
};

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

export function interpret(ast: Decl[]) {
    const envs: Envs = {public: new Map(), private: new Map()};
    ast.reduce(interpretDecl, envs);
}

function interpretDecl(envs: Envs, decl: Decl): Envs {
    switch (decl.decl) {
        case 'statement': return interpretStmt(envs, decl.stmt);
        default: err(`interpretDecl ${decl.decl}`);
    }
}

function interpretStmt(envs: Envs, stmt: Stmt): Envs {
    switch (stmt.stmt) {
        case 'bang': return interpretBang(envs, stmt.bang);
        case 'let': return interpretLet(envs, stmt.mod, stmt.type, stmt.lhs, stmt.body);
        default: err(`interpretStmt ${stmt.stmt}`);
    }
}

function interpretBang(envs: Envs, bang: Bang): Envs {
    const env = combineEnvs(envs);
    switch (bang.bang) {
        case 'value': interpretExpr(env, bang.val); return envs;
        case 'call': {
            const fn = interpretExpr(env, bang.fn);
            if (fn.expr == 'identifier') {
                if (idEquals(fn.id,ioPrintlnId)) {
                    if (bang.args.length == 1) {
                        ioPrintln(env,bang.args[0]);
                    } else {
                        err(`interpretBang IO.println with ${bang.args.length} args`);
                    }
                    return envs;
                } else {
                    err(`interpretBang id ${fn.id}`);
                }
            } else {
                err(`interpretBang ${bang.bang} ${bang.fn}`);
            }
        }
    }
}

function interpretLet(envs: Envs, mod: LetModifier, type: Type|null, lhs: Pattern, body: Expr): Envs {
    if (type != null) {
        // TODO typecheck the env against the type
    }
    switch (mod) {
        case 'Private': {
            // TODO do something else than NoModifier does?
            err(`interpretLet 1 ${mod} ${type} ${inspect(lhs)} ${inspect(body)}`);
            break;
        }
        case 'NoModifier': {
            const publicEnvAdditions = interpretPattern(lhs, body);
            return {...envs, public: envAdd(envs.public, publicEnvAdditions)};
        }
    }
}

// Returns env additions, instead of the whole env
function interpretPattern(lhs: Pattern, body: Expr): Env {
    switch (lhs.pattern) {
        case 'var': {
            return new Map([
                [idToString({qualifiers: [], name: lhs.var}), body],
            ]);
        }
        default: {
            err(`interpretPattern ${inspect(lhs)} ${inspect(body)}`);
        }
    }
}

function show(e: Expr): string {
    switch (e.expr) {
        case 'int':    return e.int.toString();
        case 'float':  return e.float.toString();
        case 'char':   return e.char;
        case 'string': return e.string;
        case 'bool':   return e.bool ? 'True' : 'False';
        case 'unit':   return '()';
        case 'tuple':  return `(${e.elements.map(show).join(',')})`;
        case 'list':   return `[${e.elements.map(show).join(',')}]`;
        default: err(`show(${e.expr})`);
    }
}

const ioPrintlnId: Identifier = {qualifiers:['IO'],name:'println'};
async function ioPrintln(env: Env, expr: Expr) {
    const e = interpretExpr(env,expr);
    await println(show(e));
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
        case 'constructor':
            return expr;
        case 'identifier': {
            if (isSpecial(expr.id)) return expr;
            const item = envGet(expr.id, env);
            if (item == null) {
                err(`Unknown identifier: ${inspect(expr.id)}. Env: ${inspect(env)}`);
            }
            return item;
        }
        case 'tuple': {
            return {...expr, elements: expr.elements.map((e) => interpretExpr(env,e))};
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
                        default: err("Can't NegateNum something that isn't int or float");
                    }
                    break;
                }
                default: err(`interpretExpr unary-op ${expr.op}`);
            }
            break;
        }
        case 'record-get': {
            const record = interpretExpr(env,expr.record);
            switch (record.expr) {
                case 'tuple': {
                    const index = tupleIndex(expr.field);
                    if (index == null) {
                        err(`Unsupported tuple getter: ${expr.field}`);
                    }
                    return record.elements[index];
                }
                default: err(`interpretExpr record-get ${record.expr} ${expr.field}`);
            }
            break;
        }
        default: err(`interpretExpr ${expr.expr}`);
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
                err(`Tuple index el0 is unsupported: they start at el1`);
            }
            return num - 1;
        }
    }
}