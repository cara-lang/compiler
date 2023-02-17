import {Decl, Stmt, Bang, Expr, Identifier} from './ast.ts';
import {arrayEquals,println} from './util.ts';
import {err} from './error.ts';

type Env = Map<Identifier,Expr>;

export function interpret(ast: Decl[]) {
    const initEnv: Env = new Map();
    ast.reduce(interpretDecl, initEnv);
}

function interpretDecl(env: Env, decl: Decl): Env {
    switch (decl.decl) {
        case 'statement': return interpretStmt(env, decl.stmt);
        default: err(`interpretDecl ${decl.decl}`);
    }
}

function interpretStmt(env: Env, stmt: Stmt): Env {
    switch (stmt.stmt) {
        case 'bang': return interpretBang(env, stmt.bang);
        default: err(`interpretStmt ${stmt.stmt}`);
    }
}

function interpretBang(env: Env, bang: Bang): Env {
    switch (bang.bang) {
        case 'value': interpretExpr(env, bang.val); return env;
        case 'call': {
            const fn = interpretExpr(env, bang.fn);
            if (fn.expr == 'identifier') {
                if (idEquals(fn.id,ioPrintlnId)) {
                    if (bang.args.length == 1) {
                        ioPrintln(env,bang.args[0]);
                    } else {
                        err(`interpretBang IO.println with ${bang.args.length} args`);
                    }
                    return env;
                } else {
                    err(`interpretBang id ${fn.id}`);
                }
            } else {
                err(`interpretBang ${bang.bang} ${bang.fn}`);
            }
        }
    }
}


const ioPrintlnId: Identifier = {qualifiers:['IO'],name:'println'};
async function ioPrintln(env: Env, expr: Expr) {
    const e = interpretExpr(env,expr);
    switch (e.expr) {
        case 'int': await println(e.int.toString()); break;
        default: err(`IO.println(${e.expr})`);
    }
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
        case 'int': return expr;
        case 'identifier': {
            if (isSpecial(expr.id)) return expr;
            err(`interpretExpr id ${expr.id}`);
            break;
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
        default: err(`interpretExpr ${expr.expr}`);
    }
}
