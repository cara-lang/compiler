import process from 'node:process';
import {Decl, Stmt, Bang, Expr, Identifier} from './ast.ts';
import {CaraError} from './error.ts';
import {Loc} from './loc.ts';
import {arrayEquals} from './util.ts';

type Env = Map<Identifier,Expr>;

export function interpret(ast: Decl[]) {
    const initEnv: Env = new Map();
    ast.reduce(interpretDecl, initEnv);
}

function interpretDecl(env: Env, decl: Decl): Env {
    switch (decl.decl) {
        case 'statement': return interpretStmt(env, decl.stmt);
        default: throw todo(`interpretDecl ${decl.decl}`);
    }
}

function interpretStmt(env: Env, stmt: Stmt): Env {
    switch (stmt.stmt) {
        case 'bang': return interpretBang(env, stmt.bang);
        default: throw todo(`interpretStmt ${stmt.stmt}`);
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
                        throw todo(`interpretBang IO.println with ${bang.args.length} args`);
                    }
                    return env;
                } else {
                    throw todo(`interpretBang id ${fn.id}`);
                }
            } else {
                throw todo(`interpretBang ${bang.bang} ${bang.fn}`);
            }
        }
    }
}

const ioPrintlnId: Identifier = {qualifiers:['IO'],name:'println'};
function ioPrintln(_env: Env, expr: Expr) {
    const print = process.stdout.write;
    switch (expr.expr) {
        case 'int': print(``);
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

function interpretExpr(_env: Env, expr: Expr): Expr {
    switch (expr.expr) {
        case 'identifier': {
            if (isSpecial(expr.id)) return expr;
            throw todo(`interpretExpr id ${expr.id}`);
        }
        default: throw todo(`interpretExpr ${expr.expr}`);
    }
}

function todo(what: string): CaraError {
    return rawErr('EYYYY',`TODO ${what}`,{row:0,col:0});
}

function rawErr(code: string, message: string, loc: Loc): CaraError {
    return { stage: 'interpreter', code, message, loc };
}
