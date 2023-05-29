import {Decl, Stmt, Bang, Typevar, Constructor, Expr, Identifier, LetModifier, Type, Pattern, BinaryOp, RecordExprContent, TypeModifier, ModuleModifier} from './ast.ts';
import {Env,Envs,Module} from './env.ts';
import {stringify,arrayEquals,print,eprintln,addToMap} from './util.ts';
<<<<<<< Updated upstream
=======
import {arrayZipper, Location} from 'npm:@thi.ng/zipper';
>>>>>>> Stashed changes

type State = {
    envs: Envs,
    stdout: string,
    stderr: string,
};

let typeConstructorI = 0;

<<<<<<< Updated upstream
function getModule(envs: Envs, stack: string[]): Module | null {
    let stackTodo = stack;
    let currentModule = envs.rootModule;
    while (stackTodo.length > 0) {
        const moduleName = stackTodo[0];
        const next = currentModule.modules.get(moduleName);
        if (next == undefined) return null;
        stackTodo = stack.slice(1);
        currentModule = next;
=======
function getModule(envs: Envs, stack: string[]): Location<Module> | null {
    let stackTodo = stack;
    let currentModule: Location<Module> | undefined = envs;
    while (stackTodo.length > 0 && currentModule != null) {
        const moduleName = stackTodo[0];
        currentModule = currentModule.down;
        while (currentModule != null && currentModule.node.name != moduleName) {
            currentModule = currentModule.right;
        }
        if (currentModule == null) return null;
        stackTodo = stack.slice(1);
>>>>>>> Stashed changes
    }
    return currentModule;
}

<<<<<<< Updated upstream
function mapModule(envs: Envs, stack: string[], fn: (m: Module) => Module): Envs {
    /*
mapModule : List String -> (Module -> Module) -> Envs -> Maybe Envs
mapModule path fn envs =
    mapModuleInner path fn envs.rootModule
        |> Maybe.map (\m -> { envs | rootModule = m })

mapModuleInner : List String -> (Module -> Module) -> Module -> Maybe Module
mapModuleInner path fn mod =
    case path of
        [] -> 
            fn mod

        fst :: rest -> 
            Dict.get fst mod.modules
            |> Maybe.map (\old ->
                let
                    new = mapModuleInner rest fn old
                in
                { mod | modules = Dict.insert fst new mod.modules }
            )
    */
=======
function upmost<T>(z: Location<T>): Location<T> {
    while (z.up != null) {
        z = z.up;
    }
    return z;
}

function mapModule(envs: Envs, stack: string[], fn: (m: Module) => Module): Envs {
    const focused = getModule(envs, stack);
    if (focused == null) return envs;
    return upmost(focused.update(fn));
>>>>>>> Stashed changes
}

// `a` with items from `b` added. On collision, `b` wins.
function envAdd(a: Env, b: Env): Env {
    return new Map([
        ...a,
        ...b,
    ]);
}

<<<<<<< Updated upstream
function rootEnvs(envs: Envs): Envs {
    return {
        ...envs,
        current: [],
    };
}

function addToPublic(envs: Envs, addition: Env): Envs {
    let module = envs.rootModule;
    let modulesToDo = envs.current;
    while (modulesToDo.length > 0) {
        const nextQualifier = modulesToDo[0];
        const nextModule = module.modules.get(nextQualifier);
        if (nextModule == undefined) {
            throw `Module ${modulesToDo.join('.')} not found.`;
        }
        module = nextModule;
        modulesToDo = modulesToDo.slice(1);
    }
    return module;
=======
function addToPublic(envs: Envs, addition: Env): Envs {
    return envs.update(m => ({...m, public: envAdd(m.public, addition)}));
>>>>>>> Stashed changes
}

function envGet(id: Identifier, envs: Envs): Expr | undefined {
    // If there are qualifiers in the ID, we need to drill down to the correct module.
    // Example: envs.current = ['Foo'], id = Bar.baz
    // First we follow the `current` (foo = envs.modules['Foo'])
    // Then we follow the qualifiers in the ID (bar = foo.modules['Bar'])
    // Then we try to find the `baz` in that module's public env (baz = bar.public['baz'])
    if (id.qualifiers.length > 0) {
        let module = envs.rootModule;
        const origQualifiersToDo = envs.current.concat(...id.qualifiers);
        let qualifiersToDo = origQualifiersToDo;
        while (qualifiersToDo.length > 0) {
            const nextQualifier = qualifiersToDo[0];
            const nextModule = module.modules.get(nextQualifier);
            if (nextModule == undefined) {
                throw `Module ${origQualifiersToDo.join('.')} not found.`;
            }
            module = nextModule;
            qualifiersToDo = qualifiersToDo.slice(1);
        }
        return module.public.get(id.name); // ignoring the qualifiers deliberately: we've already walked into those modules
    }

    // If there are no qualifiers in the ID, we have a few cases to solve - see below.
    const str = idToString(id);
    // Simplest case: self public
    if (envs.selfPublic.has(str)) return envs.selfPublic.get(str);
    // Next: self private
    if (envs.selfPrivate.has(str)) return envs.selfPrivate.get(str);
    // If none of the above: try the parent public, the grandparent public, ... until you get to the root public
    let stackToTry = envs.current.slice(0,-1);
    while (true) {
        const module: Module | null = getModule(envs, stackToTry);
        if (module == null) break;
        if (module.public.has(str)) return module.public.get(str);
        if (stackToTry.length == 0) break;
        stackToTry = stackToTry.slice(0,-1);
    }
    // Otherwise it's simply not there!
    return undefined;
}

function idToString(id: Identifier): string {
    return id.qualifiers.length == 0
        ? id.name
        : id.qualifiers.concat(id.name).join('.');
}

function addNamespace(env: Env, name: string): Env {
    return new Map([...env.entries()].map(([k,v]) => [`${name}.${k}`,v]));
}

export async function interpret(ast: Decl[]): Promise<{code: number, stdout: string, stderr: string}> {
    const envs: Envs = {
        rootModule: {
            modules: new Map(),
            public: new Map(),
            private: new Map(),
        },
        current: [],
    };
    let state: State = {
        envs,
        stdout: '', 
        stderr: '',
    };
    for (const decl of ast) {
        try {
            state = await interpretDecl(state, decl);
        } catch (e) {
            // TODO when we throw strings, we lose the stdout from the current decl. Throw something that has both the stdout and the error message?
            await eprintln(e);
            return {code: 1, stdout: state.stdout, stderr: state.stderr + e};
        }
    }
    return {code: 0, stdout: state.stdout, stderr: state.stderr};
}

async function interpretDecl(state: State, decl: Decl): Promise<State> {
    switch (decl.decl) {
        case 'statement': return await interpretStmt(state, decl.stmt);
        case 'module': return await interpretModule(state, decl.mod, decl.name, decl.decls);
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
    let newSelfPublicEnv = state.envs.selfPublic;
    let newSelfPrivateEnv = state.envs.selfPrivate;
    // Add the type's constructors to the env
    switch (mod) {
        case 'NoModifier': {
            newSelfPublicEnv = constructors.reduce(interpretTypeConstructor,newSelfPublicEnv);
            break;
        }
        case 'Private': {
            newSelfPrivateEnv = constructors.reduce(interpretTypeConstructor,newSelfPrivateEnv);
            break;
        }
        case 'Opaque': break;
    }
    const newEnvs = {
        ...state.envs, 
        selfPublic: newSelfPublicEnv, 
        selfPrivate: newSelfPrivateEnv,
    };
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

function saveSelf(envs: Envs): Module {
    if (envs.current.length == 0) {
        const finalPublic: Env = envAdd(envs.rootModule.public, envs.selfPublic);
        const finalPrivate: Env = envAdd(envs.rootModule.public, envs.selfPrivate);
        // TODO is this correct? public instead of private? ^

        envs.rootModule.public = finalPublic;
        envs.rootModule.private = finalPrivate;
        return envs.rootModule;
    } else {
        // drill into the first parent
        const nextModuleName = envs.current[0];
        const newCurrent = envs.current.slice(1);
        let nextModule = envs.rootModule.modules.get(nextModuleName);
        if (!nextModule) { 
            if (newCurrent.length == 0) {
                nextModule = {
                    modules: new Map(),
                    private: envs.selfPrivate,
                    public: envs.selfPublic,
                };
            } else {
                throw `saveSelf: module ${nextModuleName} not found in the Envs module tree`; 
            }
        }
        const nextEnvs: Envs = {
            ...envs,
            current: newCurrent,
            rootModule: nextModule,
        };
        const finalModule: Module = saveSelf(nextEnvs);
        envs.rootModule.modules.set(nextModuleName,finalModule);
        return envs.rootModule;
    }
}

async function interpretModule(state: State, mod: ModuleModifier, name: string, decls: Decl[]): Promise<State> {
    let newState = {
        ...state,
        envs: {
            rootModule: saveSelf(state.envs),
            current: state.envs.current.concat(name),
            selfPublic: new Map(),
            selfPrivate: new Map(),
        },
    };
    for (const decl of decls) {
        newState = await interpretDecl(newState, decl);
    }
    switch (mod) {
        case 'NoModifier': {
            return {
                ...newState, 
                envs: {
                    ...newState.envs,
                    selfPublic: envAdd(state.envs.selfPublic, addNamespace(newState.envs.selfPublic, name)),
                    selfPrivate: state.envs.selfPrivate,
                }
            };
        }
        case 'Private': {
            return {
                ...newState, 
                envs: {
                    ...newState.envs,
                    selfPublic: state.envs.selfPublic,
                    selfPrivate: envAdd(state.envs.selfPrivate, addNamespace(newState.envs.selfPrivate, name)),
                }
            };
        }
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
    switch (bang.bang) {
        case 'value': {
            const expr = interpretExpr(state.envs,bang.val);
            return {state,expr};
        }
        case 'call': {
            const fn = interpretExpr(state.envs,bang.fn);
            if (fn.expr == 'identifier') {
                if (idEquals(fn.id,ioPrintlnId)) {
                    if (bang.args.length == 1) {
                        const arg = bang.args[0];
                        const newStdout = await ioPrintln(state.envs,arg);
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
    switch (mod) {
        case 'NoModifier': {
            const selfEnvAdditions = interpretPattern(lhs, interpretExpr(envs,body));
            if (selfEnvAdditions == null) {
                throw `Pattern didn't match the expr: ${showPattern(lhs)} = ${show(body)}`;
            }
            return {...envs, selfPublic: envAdd(envs.selfPublic, selfEnvAdditions)};
        }
        case 'Private': {
            // TODO do something else than NoModifier does?
            throw `interpretLet 1 ${mod} ${type} ${stringify(lhs)} ${stringify(body)}`;
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
async function ioPrintln(envs: Envs, expr: Expr): Promise<string> {
    const e = interpretExpr(envs,expr);
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

function interpretExpr(envs: Envs, expr: Expr): Expr {
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
            return {...expr, args: expr.args.map((e) => interpretExpr(envs,e))};
        case 'root-identifier': 
            return interpretExpr(rootEnvs(envs), {...expr, expr: 'identifier'});
        case 'identifier': {
            if (isSpecial(expr.id)) return expr;
            const item = envGet(expr.id, envs);
            if (item == null) {
                throw `Unknown identifier: ${show(expr)}. Public env: ${showEnv(envs.selfPublic)}, private env: ${showEnv(envs.selfPrivate)}`;
            }
            return item;
        }
        case 'tuple': {
            return {...expr, elements: expr.elements.map((e) => interpretExpr(envs,e))};
        }
        case 'list': {
            return {...expr, elements: expr.elements.map((e) => interpretExpr(envs,e))};
        }
        case 'record': {
            const newContents = expr.contents
                .flatMap((c) => interpretRecordExprContent(envs,c))
                .reduce(
                    function (acc: Map<string, Expr>, content: { field: string, value: Expr }): Map<string, Expr> {
                        return addToMap(acc, content.field, content.value);
                    },
                    new Map(),
                );
            return {
                ...expr,
                contents: [...newContents.entries()].map(([k,v]) => ({
                    recordContent: 'field',
                    field: k,
                    value: v,
                })),
            };
        }
        case 'lambda': {
            return {...expr, expr:'closure', envs};
        }
        case 'unary-op': {
            const arg = interpretExpr(envs,expr.arg);
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
            const left = interpretExpr(envs,expr.left);
            const right = interpretExpr(envs,expr.right);
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
            const cond = interpretExpr(envs,expr.cond);
            if (cond.expr != 'bool') throw 'E0025: If expression with a non-bool condition';
            if (cond.bool) return interpretExpr(envs,expr.then);
            else           return interpretExpr(envs,expr.else);
        }
        case 'case': {
            const subject = interpretExpr(envs,expr.subject);
            for (const branch of expr.branches) {
                const envAddition: Env|null = matchAnyPattern(subject, branch.orPatterns);
                if (envAddition != null) {
                    // TODO is it correct to use the selfPublic here?
                    const newEnv: Env = envAdd(envs.selfPublic, envAddition);
                    const newEnvs: Envs = {...envs, selfPublic: newEnv};
                    return interpretExpr(newEnvs,branch.body);
                }
            }
            throw `interpretExpr case - nothing matched! ${show(expr.subject)} ${stringify(expr.branches)}`;
        }
        case 'record-get': {
            return recordGet(envs,expr.record,expr.field);
        }
        case 'call': {
            const fn = interpretExpr(envs,expr.fn);
            const args = expr.args.map((e) => interpretExpr(envs,e));
            switch (fn.expr) {
                case 'record-getter': {
                    if (args.length !== 1) {
                        throw `interpretExpr BUG: record getter called with more than onen argument`;
                    }
                    return recordGet(envs,args[0],fn.field);
                }
                case 'closure': {
                    const innerEnvs: Envs = applyClosureArgumentPatterns(
                        fn.envs, // lexical closure baby!
                        fn.args,
                        args
                    ); 
                    return interpretExpr(innerEnvs,fn.body);
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

function applyClosureArgumentPatterns(envs:Envs, argPatterns:Pattern[], argValues:Expr[]): Envs {
    if (argPatterns.length !== argValues.length) {
        throw `Wrong number of arguments provided: ${argPatterns.length} needed, ${argValues.length} provided`;
    }
    const envAdditions: Env[] = 
        argPatterns
            .flatMap((pattern,i) => {
                const envAddition = interpretPattern(pattern,argValues[i]);
                return envAddition == null ? [] : [envAddition];
            });
    return addToPublic(envs, envAdditions);
}

function interpretRecordExprContent(envs: Envs, content: RecordExprContent): {field: string, value: Expr}[] {
    switch (content.recordContent) {
        case 'field': return [{field: content.field, value: content.value}];
        case 'pun':   return [{
                                field: content.field,
                                value: interpretExpr(
                                    envs,
                                    {
                                        expr: 'identifier',
                                        id: {qualifiers: [], name: content.field}
                                    }
                                )
                            }];
        case 'spread': {
            const record = interpretExpr(envs, {expr: 'identifier', id: content.recordId});
            if (record.expr != 'record') throw `Tried to spread a non-record value when creating a record`;
            return record.contents.flatMap((c) => interpretRecordExprContent(envs, c));
        }
    }
}

function ensureRecordFieldsOnly(contents: RecordExprContent[]): {field:string, value:Expr}[] {
    return contents.map((content) => {
        if (content.recordContent == 'field') return {field: content.field, value: content.value};
        throw "BUG: Record didn't exclusively contain fields. Did we forget to interpretExpr it first?";
    });
}

function recordGet(envs: Envs, record: Expr, field: string): Expr {
    const rec = interpretExpr(envs,record);
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

function showEnv(env: Env): string {
    return `{${[...env.entries()].map(([k,v]) => `${k} => ${show(v)}`).join(', ')}}`;
}