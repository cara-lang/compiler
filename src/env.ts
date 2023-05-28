import {Expr} from './ast.ts';

export type Env = Map<string,Expr>;

export type Module = {
    modules: Map<string,Module>,
    public: Env,
    private: Env,
};

export type Envs = {
    rootModule: Module,
    current: string[],
};