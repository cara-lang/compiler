import {Token} from './token.ts';
import {Decl} from './ast.ts';
import {CaraError} from './error.ts';
import {Loc} from './loc.ts';

type State = { tokens: Token[], i: number };

function err(code: string, message: string, loc: Loc): CaraError {
    return { stage: 'parser', code, message, loc };
}

function isAtEnd(state: State): boolean {
    return state.tokens[state.i].type.type == 'EOF';
}

export function parse(tokens: Token[]): Decl[] {
    let state = {tokens, i: 0};
    const decls: Decl[] = [];
    while (!isAtEnd(state)) {
        throw err('EXXXX', 'TODO parse()', {row:0,col:0});
    }
    return decls;
}
