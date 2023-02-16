import {Decl} from './ast.ts';
import {CaraError} from './error.ts';
import {Loc} from './loc.ts';

export function interpret(_ast: Decl[]) {
    throw todo('interpret');
}

function todo(what: string): CaraError {
    return rawErr('EYYYY',`TODO ${what}`,{row:0,col:0});
}

function rawErr(code: string, message: string, loc: Loc): CaraError {
    return { stage: 'interpreter', code, message, loc };
}
