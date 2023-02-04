import {Loc} from './loc.ts';

export type Stage =
    | 'lexer'
    | 'parser'

export type CaraError = {
    stage: Stage,
    code: string,
    message: string,
    loc: Loc,
};

export function isCaraError(obj: any): obj is CaraError {
    return obj.stage !== undefined && obj.message !== undefined && obj.loc !== undefined;
}
