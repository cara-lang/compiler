export type Stage =
    | 'lexer'
    | 'parser'

export type Loc = { row: number, col: number };

export type CaraError = {
    stage: Stage,
    message: string,
    loc: Loc,
};

export function isCaraError(obj: any): obj is CaraError {
    return obj.stage !== undefined && obj.message !== undefined && obj.loc !== undefined;
}
