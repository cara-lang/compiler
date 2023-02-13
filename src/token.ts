import {Loc} from './loc.ts';

export type Token = {type:TokenType, loc: Loc}

export type TokenType =
    | {type:'INT',int:number}
    | {type:'FLOAT',float:number}
    | {type:'CHAR',char:string}
    | {type:'STRING',string:string}
    | {type:'GETTER',field:string}
    | {type:'QUALIFIER',name:string}
    | {type:'LOWER_NAME',name:string}
    | {type:'UPPER_NAME',name:string}
    | {type:'HOLE',n:number}
    | {type:SimpleTokenType}

export type TokenTag = TokenType['type'];

export type SimpleTokenType =
    | 'PLUS'
    | 'MINUS'
    | 'TIMES'
    | 'DIV'
    | 'PERCENT'
    | 'POWER'
    | 'PLUSPLUS'
    | 'SHL'
    | 'SHR'
    | 'SHRU'
    | 'CARET'
    | 'ANDAND'
    | 'AND'
    | 'OROR'
    | 'LTE'
    | 'LT'
    | 'EQEQ'
    | 'NEQ'
    | 'GT'
    | 'GTE'
    | 'DOTDOT'
    | 'DOTDOTDOT'
    | 'TILDE'
    | 'CASE'
    | 'OF'
    | 'IF'
    | 'THEN'
    | 'ELSE'
    | 'TRUE'
    | 'FALSE'
    | 'TYPE'
    | 'ALIAS'
    | 'MODULE'
    | 'PRIVATE'
    | 'OPAQUE'
    | 'EXTEND'
    | 'BACKSLASH'
    | 'ARROW'
    | 'UNDERSCORE'
    | 'LHOLE'
    | 'LPAREN'
    | 'RPAREN'
    | 'LBRACE'
    | 'RBRACE'
    | 'LBRACKET'
    | 'RBRACKET'
    | 'PIPELINE'
    | 'COMMA'
    | 'COLON'
    | 'BANG'
    | 'EQ'
    | 'PIPE'
    | 'EOL'
    | 'EOF'
