#!/usr/bin/env deno run --allow-read

import {inspect} from 'node:util';
import {argv} from 'node:process';
import {lex} from './src/lexer.ts';
import {parse} from './src/parser.ts';

const log = (data: any) => { console.log(inspect(data, {depth:null,maxArrayLength:null,colors:true})); }

const program = argv[2];
console.log('Source program:');
log(program);
const tokens = lex(program);
console.log('Tokens:');
log(tokens);
const ast = parse(tokens);
console.log('AST:');
log(ast);