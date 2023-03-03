#!/usr/bin/env deno run --allow-read

import {inspect} from 'node:util';
import {argv} from 'node:process';
import {lex} from './src/lexer.ts';
import {parse} from './src/parser.ts';
import {interpret} from './src/interpreter.ts';

// deno-lint-ignore no-explicit-any
const log = (data: any) => { console.log(inspect(data, {depth:null,maxArrayLength:null,colors:true})); }

console.log('Source file:');
const file = argv[2];
console.log(file);

console.log('Source program:');
const source = await Deno.readTextFile(file);
log(source);

console.log('Tokens:');
const tokens = lex(source);
log(tokens);

console.log('AST:');
const ast = parse(tokens);
log(ast);

console.log('Interpreting:');
interpret(ast);
