#!/usr/bin/env deno run --allow-read

import {argv} from 'node:process';
import {lex} from './src/lexer.ts';
import {parse} from './src/parser.ts';
import {inspect} from 'node:util';

const program = argv[2];
console.log('Source program:');
console.log(inspect(program));
const tokens = lex(program);
console.log('Tokens:');
console.log(inspect(tokens));
const ast = parse(tokens);
console.log('AST:');
console.log(inspect(ast));