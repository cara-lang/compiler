#!/usr/bin/env deno run --allow-read

import {lex} from './src/lexer.ts';
import {parse} from './src/parser.ts';
import {interpret} from './src/interpreter.ts';

const test = 'int-separators-anywhere';

const source = await Deno.readTextFile(`./end-to-end-tests/${test}/main.cara`);
interpret(parse(lex(source)));
