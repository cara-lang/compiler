#!/usr/bin/env deno run --allow-read

import process from 'node:process';
import {lex} from './src/lexer.ts';
import {parse} from './src/parser.ts';
import {interpret} from './src/interpreter.ts';

const file = process.argv[2];
const source = await Deno.readTextFile(file);
interpret(parse(lex(source)));
