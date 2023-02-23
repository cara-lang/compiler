#!/usr/bin/env deno run --allow-read

import {chdir} from 'node:process';
import {lex} from './src/lexer.ts';
import {parse} from './src/parser.ts';
import {interpret} from './src/interpreter.ts';

const test = 'tuple-10-el11-err';

const testCwd = `./end-to-end-tests/${test}`;
chdir(testCwd);

const source = await Deno.readTextFile(`./main.cara`);
interpret(parse(lex(source)));