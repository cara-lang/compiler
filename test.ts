#!/usr/bin/env deno run --allow-read

import fs from 'node:fs/promises';
import {lex} from './src/lexer.ts';
import {isCaraError, Loc} from './src/error.ts';

const testsDir = 'end-to-end-tests';

const dirs =
  [...await fs.readdir(testsDir, {withFileTypes: true})]
    .filter(dirent => dirent.isDirectory())
    .map(dirent => dirent.name);

const loc = (l:Loc): string => [l.row,l.col];

const test = async (test:string) => {
  const source = await fs.readFile(`${testsDir}/${test}/main.cara`, 'utf-8');
  try { const tokens = lex(source); }
  catch (e) { if (isCaraError(e)) return {test, loc:loc(e.loc), stage:e.stage, msg:e.message}; }
};

const fails = await Promise.all(dirs.map(dir => test(dir)));
console.table(fails.filter(x=>x));
