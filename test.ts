#!/usr/bin/env deno run --allow-read --allow-sys

import fs from 'node:fs/promises';
import {lex} from './src/lexer.ts';
import {isCaraError, Loc} from './src/error.ts';

const testsDir = 'end-to-end-tests';

const dirs =
  [...await fs.readdir(testsDir, {withFileTypes: true})]
    .filter(dirent => dirent.isDirectory())
    .map(dirent => dirent.name);

const loc = (l:Loc): number[] => [l.row,l.col];

const test = async (test:string) => {
  if (test.startsWith('test-')) return null; // TODO handle test tests
  const source = await fs.readFile(`${testsDir}/${test}/main.cara`, 'utf-8');
  try {
    //console.log(`---------- ${test}`);
    const tokens = lex(source);
  }
  catch (e) {
    if (isCaraError(e)) {
      if (test.includes('-err')) {
        const errFileContent: string = await fs.readFile(`${testsDir}/${test}/stderr.txt`, 'utf-8');
        const expectedError = errFileContent.substring(0,5);
        if (expectedError == e.code) return null;
      }
      return {test, loc:loc(e.loc), stage:e.stage, msg:e.message};
    }
  }
};

const fails = await Promise.all(dirs.map(dir => test(dir)));
console.table(fails.filter(x=>x));
