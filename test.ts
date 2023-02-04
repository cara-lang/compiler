#!/usr/bin/env deno run --allow-read --allow-sys

import fs from 'node:fs/promises';
import {lex} from './src/lexer.ts';
import {parse} from './src/parser.ts';
import {isCaraError,Stage} from './src/error.ts';
import {Loc} from './src/loc.ts';
import {Token} from './src/token.ts';
import {Decl} from './src/ast.ts';

const testsDir = 'end-to-end-tests';

const dirs =
  [...await fs.readdir(testsDir, {withFileTypes: true})]
    .filter(dirent => dirent.isDirectory())
    .map(dirent => dirent.name);

const loc = (l:Loc): number[] => [l.row,l.col];

type TestResult =
  | {status:'skip',test:string}
  | {status:'pass',test:string}
  | {status:'fail',test:string,loc:number[],stage:Stage,msg:string}

async function allSynchronously<T>(resolvables: (() => Promise<T>)[]): Promise<T[]> {
  const results = [];
  for (const resolvable of resolvables) {
    results.push(await resolvable());
  }
  return results;
}


const test = async (test:string): Promise<TestResult> => {
  if (test.startsWith('test-')) return {status:'skip',test}; // TODO handle test tests
  const source = await fs.readFile(`${testsDir}/${test}/main.cara`, 'utf-8');
  try {
    const tokens: Token[] = lex(source);
    //const ast: Decl[] = parse(tokens);
    // console.log({ast});
  }
  catch (e) {
    if (isCaraError(e)) {
      if (test.includes('-err')) {
        const errFileContent: string = await fs.readFile(`${testsDir}/${test}/stderr.txt`, 'utf-8');
        const expectedError = errFileContent.substring(0,5);
        if (expectedError == e.code) {
          // TODO also check the row,col
          return {status:'pass',test};
        }
      }
      return {status:'fail',test,loc:loc(e.loc),stage:e.stage,msg:e.message};
    }
    throw e;
  }
  return {status:'pass',test};
};

function sort(results: TestResult[]): TestResult[] {
  return results.sort((a: TestResult, b: TestResult) => {
    if (a.test < b.test) { return -1; }
    if (a.test > b.test) { return 1; }
    return 0;
  })
}

const results: TestResult[] = await allSynchronously(dirs.map(dir => () => test(dir)));
const skipped = sort(results.filter((x) => x.status == 'skip'));
const passed  = sort(results.filter((x) => x.status == 'pass'));
const failed  = sort(results.filter((x) => x.status == 'fail'));
console.table({
  skip: skipped.length,
  pass: passed.length,
  fail: failed.length,
});
console.table(failed);