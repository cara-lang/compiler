#!/usr/bin/env deno run --allow-read --allow-write
import fs from 'node:fs/promises';
import {chdir} from 'node:process';
import {lex} from './src/lexer.ts';
import {parse} from './src/parser.ts';
import {interpret} from './src/interpreter.ts';
import {log} from './src/util.ts';
import {Token} from './src/token.ts';

const selectedTest = 'module-nested-root';

const originalCwd = Deno.cwd();
const testsDir = 'end-to-end-tests';
const dirs =
  [...await fs.readdir(testsDir, {withFileTypes: true})]
    .filter(dirent => dirent.isDirectory())
    .map(dirent => dirent.name);

type TestResult =
  | {status:'skip',test:string}
  | {status:'pass',test:string}
  | {status:'fail-bug',test:string}
  | {status:'fail-different-out',test:string,actual:string}
  | {status:'fail-different-err',test:string}
  | {status:'fail-unexpected-err',test:string,actual:string}
  | {status:'fail-unexpected-ok', test:string,actual:string}


const test = async (test:string): Promise<TestResult> => {
  if (selectedTest != null && test != selectedTest) return {status:'skip',test};
  if (test.startsWith('test-')) return {status:'skip',test}; // TODO handle test tests
  const verbose = selectedTest != null;

  const shouldErr = test.match(/-err/);

  chdir(originalCwd);
  const testCwd = `./${testsDir}/${test}`;
  chdir(testCwd);

  let expectedOutput = '';
  let expectedError  = '';
  try { expectedOutput = await Deno.readTextFile(`stdout.txt`); } catch (_) {/**/}
  try { expectedError  = await Deno.readTextFile(`stderr.txt`); } catch (_) {/**/}

  if (verbose) console.log('SOURCE:');
  const source = await Deno.readTextFile('main.cara');
  if (verbose) console.log(source);

  let tokens: Token[] = [];
  try {
    if (verbose) console.log('TOKENS:');
    tokens = lex(source);
    if (verbose) log(tokens);
  } catch (e) {
    if (!shouldErr) return {status:'fail-unexpected-err',test,actual:e};
    try {
      const wantedErrCode = expectedError.match(/^E\d{4}: /)![0];
      if (!e.match(new RegExp(`^${wantedErrCode}`))) {
        return {status:'fail-different-err',test};
      } else {
        return {status:'pass',test};
      }
      // TODO strict err compliance mode: if (actualError !== expectedError)  return {status:'fail-different-err',test};
    } catch (_) {/**/}
  }

  let ast;
  try {
    if (verbose) console.log('AST:');
    ast = parse(tokens);
    if (verbose) log(ast);
  } catch (e) {
    if (!shouldErr) return {status:'fail-unexpected-err',test,actual:e};
    try {
      const wantedErrCode = expectedError.match(/^E\d{4}: /)![0];
      if (!e.match(new RegExp(`^${wantedErrCode}`))) {
        return {status:'fail-different-err',test};
      } else {
        return {status:'pass',test};
      }
      // TODO strict err compliance mode: if (actualError !== expectedError)  return {status:'fail-different-err',test};
    } catch (_) {/**/}
  }

  if (ast == null) return {status:'fail-bug',test};

  if (verbose) console.log('INTERPRETING:');

  const oldStdoutWrite = Deno.stdout.write;
  const oldStderrWrite = Deno.stderr.write;
  Deno.stdout.write = function(_): Promise<number> {return new Promise((resolve,_) => resolve(0));};
  Deno.stderr.write = function(_): Promise<number> {return new Promise((resolve,_) => resolve(0));};

  const result = await interpret(ast);

  Deno.stdout.write = oldStdoutWrite;
  Deno.stderr.write = oldStderrWrite;

  const actualOutput = result.stdout;
  const actualError  = result.stderr;

  //console.log({actualOutput,expectedOutput});
  //console.log({actualError, expectedError });

  if (verbose) {
    console.log('ACTUAL STDOUT');
    console.log('------');
    console.log(actualOutput);
    console.log('');
    console.log('EXPECTED STDOUT');
    console.log('------');
    console.log(expectedOutput);
    console.log('');
    console.log('ACTUAL STDERR');
    console.log('------');
    console.log(actualError);
    console.log('');
    console.log('EXPECTED STDERR');
    console.log('------');
    console.log(expectedError);
  }

  if (result.code != 0 && !shouldErr) return {status:'fail-unexpected-err',test,actual:actualError};
  if (result.code == 0 && shouldErr)  return {status:'fail-unexpected-ok', test,actual:actualOutput};
  
  if (actualOutput !== expectedOutput) return {status:'fail-different-out',test,actual:actualOutput};
  try {
    const wantedErrCode = expectedError.match(/^E\d{4}: /)![0];
    if (!actualError.match(new RegExp(`^${wantedErrCode}`))) return {status:'fail-different-err',test};
    // TODO strict err compliance mode: if (actualError !== expectedError)  return {status:'fail-different-err',test};
  } catch (_) {/**/}

  return {status:'pass',test};
};

async function allSynchronously<T>(resolvables: (() => Promise<T>)[]): Promise<T[]> {
  const results = [];
  for (const resolvable of resolvables) {
    results.push(await resolvable());
  }
  return results;
}

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
const failed  = sort(results.filter((x) => x.status.startsWith('fail')));
console.table({
  skip: skipped.length,
  pass: passed.length,
  fail: failed.length,
});
console.table(failed);
