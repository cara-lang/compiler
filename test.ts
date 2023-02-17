#!/usr/bin/env deno run --allow-read --allow-run
import fs from 'node:fs/promises';

const selectedTest = null;

const textDecoder = new TextDecoder();
const testsDir = 'end-to-end-tests';
const dirs =
  [...await fs.readdir(testsDir, {withFileTypes: true})]
    .filter(dirent => dirent.isDirectory())
    .map(dirent => dirent.name);

type TestResult =
  | {status:'skip',test:string}
  | {status:'pass',test:string}
  | {status:'fail-different-out',test:string,actual:string}
  | {status:'fail-different-err',test:string/*,actual:string*/}
  | {status:'fail-unexpected-err',test:string,actual:string}
  | {status:'fail-unexpected-ok', test:string,actual:string}


const test = async (test:string): Promise<TestResult> => {
  if (selectedTest != null && test != selectedTest) return {status:'skip',test};
  if (test.startsWith('test-')) return {status:'skip',test}; // TODO handle test tests

  const shouldErr = test.match(/-err/);

  const p = Deno.run({
    cmd: ['../../run.ts', 'main.cara'],
    cwd: `./${testsDir}/${test}`,
    stdout: "piped",
    stderr: "piped",
  })

  const { code } = await p.status();
  const actualOutput = textDecoder.decode(await p.output());
  const actualError  = textDecoder.decode(await p.stderrOutput());

  let expectedOutput = "";
  let expectedError  = "";
  try { expectedOutput = await Deno.readTextFile(`./${testsDir}/${test}/stdout.txt`); } catch (_) {/**/}
  try { expectedError  = await Deno.readTextFile(`./${testsDir}/${test}/stderr.txt`); } catch (_) {/**/}

  if (selectedTest != null) {
    console.log('STDOUT');
    console.log('------');
    console.log(actualOutput);
    console.log('');
    console.log('STDERR');
    console.log('------');
    console.log(actualError);
  }

  if (code != 0 && !shouldErr) return {status:'fail-unexpected-err',test,actual:actualError};
  if (code == 0 && shouldErr)  return {status:'fail-unexpected-ok', test,actual:actualOutput};
  
  if (actualOutput !== expectedOutput) return {status:'fail-different-out',test,actual:actualOutput};
  if (actualError  !== expectedError)  return {status:'fail-different-err',test/*,actual:actualError*/};

  return {status:'pass',test};
};

// deno-lint-ignore no-unused-vars
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
//const results: TestResult[] = await Promise.all(dirs.map(test));
const skipped = sort(results.filter((x) => x.status == 'skip'));
const passed  = sort(results.filter((x) => x.status == 'pass'));
const failed  = sort(results.filter((x) => x.status.startsWith('fail')));
console.table({
  skip: skipped.length,
  pass: passed.length,
  fail: failed.length,
});
console.table(failed);
