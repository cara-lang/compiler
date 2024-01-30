#!/usr/bin/env -S deno run --allow-read --allow-write

import fs from 'node:fs/promises';
import {Elm} from '../dist/elm.js';
import {chdir} from 'node:process';
import registerPorts from './registerPorts.ts';

const selectedTest = null;

///////////////////////////////////////////

let stdlibFiles = [];
for await (const dirEntry of Deno.readDir('stdlib')) {
  if (dirEntry.isFile) {
    stdlibFiles.push(`stdlib/${dirEntry.name}`);
  }
}

const stdlibSources = await Promise.all(
  stdlibFiles.map(file =>
    Deno.readTextFile(file)
        .then(content => ({file,content}))
  )
);

const testsDir = 'end-to-end-tests';

if (selectedTest == null) {

  // run all tests

  chdir(testsDir);
  const rootPath = Deno.cwd();
  const dirs =
    [...await fs.readdir('.', {withFileTypes: true})]
      .filter(dirent => dirent.isDirectory())
      .map(dirent => dirent.name)
      .sort();

  const app = Elm.TestRunner.init({flags: {dirs, rootPath, stdlibSources}});
  registerPorts(app);

} else {

  // run just the one test
  
  const testCwd = `./${testsDir}/${selectedTest}`;
  chdir(testCwd);

  const sourceCode: string = await Deno.readTextFile('main.cara');

  const app = Elm.Main.init({flags: {sourceCode, stdlibSources}});
  registerPorts(app);

}
