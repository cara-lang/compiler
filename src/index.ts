#!/usr/bin/env deno run --allow-read --allow-write

import fs from 'node:fs/promises';
import {Elm} from '../dist/elm.js';
import {chdir} from 'node:process';

const selectedTest = 'int-arithmetic-right-associativity'; // 'module-nested-root';

///////////////////////////////////////////

const textEncoder = new TextEncoder();
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

  const app = Elm.TestRunner.init({flags: {dirs, rootPath}});
  registerPorts(app);

} else {

  // run just the one test

  const testCwd = `./${testsDir}/${selectedTest}`;
  chdir(testCwd);

  const sourceCode: string = await Deno.readTextFile('main.cara');

  const app = Elm.Main.init({flags: {sourceCode}});
  registerPorts(app);

}

function registerPorts(app) {
  app.ports.chdir.subscribe(async (path: string) => {
      chdir(path);
      app.ports.completedChdir.send(null);
  });
  app.ports.print.subscribe(async (str: string) => {
      await Deno.stdout.write(textEncoder.encode(str));
      app.ports.completedPrint.send(null);
  });
  app.ports.println.subscribe(async (str: string) => {
      await Deno.stdout.write(textEncoder.encode(str + "\n"));
      app.ports.completedPrintln.send(null);
  });
  app.ports.eprintln.subscribe(async (str: string) => {
      await Deno.stderr.write(textEncoder.encode(str + "\n"));
      app.ports.completedEprintln.send(null);
  });
  app.ports.readFile.subscribe(async (r: {filename: string}) => {
      const content = await Deno.readTextFile(r.filename);
      app.ports.completedReadFile.send(content);
  });
  app.ports.writeFile.subscribe(async (r: {filename: string, content: string}) => {
      await Deno.writeTextFile(r.filename, r.content);
      app.ports.completedWriteFile.send(null);
  });
}
