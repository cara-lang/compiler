#!/usr/bin/env node

import fs from 'node:fs/promises';
import {parse} from './cara-parser.js';

const testsDir = 'end-to-end-tests';

const dirs =
  [...await fs.readdir(testsDir, {withFileTypes: true})]
    .filter(dirent => dirent.isDirectory())
    .map(dirent => dirent.name);

const loc = (l) => `${l.start.line}:${l.start.column}-${l.end.line}:${l.end.column}`;

const test = async (test) => {
  try {
    const source = await fs.readFile(`${testsDir}/${test}/main.cara`, 'utf-8');
    parse(source);
  }
  catch (e) { return {test, loc: loc(e.location), found: e.found}; }
};

const fails = await Promise.all(dirs.map(dir => test(dir)));
console.table(fails.filter(x=>x));
