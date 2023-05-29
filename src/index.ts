#!/usr/bin/env deno run --allow-read --allow-write

import {Elm} from '../dist/elm.js';
import {chdir} from 'node:process';

const selectedTest = 'module-nested-root';

///////////////////////////////////////////

const textEncoder = new TextEncoder();

const testsDir = 'end-to-end-tests';
const testCwd = `./${testsDir}/${selectedTest}`;
chdir(testCwd);

const sourceCode: string = await Deno.readTextFile('main.cara');

const app = Elm.Main.init({flags: {sourceCode}});

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
