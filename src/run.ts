#!/usr/bin/env -S deno run --allow-read --allow-write

import registerPorts from './registerPorts.ts';
import {Elm} from '../dist/elm.js';

const usage = "Usage: ok run some/path/foo.cara (keeps cwd where it is)"

if (Deno.args.length !== 1) {
  console.error(usage);
  Deno.exit(1);
}

const filename = Deno.args[0];

if (filename.length === 0) {
  console.error(usage);
  Deno.exit(1);
}

const sourceCode: string = await Deno.readTextFile(filename);

const app = Elm.Main.init({flags: {sourceCode}});
registerPorts(app);
