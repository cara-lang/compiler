#!/usr/bin/env deno run --allow-read --allow-write

// Usage: ok run some/path/foo.cara (keeps cwd where it is)

import registerPorts from './registerPorts.ts';

const filename = Deno.args[0];
const sourceCode: string = await Deno.readTextFile(filename);

const app = Elm.Main.init({flags: {sourceCode}});
registerPorts(app);
