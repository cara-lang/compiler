import {eprintln} from './util.ts';

export function err(message: string): never {
    eprintln(message);
    Deno.exit(1);
}