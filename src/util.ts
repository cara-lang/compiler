import {inspect} from 'node:util';

const textEncoder = new TextEncoder();

export async function print(x: string)    { await Deno.stdout.write(textEncoder.encode(x)); }
export async function eprint(x: string)   { await Deno.stderr.write(textEncoder.encode(x)); }
export async function println(x: string)  { await print(`${x}\n`); }
export async function eprintln(x: string) { await eprint(`${x}\n`); }

export function arrayEquals<T>(a: T[], b: T[]): boolean {
    return Array.isArray(a) &&
        Array.isArray(b) &&
        a.length === b.length &&
        a.every((val, index) => val === b[index]);
}

export function hasDuplicates<T>(array: T[]): boolean {
    return (new Set(array)).size !== array.length;
}

export function stringify(a: unknown): string {
    return inspect(a, {depth:null, colors:false, breakLength: 200});
}

export function log(a: unknown) {
    console.log(inspect(a, {depth:null, colors:true, breakLength: 200}));
}

export function addToMap<K,V>(map: Map<K,V>, key: K, val: V): Map<K,V> {
    return new Map([...map.entries(),[key,val]]);
}