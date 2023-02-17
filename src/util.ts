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
