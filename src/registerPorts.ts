import {chdir} from 'node:process';

const textEncoder = new TextEncoder();

export default function(app) {
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
  app.ports.readFileMaybe.subscribe(async (r: {filename: string}) => {
      try {
        const content = await Deno.readTextFile(r.filename);
        app.ports.completedReadFileMaybe.send(content);
      } catch (e) {
        app.ports.completedReadFileMaybe.send(null);
      }
  });
  app.ports.writeFile.subscribe(async (r: {filename: string, content: string}) => {
      await Deno.writeTextFile(r.filename, r.content);
      app.ports.completedWriteFile.send(null);
  });
}
