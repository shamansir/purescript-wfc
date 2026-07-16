import { readFileSync } from "node:fs";
import { PNG } from "pngjs";

export function decodePngFile(path) {
  return function () {
    const buffer = readFileSync(path);
    const png = PNG.sync.read(buffer);
    return { width: png.width, height: png.height, bytes: Array.from(png.data) };
  };
}
