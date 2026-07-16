import { randomUUID } from "node:crypto";

export function randomId() {
  return randomUUID();
}
