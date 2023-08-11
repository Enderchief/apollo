import { Error, Ok } from "./gleam.mjs";

export function validate(x, y) {
  return x(y);
}

/**
 * @param {(...args: any[]) => unknown} constructor
 * @param {import('./gleam.d.ts').List} list
 */
export function typed(constructor, list) {
  /** @type {Record<string, import("./apollo_t.mjs").Value>} */
  const o = Object.fromEntries(list);

  return (/** @type {Array<[string, any]>} */ args) => {
    for (const [k, v] of args) {
      if (!o[k]) return new Error(`Invalid property name '${k}'`);
      let res = o[k].validate(v);
      if (!res.isOk()) return res;
    }
    return new Ok(constructor(...args.map(([_, x]) => x)));
  };
}
