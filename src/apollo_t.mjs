import { BoolT, FloatT, IntT, StringT } from "./apollo/util.mjs";
import { Error, Ok } from "./gleam.mjs";

/** @type {Record<string, (v: unknown) => boolean>} */
const _validator_map = {
  Int: Number.isInteger,
  String: (v) => typeof v === "string",
  Float: (v) => typeof v === "number" && !Number.isInteger(v),
  Bool: (v) => typeof v === "boolean",
};

export class Value {
  /**
   * @param {"Int" | "String" | "Float" | "Bool"} type
   */
  constructor(type) {
    this.type = type;
    /** @type {Array<(v: any) => import('./gleam').Result<undefined, string>>} */
    this.checks = [];
  }

  /**
   * @param {any} obj
   * @returns {import('./gleam').Result<undefined, string>}
   */
  validate(obj) {
    // @ts-ignore
    if (!_validator_map[this.type](obj)) {
      return new Error(`Type of value \`${obj}\` is not \`${this.type}\``);
    }
    let res;
    for (const check of this.checks) {
      res = check(obj);
      if (!res.isOk()) return res;
    }
    // @ts-ignore
    return new Ok();
  }
}

export function new_value(type) {
  return new Value(type);
}

/**
 * @param {Value} v
 * @param {(v: any) => import('./gleam').Result<undefined, string>} validator
 */
export function custom(v, validator) {
  v.checks.push((o) => {
    let value;
    if (v.type === "Bool") value = new BoolT(o);
    else if (v.type === "Float") value = new FloatT(o);
    else if (v.type === "Int") value = new IntT(o);
    else if (v.type === "String") value = new StringT(o);
    return validator(value);
  });

  return v;
}
