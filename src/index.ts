import { spawnSync } from "child_process";

class ErrorWrapper<T> extends Error {
  constructor(name: string, public value: T) {
    super();
    this.name = name;
    this.value = value;
    this.message = JSON.stringify(value);
  }
}

const numberLiteralExp = /^\-?\d+(\.\d+)?/;

const consumeNumberLiteral = (t: string): string =>
  t.match(numberLiteralExp)?.[0] ?? "";

if (import.meta.vitest) {
  const { it, expect } = import.meta.vitest;

  const tests = [
    {
      input: "1",
      want: "1",
    },
    {
      input: "+",
      want: "",
    },
    {
      input: "2930",
      want: "2930",
    },
    {
      input: "2 3 4",
      want: "2",
    },
    {
      input: "4.5",
      want: "4.5",
    },
    {
      input: "4...5",
      want: "4",
    },
    {
      input: "-3",
      want: "-3",
    },
    {
      input: "-",
      want: "",
    },
  ];

  for (const test of tests) {
    it(`should return ${test.want} for ${test.input}`, () => {
      expect(consumeNumberLiteral(test.input)).toBe(test.want);
    });
  }
}

const variableExp = /^[a-zA-Z_][a-zA-Z0-9_]*/;

const consumeVariable = (t: string): string => t.match(variableExp)?.[0] ?? "";

interface Token {
  type:
    | "plus"
    | "mult"
    | "div"
    | "minus"
    | "number"
    | "lparen"
    | "rparen"
    | "variable"
    | "assignment"
    | "def"
    | "semicolon";
  number?: number;
  variable?: string;
  position?: number;
}

type LexerError = {
  type: "unexpectedCharacter";
  got: string;
  position?: number;
};

const lexerError = (error: LexerError): ErrorWrapper<LexerError> =>
  new ErrorWrapper("lexerError", error);

const runLexer = (input: string, withPosition: boolean = true): Token[] => {
  const tokens: Token[] = [];
  let position = 0;

  const pushToken = (token: Token) => {
    tokens.push({
      ...token,
      position: withPosition ? position : undefined,
    });
  };

  while (position < input.length) {
    if (input[position] === " " || input[position] === "\n") {
      position++;
      continue;
    }

    const numberStr = consumeNumberLiteral(input.slice(position));
    if (numberStr) {
      pushToken({ type: "number", number: parseFloat(numberStr) });
      position += numberStr.length;
      continue;
    }

    if (input.slice(position, position + 3) === "def") {
      pushToken({ type: "def" });
      position += 3;
      continue;
    }

    const variableStr = consumeVariable(input.slice(position));
    if (variableStr) {
      pushToken({ type: "variable", variable: variableStr });
      position += variableStr.length;
      continue;
    }

    if (input[position] === "+") {
      pushToken({ type: "plus", position });
      position++;
      continue;
    }
    if (input[position] === "-") {
      pushToken({ type: "minus", position });
      position++;
      continue;
    }
    if (input[position] === "*") {
      pushToken({ type: "mult" });
      position++;
      continue;
    }
    if (input[position] === "/") {
      pushToken({ type: "div" });
      position++;
      continue;
    }
    if (input[position] === "(") {
      pushToken({ type: "lparen" });
      position++;
      continue;
    }
    if (input[position] === ")") {
      pushToken({ type: "rparen" });
      position++;
      continue;
    }
    if (input[position] === ";") {
      pushToken({ type: "semicolon" });
      position++;
      continue;
    }
    if (input.slice(position, position + 2) === ":=") {
      pushToken({ type: "assignment" });
      position += 2;
      continue;
    }

    throw lexerError({
      type: "unexpectedCharacter",
      got: input[position],
      position,
    });
  }

  return tokens;
};

if (import.meta.vitest) {
  const { it, expect, describe } = import.meta.vitest;

  describe("runLexer", () => {
    const tests = [
      {
        input: "1 + 2 * 4 / 2",
        want: [
          { type: "number", number: 1 },
          { type: "plus" },
          { type: "number", number: 2 },
          { type: "mult" },
          { type: "number", number: 4 },
          { type: "div" },
          { type: "number", number: 2 },
        ],
      },
      {
        input: "3 - -7.4",
        want: [
          { type: "number", number: 3 },
          { type: "minus" },
          { type: "number", number: -7.4 },
        ],
      },
    ];

    for (const test of tests) {
      it(`should return ${JSON.stringify(test.want)} for ${test.input}`, () => {
        expect(runLexer(test.input, false)).toEqual(test.want);
      });
    }
  });

  describe("runLexer errors", () => {
    const tests = [
      {
        input: "def f(x) = x",
        want: {
          type: "unexpectedCharacter",
          got: "=",
          position: 9,
        },
      },
    ];

    for (const test of tests) {
      it(`should return ${JSON.stringify(test.want)} for ${test.input}`, () => {
        try {
          runLexer(test.input, false);
          expect(true).toBe(false);
        } catch (e) {
          const error = e as ErrorWrapper<LexerError>;
          expect(error.value).toEqual(test.want);
        }
      });
    }
  });
}

type GLang = Statement[];

type Statement =
  | {
      type: "definition";
      name: string;
      arguments: string[];
      body: Expression;
    }
  | {
      type: "expression";
      expression: Expression;
    };

type Expression =
  | {
      type: "binaryOperator";
      operator: "plus" | "minus" | "mult" | "div";
      left: Expression;
      right: Expression;
    }
  | {
      type: "number";
      value: number;
    }
  | {
      type: "call";
      name: string;
      arguments: Expression[];
    }
  | {
      type: "variable";
      variable: string;
    };

type ParseError =
  | {
      type: "tokenMismatch";
      want: Token["type"];
      got: Token["type"];
    }
  | {
      type: "unexpectedToken";
      got: Token;
    }
  | {
      type: "definitionNotAllowed";
      got: Statement;
    }
  | {
      type: "unexpectedEos";
    };

const parseError = (error: ParseError): ErrorWrapper<ParseError> =>
  new ErrorWrapper("parseError", error);

const runParse = (tokens: Token[]): GLang => {
  let position = 0;

  const getToken = (): Token => {
    if (position < tokens.length) {
      return tokens[position];
    }

    throw parseError({ type: "unexpectedEos" });
  };
  const expect = (type: Token["type"]): Token => {
    const token = getToken();
    if (token.type !== type) {
      throw parseError({ type: "tokenMismatch", want: type, got: token.type });
    }
    position++;
    return token;
  };
  const expectVariable = (): string => {
    const token = getToken();
    if (token.type !== "variable") {
      throw parseError({
        type: "tokenMismatch",
        want: "variable",
        got: token.type,
      });
    }
    position++;
    return token.variable!;
  };

  const statements = (): GLang => {
    const stmts: GLang = [];
    while (position < tokens.length) {
      stmts.push(statement());

      if (position < tokens.length) {
        expect("semicolon");
      }
    }
    return stmts;
  };
  const statement = (): Statement => {
    const token = getToken();
    if (token.type === "def") {
      return definition();
    }

    const expr = expression();
    return { type: "expression", expression: expr };
  };
  const definition = (): Statement => {
    expect("def");

    const name = expectVariable();
    expect("lparen");
    const arg = expectVariable();
    expect("rparen");

    expect("assignment");

    const body = expression();

    return { type: "definition", name, arguments: [arg], body };
  };
  const expression = (): Expression => {
    return term2();
  };
  const term2 = (): Expression => {
    let current = term1();

    while (position < tokens.length) {
      const token = getToken();
      if (token.type === "plus" || token.type === "minus") {
        position++;
        current = {
          type: "binaryOperator",
          operator: token.type,
          left: current,
          right: term1(),
        };
      } else {
        break;
      }
    }

    return current;
  };
  const term1 = (): Expression => {
    let current = term0();

    while (position < tokens.length) {
      const token = getToken();
      if (token.type === "mult" || token.type === "div") {
        position++;
        current = {
          type: "binaryOperator",
          operator: token.type,
          left: current,
          right: term0(),
        };
      } else {
        break;
      }
    }

    return current;
  };
  const term0 = (): Expression => {
    const token = getToken();
    if (token.type === "number") {
      position++;
      return { type: "number", value: token.number! };
    }
    if (token.type === "lparen") {
      position++;
      const exp = expression();
      expect("rparen");
      return exp;
    }
    if (token.type === "variable") {
      position++;

      if (position < tokens.length && tokens[position].type === "lparen") {
        position++;
        const args = [expression()];
        expect("rparen");
        return { type: "call", name: token.variable!, arguments: args };
      } else {
        return { type: "variable", variable: token.variable! };
      }
    }

    throw parseError({ type: "unexpectedToken", got: token });
  };

  return statements();
};

const runParseExpression = (tokens: Token[]): Expression => {
  const result = runParse(tokens)[0];
  if (result.type === "expression") {
    return result.expression;
  }

  throw parseError({ type: "definitionNotAllowed", got: result });
};

if (import.meta.vitest) {
  const { it, expect, describe } = import.meta.vitest;

  describe("runParseExpression", () => {
    const tests = [
      {
        input: "200.2",
        want: {
          type: "number",
          value: 200.2,
        },
      },
      {
        input: "4 + 2 - 1",
        want: {
          type: "binaryOperator",
          operator: "minus",
          left: {
            type: "binaryOperator",
            operator: "plus",
            left: { type: "number", value: 4 },
            right: { type: "number", value: 2 },
          },
          right: { type: "number", value: 1 },
        },
      },
      {
        input: "1 + 2 + 3 + 4 + 5 + 6 + 7",
        want: {
          type: "binaryOperator",
          operator: "plus",
          left: {
            type: "binaryOperator",
            operator: "plus",
            left: {
              type: "binaryOperator",
              operator: "plus",
              left: {
                type: "binaryOperator",
                operator: "plus",
                left: {
                  type: "binaryOperator",
                  operator: "plus",
                  left: {
                    type: "binaryOperator",
                    operator: "plus",
                    left: {
                      type: "number",
                      value: 1,
                    },
                    right: {
                      type: "number",
                      value: 2,
                    },
                  },
                  right: {
                    type: "number",
                    value: 3,
                  },
                },
                right: {
                  type: "number",
                  value: 4,
                },
              },
              right: {
                type: "number",
                value: 5,
              },
            },
            right: {
              type: "number",
              value: 6,
            },
          },
          right: {
            type: "number",
            value: 7,
          },
        },
      },
      {
        input: "1 + 2 * 4",
        want: {
          type: "binaryOperator",
          operator: "plus",
          left: { type: "number", value: 1 },
          right: {
            type: "binaryOperator",
            operator: "mult",
            left: { type: "number", value: 2 },
            right: { type: "number", value: 4 },
          },
        },
      },
      {
        input: "2 / 4 - 4",
        want: {
          type: "binaryOperator",
          operator: "minus",
          left: {
            type: "binaryOperator",
            operator: "div",
            left: { type: "number", value: 2 },
            right: { type: "number", value: 4 },
          },
          right: { type: "number", value: 4 },
        },
      },
      {
        input: "1 + 2 * 4 / 2 - 1",
        want: {
          type: "binaryOperator",
          operator: "minus",
          left: {
            type: "binaryOperator",
            operator: "plus",
            left: { type: "number", value: 1 },
            right: {
              type: "binaryOperator",
              operator: "div",
              left: {
                type: "binaryOperator",
                operator: "mult",
                left: { type: "number", value: 2 },
                right: { type: "number", value: 4 },
              },
              right: { type: "number", value: 2 },
            },
          },
          right: { type: "number", value: 1 },
        },
      },
      {
        input: "(1 + 2) * 2",
        want: {
          type: "binaryOperator",
          operator: "mult",
          left: {
            type: "binaryOperator",
            operator: "plus",
            left: { type: "number", value: 1 },
            right: { type: "number", value: 2 },
          },
          right: { type: "number", value: 2 },
        },
      },
    ];

    for (const test of tests) {
      it(`should return ${JSON.stringify(test.want)} for ${test.input}`, () => {
        expect(runParseExpression(runLexer(test.input))).toEqual(test.want);
      });
    }
  });

  describe("runParse", () => {
    const tests = [
      {
        input: "def f(x) := x; f(2)",
        want: [
          {
            type: "definition",
            name: "f",
            arguments: ["x"],
            body: { type: "variable", variable: "x" },
          },
          {
            type: "expression",
            expression: {
              type: "call",
              name: "f",
              arguments: [{ type: "number", value: 2 }],
            },
          },
        ],
      },
    ];

    for (const test of tests) {
      it(`should return ${JSON.stringify(test.want)} for ${test.input}`, () => {
        expect(runParse(runLexer(test.input))).toEqual(test.want);
      });
    }
  });

  describe("runParse errors", () => {
    const tests = [
      {
        input: "def f(",
        want: {
          type: "unexpectedEos",
        },
      },
      {
        input: "def f(x) x",
        want: {
          type: "tokenMismatch",
          want: "assignment",
          got: "variable",
        },
      },
      {
        input: "1 + def",
        want: {
          type: "unexpectedToken",
          got: { type: "def" },
        },
      },
    ];

    for (const test of tests) {
      it(`should return ${JSON.stringify(test.want)} for ${test.input}`, () => {
        try {
          runParse(runLexer(test.input, false));
          expect(true).toBe(false);
        } catch (e) {
          const error = e as ErrorWrapper<ParseError>;
          expect(error.value).toEqual(test.want);
        }
      });
    }
  });
}

type Value =
  | { type: "number"; value: number }
  | {
      type: "function";
      name: string;
      arguments: string[];
      body: Expression;
    };

const expectNumber = (value: Value): number => {
  if (value.type !== "number") {
    throw new Error("Expected number");
  }
  return value.value;
};

const interpret = (ast: GLang): Value => {
  const defs: Record<string, Statement> = {};

  for (const stmt of ast) {
    if (stmt.type === "definition") {
      defs[stmt.name] = stmt;
    } else if (stmt.type === "expression") {
      return interpretExpression(stmt.expression, defs, {});
    } else {
      throw new Error("Invalid AST");
    }
  }

  throw new Error("No expression found");
};
const interpretExpression = (
  ast: Expression,
  defs: Record<string, Statement>,
  assignments: Record<string, number>
): Value => {
  if (ast.type === "number") {
    return {
      type: "number",
      value: ast.value,
    };
  }
  if (ast.type === "binaryOperator") {
    switch (ast.operator) {
      case "plus":
        return {
          type: "number",
          value:
            expectNumber(interpretExpression(ast.left, defs, assignments)) +
            expectNumber(interpretExpression(ast.right, defs, assignments)),
        };
      case "minus":
        return {
          type: "number",
          value:
            expectNumber(interpretExpression(ast.left, defs, assignments)) -
            expectNumber(interpretExpression(ast.right, defs, assignments)),
        };
      case "mult":
        return {
          type: "number",
          value:
            expectNumber(interpretExpression(ast.left, defs, assignments)) *
            expectNumber(interpretExpression(ast.right, defs, assignments)),
        };
      case "div":
        return {
          type: "number",
          value:
            expectNumber(interpretExpression(ast.left, defs, assignments)) /
            expectNumber(interpretExpression(ast.right, defs, assignments)),
        };
    }
  }
  if (ast.type === "call") {
    const statement = defs[ast.name];
    if (statement.type === "expression") {
      throw new Error("Expected function definition");
    }

    const newAssignments = { ...assignments };
    for (let i = 0; i < statement.arguments.length; i++) {
      newAssignments[statement.arguments[i]] = expectNumber(
        interpretExpression(ast.arguments[i], defs, newAssignments)
      );
    }

    return interpretExpression(statement.body, defs, newAssignments);
  }
  if (ast.type === "variable") {
    if (ast.variable in defs) {
      const def = defs[ast.variable];
      if (def.type === "definition") {
        return {
          type: "function",
          name: def.name,
          arguments: def.arguments,
          body: def.body,
        };
      }

      throw new Error("Invalid AST");
    }
    if (ast.variable in assignments) {
      return { type: "number", value: assignments[ast.variable] };
    }

    throw new Error("Undefined variable: " + ast.variable);
  }

  throw new Error("Invalid AST");
};

if (import.meta.vitest) {
  const { it, expect, describe } = import.meta.vitest;

  describe("interpretExpression", () => {
    const tests = [
      {
        input: "1 + 2 * 4 / 2",
        want: 5,
      },
      {
        input: "3 - -7.4",
        want: 10.4,
      },
      {
        input: "200.2",
        want: 200.2,
      },
      {
        input: "4 + 2 - 1",
        want: 5,
      },
      {
        input: "1 + 2 + 3 + 4 + 5 + 6 + 7",
        want: 28,
      },
      {
        input: "1 + 2 * 4",
        want: 9,
      },
      {
        input: "2 / 4 - 4",
        want: -3.5,
      },
      {
        input: "1 + 2 * 4 / 2 - 1",
        want: 4,
      },
      {
        input: "(1 + 2) * 2",
        want: 6,
      },
    ];

    for (const test of tests) {
      it(`should return ${test.want} for ${test.input}`, () => {
        expect(
          expectNumber(
            interpretExpression(
              runParseExpression(runLexer(test.input)),
              {},
              {}
            )
          )
        ).toBe(test.want);
      });
    }
  });

  describe("interpret returns number", () => {
    const tests = [
      {
        input: "def f(x) := x; f(2)",
        want: 2,
      },
      {
        input: "def f(x) := x + 2; f(2)",
        want: 4,
      },
      {
        input: "def f(x) := x + 2; def g(x) := x * x; g(f(2))",
        want: 16,
      },
      {
        input: "def f(x) := x + 2; def g(x) := x * x; f(g(2))",
        want: 6,
      },
    ];

    for (const test of tests) {
      it(`should return ${test.want} for ${test.input}`, () => {
        expect(expectNumber(interpret(runParse(runLexer(test.input))))).toEqual(
          test.want
        );
      });
    }
  });

  describe("interpret returns function", () => {
    const tests = [
      {
        input: "def f(x) := x; f",
        want: {
          type: "function",
          name: "f",
          arguments: ["x"],
          body: {
            type: "variable",
            variable: "x",
          },
        },
      },
    ];

    for (const test of tests) {
      it(`should return ${JSON.stringify(test.want)} for ${test.input}`, () => {
        expect(interpret(runParse(runLexer(test.input)))).toEqual(test.want);
      });
    }
  });
}

if (process.env.NODE_ENV !== "test") {
  const doPlot = process.argv.findIndex((arg) => arg === "--plot") !== -1;

  const plotStartIndex = process.argv.findIndex(
    (arg) => arg === "--plot-start"
  );
  const plotStart =
    plotStartIndex !== -1 ? parseFloat(process.argv[plotStartIndex + 1]) : -1;
  const plotEndIndex = process.argv.findIndex((arg) => arg === "--plot-end");
  const plotEnd =
    plotEndIndex !== -1 ? parseFloat(process.argv[plotEndIndex + 1]) : 1;

  const arg = process.argv.findIndex((arg) => arg === "-e");
  const input = process.argv[arg + 1];
  if (arg !== -1) {
    try {
      const result = interpret(runParse(runLexer(input)));
      if (result.type === "number") {
        console.log(result.value);
      } else {
        if (doPlot) {
          const steps = 100;
          const ids: number[] = [];
          const xs: number[] = [];
          const ys: number[] = [];
          for (let i = 0; i <= steps; i++) {
            const x = plotStart + (plotEnd - plotStart) * (i / steps);
            const y = expectNumber(
              interpretExpression(result.body, {}, { [result.arguments[0]]: x })
            );

            console.log(`f(${x}) = ${y}`);
            ids.push(i);
            xs.push(x);
            ys.push(y);
          }
          spawnSync("gnuplot", ["-p", "-persist"], {
            input: `plot '-' with lines\n${ids
              .map((i) => `${xs[i]} ${ys[i]}`)
              .join("\n")}\ne\n`,
          });
        }
      }
    } catch (err) {
      if (err instanceof ErrorWrapper) {
        if (err.name === "lexerError") {
          const errValue = (err as ErrorWrapper<LexerError>).value;

          console.error(`${input}\n${" ".repeat(errValue.position!)}^`);
        }
      }

      throw err;
    }
  } else {
    console.log(`Usage: bun run ${process.argv[1]} -e "expression"`);
  }
}
