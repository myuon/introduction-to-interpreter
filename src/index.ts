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
}

const runLexer = (input: string): Token[] => {
  const tokens: Token[] = [];
  let position = 0;
  while (position < input.length) {
    if (input[position] === " ") {
      position++;
      continue;
    }

    const numberStr = consumeNumberLiteral(input.slice(position));
    if (numberStr) {
      tokens.push({ type: "number", number: parseFloat(numberStr) });
      position += numberStr.length;
      continue;
    }

    if (input.slice(position, position + 3) === "def") {
      tokens.push({ type: "def" });
      position += 3;
      continue;
    }

    const variableStr = consumeVariable(input.slice(position));
    if (variableStr) {
      tokens.push({ type: "variable", variable: variableStr });
      position += variableStr.length;
      continue;
    }

    if (input[position] === "+") {
      tokens.push({ type: "plus" });
      position++;
      continue;
    }
    if (input[position] === "-") {
      tokens.push({ type: "minus" });
      position++;
      continue;
    }
    if (input[position] === "*") {
      tokens.push({ type: "mult" });
      position++;
      continue;
    }
    if (input[position] === "/") {
      tokens.push({ type: "div" });
      position++;
      continue;
    }
    if (input[position] === "(") {
      tokens.push({ type: "lparen" });
      position++;
      continue;
    }
    if (input[position] === ")") {
      tokens.push({ type: "rparen" });
      position++;
      continue;
    }
    if (input[position] === ";") {
      tokens.push({ type: "semicolon" });
      position++;
      continue;
    }
    if (input.slice(position, position + 2) === ":=") {
      tokens.push({ type: "assignment" });
      position += 2;
      continue;
    }

    throw new Error("Invalid character: " + input[position]);
  }

  return tokens;
};

if (import.meta.vitest) {
  const { it, expect } = import.meta.vitest;

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
      expect(runLexer(test.input)).toEqual(test.want);
    });
  }
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

const runParse = (tokens: Token[]): GLang => {
  let position = 0;

  const expect = (type: Token["type"]): Token => {
    const token = tokens[position];
    if (token.type !== type) {
      throw new Error(`Expected ${type}`);
    }
    position++;
    return token;
  };
  const expectVariable = (): string => {
    const token = tokens[position];
    if (token.type !== "variable") {
      throw new Error("Expected variable");
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
    const token = tokens[position];
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
      const token = tokens[position];
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
      const token = tokens[position];
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
    const token = tokens[position];
    if (token.type === "number") {
      position++;
      return { type: "number", value: token.number! };
    }
    if (token.type === "lparen") {
      position++;
      const exp = expression();
      if (tokens[position].type !== "rparen") {
        throw new Error("Expected )");
      }
      position++;
      return exp;
    }
    if (token.type === "variable") {
      position++;

      if (tokens[position].type === "lparen") {
        position++;
        const args = [expression()];
        expect("rparen");
        return { type: "call", name: token.variable!, arguments: args };
      } else {
        return { type: "variable", variable: token.variable! };
      }
    }

    throw new Error("Expected number");
  };

  return statements();
};

const runParseExpression = (tokens: Token[]): Expression => {
  const result = runParse(tokens)[0];
  if (result.type === "expression") {
    return result.expression;
  }

  throw new Error("Invalid expression");
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
}

const interpret = (ast: GLang): number => {
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
): number => {
  if (ast.type === "number") {
    return ast.value;
  }
  if (ast.type === "binaryOperator") {
    switch (ast.operator) {
      case "plus":
        return (
          interpretExpression(ast.left, defs, assignments) +
          interpretExpression(ast.right, defs, assignments)
        );
      case "minus":
        return (
          interpretExpression(ast.left, defs, assignments) -
          interpretExpression(ast.right, defs, assignments)
        );
      case "mult":
        return (
          interpretExpression(ast.left, defs, assignments) *
          interpretExpression(ast.right, defs, assignments)
        );
      case "div":
        return (
          interpretExpression(ast.left, defs, assignments) /
          interpretExpression(ast.right, defs, assignments)
        );
    }
  }
  if (ast.type === "call") {
    const statement = defs[ast.name];
    if (statement.type === "expression") {
      throw new Error("Expected function definition");
    }

    const newAssignments = { ...assignments };
    for (let i = 0; i < statement.arguments.length; i++) {
      newAssignments[statement.arguments[i]] = interpretExpression(
        ast.arguments[i],
        defs,
        newAssignments
      );
    }

    return interpretExpression(statement.body, defs, newAssignments);
  }
  if (ast.type === "variable") {
    return assignments[ast.variable];
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
          interpretExpression(runParseExpression(runLexer(test.input)), {}, {})
        ).toBe(test.want);
      });
    }
  });

  describe("interpret", () => {
    const tests = [
      {
        input: "def f(x) := x; f(2)",
        want: 2,
      },
      {
        input: "def f(x) := x + 2; f(2)",
        want: 4,
      },
    ];

    for (const test of tests) {
      it(`should return ${test.want} for ${test.input}`, () => {
        expect(interpret(runParse(runLexer(test.input)))).toEqual(test.want);
      });
    }
  });
}

if (process.env.NODE_ENV !== "test") {
  const arg = process.argv.findIndex((arg) => arg === "-e");
  if (arg !== -1) {
    console.log(interpret(runParse(runLexer(process.argv[arg + 1]))));
  } else {
    console.log(`Usage: node ${process.argv[1]} -e "expression"`);
  }
}
