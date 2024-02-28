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

interface Token {
  type: "plus" | "mult" | "div" | "minus" | "number" | "lparen" | "rparen";
  number?: number;
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

type AST =
  | {
      type: "binaryOperator";
      operator: "plus" | "minus" | "mult" | "div";
      left: AST;
      right: AST;
    }
  | {
      type: "number";
      value: number;
    };

const runParse = (tokens: Token[]): AST => {
  let position = 0;

  const term = (): AST => {
    return term2();
  };
  const term2 = (): AST => {
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
  const term1 = (): AST => {
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
  const term0 = (): AST => {
    const token = tokens[position];
    if (token.type === "number") {
      position++;
      return { type: "number", value: token.number! };
    }
    if (token.type === "lparen") {
      position++;
      const exp = term();
      if (tokens[position].type !== "rparen") {
        throw new Error("Expected )");
      }
      position++;
      return exp;
    }

    throw new Error("Expected number");
  };

  return term2();
};

if (import.meta.vitest) {
  const { it, expect } = import.meta.vitest;

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
      expect(runParse(runLexer(test.input))).toEqual(test.want);
    });
  }
}

const interpret = (ast: AST): number => {
  if (ast.type === "number") {
    return ast.value;
  }
  if (ast.type === "binaryOperator") {
    switch (ast.operator) {
      case "plus":
        return interpret(ast.left) + interpret(ast.right);
      case "minus":
        return interpret(ast.left) - interpret(ast.right);
      case "mult":
        return interpret(ast.left) * interpret(ast.right);
      case "div":
        return interpret(ast.left) / interpret(ast.right);
    }
  }

  throw new Error("Invalid AST");
};

if (import.meta.vitest) {
  const { it, expect } = import.meta.vitest;

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
      expect(interpret(runParse(runLexer(test.input)))).toBe(test.want);
    });
  }
}

const arg = process.argv.findIndex((arg) => arg === "-e");
if (arg !== -1) {
  console.log(interpret(runParse(runLexer(process.argv[arg + 1]))));
} else {
  console.log(`Usage: node ${process.argv[1]} -e "expression"`);
}
