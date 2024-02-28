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
  type: "plus" | "mult" | "div" | "minus" | "number";
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

  const term1 = (): AST => {
    let current = number();

    while (position < tokens.length) {
      const token = tokens[position];
      if (token.type === "plus" || token.type === "minus") {
        position++;
        current = {
          type: "binaryOperator",
          operator: token.type,
          left: current,
          right: number(),
        };
      } else {
        break;
      }
    }

    return current;
  };
  const number = (): AST => {
    const token = tokens[position];
    if (token.type === "number") {
      position++;
      return { type: "number", value: token.number! };
    }

    throw new Error("Expected number");
  };

  return term1();
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
  ];

  for (const test of tests) {
    it(`should return ${JSON.stringify(test.want)} for ${test.input}`, () => {
      expect(runParse(runLexer(test.input))).toEqual(test.want);
    });
  }
}

console.log("1 + 2 * 4 / 2");
