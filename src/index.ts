const numberLiteralExp = /^\-?\d?\.?\d+/;

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

const lexer = (input: string): Token[] => {
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
      expect(lexer(test.input)).toEqual(test.want);
    });
  }
}

console.log("1 + 2 * 4 / 2");
