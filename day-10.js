const assert = require("assert/strict");
const fs = require("fs");

function fileInput(path) {
  return fs
    .readFileSync(path, { encoding: "utf-8" })
    .trim()
    .split(/\r?\n/)
    .map((line) => line.split(""));
}

const testInput = fileInput("day-10-test-input.txt");
const input = fileInput("day-10-input.txt");

const match = {
  "(": ")",
  "[": "]",
  "{": "}",
  "<": ">",
};

const corruptedScore = {
  ")": 3,
  "]": 57,
  "}": 1197,
  ">": 25137,
};

const incompleteScore = {
  ")": 1,
  "]": 2,
  "}": 3,
  ">": 4,
};

function parse(chrs) {
  const stack = [];

  for (let chr of chrs) {
    if (match[chr]) {
      stack.push(chr);
    } else if (chr === match[stack[stack.length - 1]]) {
      stack.pop();
    } else {
      return {
        valid: false,
        error: "corrupted",
        chr,
      };
    }
  }

  if (stack.length) {
    return {
      valid: false,
      error: "incomplete",
      stack,
    };
  }

  return {
    valid: true,
    str: str.join(""),
  };
}

// Part 1

function solveCorrupted(inputs) {
  return inputs
    .map(parse)
    .filter((result) => result.error === "corrupted")
    .reduce((totalScore, result) => totalScore + corruptedScore[result.chr], 0);
}

assert.equal(solveCorrupted(testInput), 26397);
console.log(solveCorrupted(input));

// Part 2

function solveIncomplete(inputs) {
  const scores = inputs
    .map(parse)
    .filter((result) => result.error === "incomplete")
    .map((result) =>
      [...result.stack]
        .reverse()
        .reduce(
          (totalScore, chr) => 5 * totalScore + incompleteScore[match[chr]],
          0
        )
    );
  scores.sort((a, b) => a - b);

  return scores[(scores.length - 1) / 2];
}

assert.equal(solveIncomplete(testInput), 288957);
console.log(solveIncomplete(input));
