#!/usr/bin/env python3

import argparse
import collections
from dataclasses import dataclass
import itertools
import numpy as np
import re
import typing


@dataclass
class Instruction:
    id: int
    loc: typing.Tuple[int, int]
    width: int
    height: int


def parse(puzzle):
    parser = re.compile(
        r'#(?P<id>\d+) @ (?P<x>\d+),(?P<y>\d+): (?P<w>\d+)x(?P<h>\d+)'
    )
    instructions = []
    for line in puzzle:
        match = parser.match(line)
        if match:
            instructions.append(Instruction(
                id=int(match['id']),
                loc=(int(match['x']), int(match['y'])),
                width=int(match['w']),
                height=int(match['h']),
            ))
        else:
            raise(ValueError('Could not parse instruction: ' + line))
    return instructions


def part1(puzzle):
    instructions = parse(puzzle)
    grid = np.zeros((1000, 1000), dtype=np.int8)
    for instr in instructions:
        grid[instr.loc[0]:(instr.loc[0] + instr.width),
             instr.loc[1]:(instr.loc[1] + instr.height)] += 1
    return np.count_nonzero(grid >= 2)


def part2(puzzle):
    instructions = parse(puzzle)
    grid = np.zeros((1000, 1000), dtype=np.int8)
    for instr in instructions:
        grid[instr.loc[0]:(instr.loc[0] + instr.width),
             instr.loc[1]:(instr.loc[1] + instr.height)] += 1
    for instr in instructions:
        if np.all(grid[instr.loc[0]:(instr.loc[0] + instr.width),
                       instr.loc[1]:(instr.loc[1] + instr.height)] == 1):
            return instr.id


def main():
    parser = argparse.ArgumentParser(description='Advent of Code 2018 Day 02')
    parser.add_argument('input', help='input file')
    opts = parser.parse_args()
    with open(opts.input) as handle:
        puzzle = [line.strip() for line in handle.readlines()]
    print('Part 1:', part1(puzzle))
    print('Part 2:', part2(puzzle))


if __name__ == '__main__':
    main()
