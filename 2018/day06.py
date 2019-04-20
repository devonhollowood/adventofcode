#!/usr/bin/env python3
""" 2018 AOC Day 6 """

import argparse
import re
import typing
import unittest


Coord = typing.Tuple[int, int]


def parse(puzzle: str) -> typing.List[Coord]:
    ''' Parse the input into a list of (int, int) tuples '''
    line_re = re.compile(r'\((?P<x>\d+), (?P<y>\d+)\)')
    coords = []
    for line in puzzle.splitlines():
        match = line_re.match(line)
        if not match:
            raise ValueError('Invalid line: ' + line)
        coords.append((int(match['x']), int(match['y'])))
    return coords


def manhattan_dist(a: Coord, b: Coord) -> int:
    ''' Manhattan distance between two coordinates '''
    return abs(a[0]-b[0]) + abs(a[1]-b[1])


def part1(puzzle):
    """ Solve part 1 """
    coords = parse(puzzle)
    upper_left = (min(c[0] for c in coords), min(c[1] for c in coords))
    lower_right = (max(c[0] for c in coords), max(c[1] for c in coords))


def part2(puzzle):
    """ Solve part 2 """
    pass


def main():
    """ Run 2018 Day 6 """
    parser = argparse.ArgumentParser(description='Advent of Code 2018 Day 06')
    parser.add_argument('input', help='input file')
    opts = parser.parse_args()
    with open(opts.input) as handle:
        puzzle = handle.read().strip()
    print('Part 1:', part1(puzzle))
    print('Part 2:', part2(puzzle))


if __name__ == '__main__':
    main()


class Part1Test(unittest.TestCase):
    def test_example(self):
        self.assertEq(part1('1, 1\n1, 6\n8, 3\n3, 4\n5, 5\n8, 9'), 17)


class Part2Test(unittest.TestCase):
    pass
