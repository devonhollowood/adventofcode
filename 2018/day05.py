#!/usr/bin/env python3
""" 2018 AOC Day 5 """

import argparse
import unittest


def part1(puzzle):
    """ Solve part 1 """
    stack = []
    for char in puzzle:
        if not stack:
            stack.append(char)
            continue
        if stack[-1] == char.swapcase():
            stack.pop()
        else:
            stack.append(char)
    return len(stack)


def part2(puzzle):
    """ Solve part 2 """
    elements = set(puzzle.lower())
    replace_lens = {
        elem: part1(puzzle.replace(elem, '').replace(elem.swapcase(), ''))
        for elem in elements
    }
    return min(replace_lens.values())


def main():
    """ Run 2018 Day 4 """
    parser = argparse.ArgumentParser(description='Advent of Code 2018 Day 02')
    parser.add_argument('input', help='input file')
    opts = parser.parse_args()
    with open(opts.input) as handle:
        puzzle = handle.read().strip()
    print('Part 1:', part1(puzzle))
    print('Part 2:', part2(puzzle))


if __name__ == '__main__':
    main()


class Part1Test(unittest.TestCase):
    def test_examples(self):
        self.assertEqual(part1('aA'), 0)
        self.assertEqual(part1('abBA'), 0)
        self.assertEqual(part1('abAB'), 4)
        self.assertEqual(part1('aabAAB'), 6)
        self.assertEqual(part1('dabAcCaCBAcCcaDA'), 10)


class Part2Test(unittest.TestCase):
    def test_example(self):
        self.assertEqual(part2('dabAcCaCBAcCcaDA'), 4)
