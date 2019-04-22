#!/usr/bin/env python3
""" 2018 AOC Day 08 """

import argparse
from dataclasses import dataclass
import typing
import unittest


@dataclass
class Node(object):
    children: typing.List['Node']
    metadata: typing.List[int]


def parse_node(stream: typing.Iterator[int]) -> Node:
    ''' Parse a single node from input stream '''
    nchildren = next(stream)
    nmetadata = next(stream)
    return Node(
        children=[parse_node(stream) for _ in range(nchildren)],
        metadata=[next(stream) for _ in range(nmetadata)]
    )


def parse(puzzle: str) -> Node:
    ''' Parse the puzzle input into a tree '''
    stream = (int(elem) for elem in puzzle.split())
    result = parse_node(stream)
    if next(stream, None) is not None:
        raise AssertionError("Did not exhaust stream")
    return result


def metadata_sum(tree: Node) -> int:
    ''' Get sum of metadata values of `tree` and its children '''
    child_sum = sum(metadata_sum(child) for child in tree.children)
    return child_sum + sum(tree.metadata)


def value(node: Node) -> int:
    if not node.children:
        return sum(node.metadata)
    total = 0
    for idx in node.metadata:
        if idx == 0:
            continue
        try:
            total += value(node.children[idx - 1])
        except IndexError:
            pass  # skip OOB nodes
    return total


def part1(puzzle: str) -> int:
    """ Solve part 1 """
    tree = parse(puzzle)
    return metadata_sum(tree)


def part2(puzzle: str) -> int:
    """ Solve part 2 """
    tree = parse(puzzle)
    return value(tree)


def main():
    """ Run 2018 Day 08 """
    parser = argparse.ArgumentParser(description='Advent of Code 2018 Day 08')
    parser.add_argument('input', help='input file')
    opts = parser.parse_args()
    with open(opts.input) as handle:
        puzzle = handle.read().strip()
    print('Part 1:', part1(puzzle))
    print('Part 2:', part2(puzzle))


if __name__ == '__main__':
    main()


class ExampleTest(unittest.TestCase):
    example = ('2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2')

    def test_part1(self):
        self.assertEqual(part1(self.example), 138)

    def test_part2(self):
        self.assertEqual(part2(self.example), 66)
