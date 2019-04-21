#!/usr/bin/env python3
""" 2018 AOC Day 07 """

import argparse
from collections import defaultdict
import heapq
import re
import typing
import unittest


def parse(puzzle: str) -> typing.List[typing.Tuple[str, str]]:
    ''' Parse the input into a list of (step, requirement) tuples '''
    line_re = re.compile(r'Step (?P<req>\w) must be finished before step (?P<step>\w) can begin.')
    parsed = []
    for line in puzzle.splitlines():
        match = line_re.match(line.strip())
        if not match:
            raise ValueError('Bad line: ' + line)
        parsed.append((match['step'], match['req']))
    return parsed


def part1(puzzle: str) -> str:
    """ Solve part 1 """
    steps = set()
    requirements: typing.Mapping[str, typing.List[str]] = defaultdict(list)
    reverse_reqs: typing.Mapping[str, typing.List[str]] = defaultdict(list)
    for (step, requirement) in parse(puzzle):
        steps.add(step)
        steps.add(requirement)
        requirements[step].append(requirement)
        reverse_reqs[requirement].append(step)
    heap = [step for step in steps if not requirements[step]]
    heapq.heapify(heap)
    complete: typing.Set[str] = set()
    final_order = ''
    while heap:
        next_step = heapq.heappop(heap)
        complete.add(next_step)
        final_order += next_step
        for rev_req in reverse_reqs[next_step]:
            if all(r in complete for r in requirements[rev_req]):
                heapq.heappush(heap, rev_req)
    return final_order


def part2(puzzle: str) -> str:
    """ Solve part 2 """
    pass


def main():
    """ Run 2018 Day 07 """
    parser = argparse.ArgumentParser(description='Advent of Code 2018 Day 07')
    parser.add_argument('input', help='input file')
    opts = parser.parse_args()
    with open(opts.input) as handle:
        puzzle = handle.read().strip()
    print('Part 1:', part1(puzzle))
    print('Part 2:', part2(puzzle))


if __name__ == '__main__':
    main()


class Part1Test(unittest.TestCase):
    example = (
        'Step C must be finished before step A can begin.\n'
        'Step C must be finished before step F can begin.\n'
        'Step A must be finished before step B can begin.\n'
        'Step A must be finished before step D can begin.\n'
        'Step B must be finished before step E can begin.\n'
        'Step D must be finished before step E can begin.\n'
        'Step F must be finished before step E can begin.\n'
    )

    def test_example(self):
        self.assertEqual(part1(self.example), 'CABDFE')


class Part2Test(unittest.TestCase):
    def test_example(self):
        pass
