#!/usr/bin/env python3
""" 2018 AOC Day 6 """

import argparse
from collections import defaultdict
import itertools
import re
import typing
import unittest

Coord = typing.Tuple[int, int]
T = typing.TypeVar('T')


def parse(puzzle: str) -> typing.List[Coord]:
    ''' Parse the input into a list of (int, int) tuples '''
    line_re = re.compile(r'(?P<x>\d+), (?P<y>\d+)')
    coords = []
    for line in puzzle.splitlines():
        match = line_re.match(line)
        if not match:
            raise ValueError('Invalid line: ' + line)
        coords.append((int(match['x']), int(match['y'])))
    return coords


def manhattan_dist(a: Coord, b: Coord) -> int:
    ''' Manhattan distance between two coordinates '''
    return abs(a[0] - b[0]) + abs(a[1] - b[1])


def iter_border(upper_left: Coord, lower_right: Coord) -> typing.Iterator[Coord]:
    ''' Iterate over square border defined by the given coordinates, clockwise
    '''
    for x in range(upper_left[0], lower_right[0] + 1):
        yield (x, upper_left[1])
    for y in range(upper_left[1], lower_right[1] + 1):
        yield (lower_right[0], y)
    for x in range(lower_right[0], upper_left[0] - 1, -1):
        yield (x, lower_right[1])
    for y in range(lower_right[1], upper_left[1] - 1, -1):
        yield (upper_left[0], y)


def iter_area(upper_left: Coord, lower_right: Coord):
    ''' Iterate over square area defined by the given coordinates '''
    for x in range(upper_left[0], lower_right[0] + 1):
        for y in range(upper_left[1], lower_right[1] + 1):
            yield (x, y)


def lesser_median(iterable: typing.Iterable[T]) -> T:
    ''' Return the median element in `iterable`. `iterable` must be comparable.
        If there are an even number of elements, this will pick the smaller of the
        two middle elements. E.g. lesser_median([1, 2, 3, 4]) will return 2.
    '''
    ordered = sorted(iterable)
    return ordered[len(ordered) // 2]


def find_bound(
    start: T,
    produce: typing.Callable[[T], T],
    in_bounds: typing.Callable[[T], bool],
) -> typing.Optional[T]:
    ''' Starting at `start`, repeatedly apply `produce`, and return last value
        such that `in_bounds` returns true. Returns None if `start` is out of
        bounds.
    '''
    current, previous = start, None
    while in_bounds(current):
        previous, current = current, produce(current)
    return previous


def part1(puzzle):
    """ Solve part 1 """
    coords = parse(puzzle)

    def closest(start):
        return min(coords, key=lambda coord: manhattan_dist(start, coord))

    upper_left = (min(c[0] for c in coords), min(c[1] for c in coords))
    lower_right = (max(c[0] for c in coords), max(c[1] for c in coords))

    infinite = set(closest(point) for point in iter_border(upper_left, lower_right))
    areas = defaultdict(lambda: 0)
    for point in iter_area(upper_left, lower_right):
        nearest = closest(point)
        if nearest not in infinite:
            areas[nearest] += 1
    return max(areas.values())


def part2(puzzle, dist_limit=10000):
    """ Solve part 2 """
    coords = parse(puzzle)

    def total_dist(p):
        return sum(manhattan_dist(p, c) for c in coords)

    def in_bounds(p):
        return total_dist(p) < dist_limit

    median = (lesser_median(p[0] for p in coords), lesser_median(p[1] for p in coords))
    upper_left = (
        find_bound(median[0], lambda x: x - 1, lambda x: in_bounds((x, median[1]))),
        find_bound(median[1], lambda y: y - 1, lambda y: in_bounds((median[0], y))),
    )
    lower_right = (
        find_bound(median[0], lambda x: x + 1, lambda x: in_bounds((x, median[1]))),
        find_bound(median[1], lambda y: y + 1, lambda y: in_bounds((median[0], y))),
    )
    return sum(1 for p in iter_area(upper_left, lower_right) if in_bounds(p))


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
        self.assertEqual(part1('1, 1\n1, 6\n8, 3\n3, 4\n5, 5\n8, 9'), 17)


class Part2Test(unittest.TestCase):
    def test_example(self):
        self.assertEqual(part2('1, 1\n1, 6\n8, 3\n3, 4\n5, 5\n8, 9', dist_limit=32), 16)
