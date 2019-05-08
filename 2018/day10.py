#!/usr/bin/env python3
""" 2018 AOC Day 10 """

import argparse
from dataclasses import dataclass
import re
import typing
import unittest


@dataclass
class Vector:
    ''' 2D vector '''
    x: int
    y: int


@dataclass
class Point:
    ''' One point on the grid '''
    position: Vector
    velocity: Vector

    def step(self) -> 'Point':
        ''' Return copy of point one time step in future '''
        return Point(
            position=Vector(x=self.position.x + self.velocity.x,
                            y=self.position.y + self.velocity.y),
            velocity=self.velocity)


def bounding_circumference(points: typing.List[Point]) -> int:
    ''' Return bounding circumference of points '''
    left = min(points, key=lambda point: point.position.x, default=0).position.x
    right = max(points, key=lambda point: point.position.x, default=0).position.x
    top = min(points, key=lambda point: point.position.y, default=0).position.y
    bottom = max(points, key=lambda point: point.position.y, default=0).position.y
    return 2 * (right - left) + 2 * (bottom - top)


def run_until_compact(points: typing.List[Point]) -> typing.List[Point]:
    ''' Return points at their most compact '''
    # Take advantage of the fact that the bounding box monotonically decreases in circumference,
    # then monotonically increases
    while True:
        previous = points
        previous_circumference = bounding_circumference(points)
        points = [point.step() for point in points]
        circumference = bounding_circumference(points)
        if circumference > previous_circumference:
            return previous


def parse(puzzle: str) -> typing.List[Point]:
    ''' Parse points list from input puzzle '''
    line_re = re.compile(
        r'position=<\s*(?P<x>-?\d+),\s*(?P<y>-?\d+)> velocity=<\s*(?P<vx>-?\d+),\s*(?P<vy>-?\d+)>')
    points = []
    for line in puzzle.splitlines():
        match = line_re.match(line)
        if not match:
            raise ValueError('Bad input line: ' + line)
        points.append(Point(
            position=Vector(x=int(match['x']), y=int(match['y'])),
            velocity=Vector(x=int(match['vx']), y=int(match['vy']))))
    return points


def part1(puzzle: str) -> str:
    """ Solve part 1 """
    points = parse(puzzle)
    compact = run_until_compact(points)
    upper_left = Vector(
        x=min(compact, key=lambda point: point.position.x, default=0).position.x,
        y=min(compact, key=lambda point: point.position.y, default=0).position.y)
    bottom_right = Vector(
        x=max(compact, key=lambda point: point.position.x, default=0).position.x,
        y=max(compact, key=lambda point: point.position.y, default=0).position.y)
    # +1s here are so that range is inclusive
    width = bottom_right.x - upper_left.x + 1
    height = bottom_right.y - upper_left.y + 1
    grid = [['.'] * width for _ in range(height)]
    for point in compact:
        grid[point.position.y - upper_left.y][point.position.x - upper_left.x] = '#'
    return '\n' + '\n'.join(''.join(line) for line in grid)


def part2(puzzle: str) -> int:
    """ Solve part 2 """
    pass


def main():
    """ Run 2018 Day 10 """
    parser = argparse.ArgumentParser(description='Advent of Code 2018 Day 10')
    parser.add_argument('input', help='input file')
    opts = parser.parse_args()
    with open(opts.input) as handle:
        puzzle = handle.read().strip()
    print('Part 1:', part1(puzzle))
    print('Part 2:', part2(puzzle))


if __name__ == '__main__':
    main()


class ExampleTest(unittest.TestCase):
    example = """\
position=< 9,  1> velocity=< 0,  2>
position=< 7,  0> velocity=<-1,  0>
position=< 3, -2> velocity=<-1,  1>
position=< 6, 10> velocity=<-2, -1>
position=< 2, -4> velocity=< 2,  2>
position=<-6, 10> velocity=< 2, -2>
position=< 1,  8> velocity=< 1, -1>
position=< 1,  7> velocity=< 1,  0>
position=<-3, 11> velocity=< 1, -2>
position=< 7,  6> velocity=<-1, -1>
position=<-2,  3> velocity=< 1,  0>
position=<-4,  3> velocity=< 2,  0>
position=<10, -3> velocity=<-1,  1>
position=< 5, 11> velocity=< 1, -2>
position=< 4,  7> velocity=< 0, -1>
position=< 8, -2> velocity=< 0,  1>
position=<15,  0> velocity=<-2,  0>
position=< 1,  6> velocity=< 1,  0>
position=< 8,  9> velocity=< 0, -1>
position=< 3,  3> velocity=<-1,  1>
position=< 0,  5> velocity=< 0, -1>
position=<-2,  2> velocity=< 2,  0>
position=< 5, -2> velocity=< 1,  2>
position=< 1,  4> velocity=< 2,  1>
position=<-2,  7> velocity=< 2, -2>
position=< 3,  6> velocity=<-1, -1>
position=< 5,  0> velocity=< 1,  0>
position=<-6,  0> velocity=< 2,  0>
position=< 5,  9> velocity=< 1, -2>
position=<14,  7> velocity=<-2,  0>
position=<-3,  6> velocity=< 2, -1>
"""

    expected = """
#...#..###
#...#...#.
#...#...#.
#####...#.
#...#...#.
#...#...#.
#...#...#.
#...#..###\
"""

    def test_part1(self):
        self.assertEqual(part1(self.example), self.expected)
