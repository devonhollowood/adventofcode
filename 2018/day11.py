#!/usr/bin/env python3
""" 2018 AOC Day 11 """

import argparse
from functools import lru_cache
import typing
import unittest


@lru_cache()
def power(x: int, y: int, serial: int) -> int:
    ''' Return power of cell at (x, y) in grid with given serial number '''
    rack_id = x + 10
    power_level = rack_id * y
    power_level += serial
    power_level *= rack_id
    power_level = power_level % 1000 // 100
    power_level -= 5
    return power_level


def square_power(x: int, y: int, serial: int, size: int = 3) -> int:
    ''' total power of 3x3 square with (x, y) as upper-left corner '''
    return sum(power(i, j, serial) for i in range(x, x + size) for j in range(y, y + size))


def part1(serial: int) -> typing.Tuple[int, int]:
    """ Solve part 1 """
    return max(
        ((x, y) for x in range(1, 299) for y in range(1, 299)),
        key=lambda p: square_power(p[0], p[1], serial),
    )


def part2(serial: int) -> int:
    """ Solve part 2 """
    pass


def main():
    """ Run 2018 Day 11 """
    parser = argparse.ArgumentParser(description='Advent of Code 2018 Day 11')
    parser.add_argument('serial', type=int, help='grid serial number')
    opts = parser.parse_args()
    print('Part 1:', part1(opts.serial))
    print('Part 2:', part2(opts.serial))


if __name__ == '__main__':
    main()


class ExampleTest(unittest.TestCase):
    def test_power_level(self):
        self.assertEqual(power(x=3, y=5, serial=8), 4)
        self.assertEqual(power(x=122, y=79, serial=57), -5)
        self.assertEqual(power(x=217, y=196, serial=39), 0)
        self.assertEqual(power(x=101, y=153, serial=71), 4)

    def test_sq_pow(self):
        examples = {
            (33, 45, 18, 3): 29,
            (21, 61, 42, 3): 30,
            (90, 269, 18, 16): 113,
            (232, 251, 42, 12): 119,
        }
        for example, expected in examples.items():
            self.assertEqual(square_power(*example), expected)

    def test_part1(self):
        examples = {
            18: (33, 45),
            42: (21, 61),
        }
        for example, expected in examples.items():
            self.assertEqual(part1(example), expected)
