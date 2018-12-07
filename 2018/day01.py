#!/usr/bin/env python3

import argparse
import itertools


def part1(puzzle):
    return sum(int(line) for line in puzzle)


def part2(puzzle):
    freq = 0
    freqs = {freq}
    for delta in itertools.cycle(int(line) for line in puzzle):
        freq += delta
        if freq in freqs:
            return freq
        else:
            freqs.add(freq)


def main():
    parser = argparse.ArgumentParser(description='Advent of Code 2018 Day 01')
    parser.add_argument('input', help='input file')
    opts = parser.parse_args()
    with open(opts.input) as handle:
        puzzle = handle.readlines()
    print('Part 1:', part1(puzzle))
    print('Part 2:', part2(puzzle))


if __name__ == '__main__':
    main()
