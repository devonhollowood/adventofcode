#!/usr/bin/env python3

import argparse
import collections
import itertools


def part1(puzzle):
    doublets = 0
    triplets = 0
    for line in puzzle:
        counts = dict(collections.Counter(line)).values()
        if 2 in counts:
            doublets += 1
        if 3 in counts:
            triplets += 1
    return doublets * triplets


def part2(puzzle):
    for (box1, box2) in itertools.combinations(puzzle, 2):
        common = ''
        diffs = 0
        for (a, b) in zip(box1, box2):
            if a == b:
                common += a
            else:
                diffs += 1
            if diffs > 1:
                break
        else:  # successfully completed for loop, we have a match
            return common
    return None


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
