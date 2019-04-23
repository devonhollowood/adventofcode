#!/usr/bin/env python3
""" 2018 AOC Day 09 """

import argparse
import typing
import unittest


class Node(object):
    ''' Class representing node in cyclic linked list '''

    def __init__(self, prev: 'Node', next: 'Node', value: int):
        ''' Create a node with explicit parameters '''
        self._prev = prev
        self._next = next
        self._value = value

    @staticmethod
    def default() -> 'Node':
        ''' Create a node linked to itself with value 0 '''
        node = Node(None, None, 0)  # type: ignore
        node._prev = node
        node._next = node
        return node

    def forward(self, n: int = 1) -> 'Node':
        ''' Go forward n nodes '''
        current = self
        for _ in range(n):
            current = current._next
        return current

    def back(self, n: int = 1) -> 'Node':
        ''' Go backward n nodes '''
        current = self
        for _ in range(n):
            current = current._prev
        return current

    def insert(self, value: int) -> 'Node':
        ''' Insert new node after current node with given value, and return newly inserted Node '''
        new_node = Node(self, self._next, value)
        self._next._prev = new_node
        self._next = new_node
        return self._next

    def remove(self) -> 'Node':
        ''' Remove current Node and return the following Node '''
        self._prev._next = self._next
        self._next._prev = self._prev
        return self._next

    def value(self) -> int:
        ''' Get value '''
        return self._value

    def chain_values(self):
        values = [self.value()]
        current = self.forward()
        while current != self:
            values.append(current.value())
            current = current.forward()
        return values


def part1(nplayers: int, highest_marble: int) -> int:
    """ Solve part 1 """
    current = Node.default()
    player = 0
    scores = {p: 0 for p in range(nplayers)}
    for idx in range(1, highest_marble + 1):
        if idx % 23 == 0:
            scores[player] += idx
            current = current.back(7)
            scores[player] += current.value()
            current = current.remove()
        else:
            current = current.forward().insert(idx)
        player = (player + 1) % nplayers
    return max(scores.values())


def part2(nplayers: int, highest_node: int) -> int:
    """ Solve part 2 """
    return part1(nplayers, highest_node)


def main():
    """ Run 2018 Day 09 """
    parser = argparse.ArgumentParser(description='Advent of Code 2018 Day 09')
    parser.add_argument('nplayers', type=int, help='# of players')
    parser.add_argument(
        'highest_marble',
        type=int,
        help='highest-valued marble',
    )
    opts = parser.parse_args()
    print('Part 1:', part1(opts.nplayers, opts.highest_marble))
    print('Part 2:', part2(opts.nplayers, opts.highest_marble * 100))


if __name__ == '__main__':
    main()


class ExampleTest(unittest.TestCase):
    def test_part1(self):
        examples = {
            (9, 25): 32,
            (10, 1618): 8317,
            (13, 7999): 146373,
            (17, 1104): 2764,
            (21, 6111): 54718,
            (30, 5807): 37305,
        }
        for example, expected in examples.items():
            self.assertEqual(part1(*example), expected)
