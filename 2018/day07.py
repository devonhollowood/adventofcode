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
    requirements: typing.Mapping[str, typing.List[str]] = defaultdict(list)
    reverse_reqs: typing.Mapping[str, typing.List[str]] = defaultdict(list)
    for (step, requirement) in parse(puzzle):
        requirements[step].append(requirement)
        reverse_reqs[requirement].append(step)
    steps = set(requirements.keys()) | set(reverse_reqs.keys())
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
    if complete != steps:
        raise AssertionError('Never completed ' + str(complete - steps))
    return final_order


def part2(puzzle: str, nworkers=5, base_time: int = 60) -> int:
    """ Solve part 2 """

    def step_time(step: str) -> int:
        return base_time + ord(step[0]) - ord('A') + 1

    assert nworkers > 0
    requirements: typing.Mapping[str, typing.List[str]] = defaultdict(list)
    reverse_reqs: typing.Mapping[str, typing.List[str]] = defaultdict(list)
    for (step, requirement) in parse(puzzle):
        requirements[step].append(requirement)
        reverse_reqs[requirement].append(step)
    all_tasks = set(requirements.keys()) | set(reverse_reqs.keys())
    available_tasks = [task for task in all_tasks if not requirements[task]]
    heapq.heapify(available_tasks)
    queued_tasks: typing.List[typing.Tuple[int, str]] = []  # heap of (finish time, task) pairs
    completed_tasks: typing.Set[str] = set()
    current_time = 0
    while available_tasks or queued_tasks:
        # queue up as many available tasks as possible
        # postcondition: have at least one queued task, since we started with some available
        while available_tasks and len(queued_tasks) < nworkers:
            todo = heapq.heappop(available_tasks)
            heapq.heappush(queued_tasks, (current_time + step_time(todo), todo))

        # pop off one of the queued tasks
        assert queued_tasks
        current_time, completed = heapq.heappop(queued_tasks)
        completed_tasks.add(completed)
        # add newly available tasks
        for rev_req in reverse_reqs[completed]:
            if all(r in completed_tasks for r in requirements[rev_req]):
                heapq.heappush(available_tasks, rev_req)

    if completed_tasks != all_tasks:
        raise AssertionError('Never completed ' + str(all_tasks - completed_tasks))
    return current_time


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


class ExampleTest(unittest.TestCase):
    example = (
        'Step C must be finished before step A can begin.\n'
        'Step C must be finished before step F can begin.\n'
        'Step A must be finished before step B can begin.\n'
        'Step A must be finished before step D can begin.\n'
        'Step B must be finished before step E can begin.\n'
        'Step D must be finished before step E can begin.\n'
        'Step F must be finished before step E can begin.\n'
    )

    def test_part1(self):
        self.assertEqual(part1(self.example), 'CABDFE')

    def test_part2(self):
        self.assertEqual(part2(self.example, nworkers=2, base_time=0), 15)
