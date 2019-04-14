#!/usr/bin/env python3
""" 2018 AOC Day 4 """

import argparse
from collections import defaultdict
from dataclasses import dataclass
import datetime
import re
from typing import List


@dataclass
class sleep(object):
    start: datetime.datetime
    end: datetime.datetime


@dataclass
class shift(object):
    start: datetime.datetime
    guard: int
    sleeps: List[sleep]


def parse(puzzle: List[str]) -> List[shift]:
    """ Parse puzzle and return (unordered) mapping of date -> shift """
    action_re = re.compile(
        r'\[(?P<date>\d+-\d+-\d+ \d+:\d+)\] '
        r'(?P<action>Guard #(?P<guard>\d+) begins shift|falls asleep|wakes up)'
    )
    # Create list of parsed actions
    parsed_actions = []
    for line in puzzle:
        match = action_re.match(line)
        if not match:
            raise ValueError("Could not parse line: " + line)
        dt = datetime.datetime.strptime(match['date'], '%Y-%m-%d %H:%M')
        parsed_actions.append((dt, match))
    # iterate through actions and create ordered list of shifts
    # better performance if we pop off the back
    parsed_actions = sorted(parsed_actions, reverse=True)
    guard = None
    shift_start = None
    sleeps: List[sleep] = []
    sleep_start = None
    shifts = []
    if len(parsed_actions) == 0 or not parsed_actions[-1][1][0].endswith('begins shift'):
        raise ValueError(f'No initial guard')
    while parsed_actions:
        dt, action_match = parsed_actions.pop()
        action = action_match['action']
        if action.endswith('begins shift'):  # new guard
            if guard is not None and shift_start:  # save previous guard info
                shifts.append(
                    shift(guard=guard, sleeps=sleeps, start=shift_start))
            guard = int(action_match['guard'])
            shift_start = dt
            sleeps = []
        elif action == 'falls asleep':
            if sleep_start is not None:
                raise ValueError(
                    f'{dt}: sleeping guard {guard} cannot take action: {action}')
            sleep_start = dt
        elif action == 'wakes up':
            if sleep_start is None:
                raise ValueError(
                    f'{dt}: awake guard {guard} cannot take action: {action}')
            sleeps.append(sleep(start=sleep_start, end=dt))
            sleep_start = None
        else:
            raise AssertionError('Bad action: ' + action)
    # make sure last guard woke up
    if sleep_start:
        raise ValueError(f'guard {guard} on date {dt.date()} never wakes up')
    # save last action
    assert guard is not None
    assert shift_start is not None
    shifts.append(shift(guard=guard, sleeps=sleeps, start=shift_start))
    return shifts


def part1(puzzle):
    """ Solve part 1 """
    shifts = parse(puzzle)
    sleeptime = defaultdict(lambda: 0)
    time_asleep_at = defaultdict(lambda: defaultdict(lambda: 0))
    for shift in shifts:
        for sleep in shift.sleeps:
            time = sleep.start
            while time < sleep.end:
                sleeptime[shift.guard] += 1
                time_asleep_at[shift.guard][time.minute] += 1
                time += datetime.timedelta(minutes=1)
    sleepiest = max(sleeptime, key=sleeptime.__getitem__)
    time_most_often_asleep = max(
        time_asleep_at[sleepiest], key=time_asleep_at[sleepiest].__getitem__)
    output = (f'Guard {sleepiest} slept the most, at {sleeptime[sleepiest]} minutes.\n'
              f'  They were asleep most often at minute {time_most_often_asleep}.\n'
              f'  Product = {sleepiest * time_most_often_asleep}.')
    return output


def part2(puzzle):
    """ Solve part 2 """
    pass


def main():
    """ Run 2018 Day 4 """
    parser = argparse.ArgumentParser(description='Advent of Code 2018 Day 02')
    parser.add_argument('input', help='input file')
    opts = parser.parse_args()
    with open(opts.input) as handle:
        puzzle = [line.strip() for line in handle.readlines()]
    print('Part 1:', part1(puzzle))
    print('Part 2:', part2(puzzle))


if __name__ == '__main__':
    main()
