use anyhow::{anyhow, Result};
use chumsky::prelude::{Parser, Simple};
use chumsky::Error;
use nalgebra::{vector, Vector2};

use std::collections::HashMap;

type Vec2 = Vector2<i64>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Line {
    start: Vec2,
    end: Vec2,
}

impl Line {
    fn is_vertical(&self) -> bool {
        self.start.x == self.end.x
    }

    fn is_horizontal(&self) -> bool {
        self.start.y == self.end.y
    }

    fn vertical_points(&self) -> Vec<Vec2> {
        assert!(self.is_vertical());

        let (min, max) = if self.start.y <= self.end.y {
            (self.start, self.end)
        } else {
            (self.end, self.start)
        };

        (min.y..=max.y).map(|y| vector![self.start.x, y]).collect()
    }

    fn horizontal_points(&self) -> Vec<Vec2> {
        assert!(self.is_horizontal());

        let (min, max) = if self.start.x <= self.end.x {
            (self.start, self.end)
        } else {
            (self.end, self.start)
        };

        (min.x..=max.x).map(|x| vector![x, self.start.y]).collect()
    }

    fn diagonal_points(&self) -> Vec<Vec2> {
        let (left, right) = if self.start.x <= self.end.x {
            (self.start, self.end)
        } else {
            (self.end, self.start)
        };

        let mut result = Vec::new();
        result.reserve((right.x - left.x + 1) as usize);

        let sign = (right.y - left.y).signum();
        let mut y = left.y;
        for x in left.x..=right.x {
            result.push(vector![x, y]);
            y += sign;
        }
        result
    }
}

fn parser() -> impl Parser<char, Vec<Line>, Error = Simple<char>> {
    use chumsky::prelude::*;
    use chumsky::text::*;
    let point = int(10)
        .map(|i: String| i.parse::<i64>().unwrap())
        .then_ignore(just(","))
        .then(int(10).map(|i: String| i.parse::<i64>().unwrap()))
        .map(|(x, y)| vector![x, y]);

    point
        .then_ignore(just(" -> "))
        .then(point)
        .map(|(start, end)| Line { start, end })
        .separated_by(just("\n"))
        .allow_leading()
        .allow_trailing()
        .then_ignore(end())
}

pub fn parse(input: &str) -> Result<Vec<Line>> {
    parser()
        .parse(input)
        .map_err(|errs| errs.into_iter().reduce(|acc, err| acc.merge(err)).unwrap())
        .map_err(|err| anyhow!("error parsing day 5 input: {:?}", err))
}

pub fn part1(input: &[Line]) -> usize {
    let mut counter: HashMap<Vec2, usize> = HashMap::new();
    for point in input
        .iter()
        .filter_map(|l| {
            if l.is_vertical() {
                Some(l.vertical_points())
            } else if l.is_horizontal() {
                Some(l.horizontal_points())
            } else {
                None
            }
        })
        .flatten()
    {
        *counter.entry(point).or_default() += 1;
    }

    counter.values().filter(|v| **v >= 2).count()
}

pub fn part2(input: &[Line]) -> usize {
    let mut counter: HashMap<Vec2, usize> = HashMap::new();
    for point in input
        .iter()
        .map(|l| {
            if l.is_vertical() {
                l.vertical_points()
            } else if l.is_horizontal() {
                l.horizontal_points()
            } else {
                l.diagonal_points()
            }
        })
        .flatten()
    {
        *counter.entry(point).or_default() += 1;
    }

    counter.values().filter(|v| **v >= 2).count()
}

#[cfg(test)]
mod tests {
    use super::*;

    const INPUT: &str = r"
0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2";

    #[test]
    fn test_parse() {
        let input = parse(INPUT).unwrap();
        assert_eq!(input.len(), 10);
        assert_eq!(
            input[0],
            Line {
                start: vector![0, 9],
                end: vector![5, 9]
            }
        );
    }

    #[test]
    fn test_part1() {
        let input = parse(INPUT).unwrap();
        assert_eq!(part1(&input), 5);
    }

    #[test]
    fn test_part2() {
        let input = parse(INPUT).unwrap();
        assert_eq!(part2(&input), 12);
    }
}
