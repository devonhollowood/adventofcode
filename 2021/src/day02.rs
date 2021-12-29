use anyhow::{anyhow, Result};
use chumsky::prelude::*;
use chumsky::text::*;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Direction {
    Up(i64),
    Down(i64),
    Forward(i64),
}

fn parser() -> impl Parser<char, Vec<Direction>, Error = Simple<char>> {
    let direction = choice((
        keyword("up").to(Direction::Up as fn(i64) -> Direction),
        keyword("down").to(Direction::Down as fn(i64) -> Direction),
        keyword("forward").to(Direction::Forward as fn(i64) -> Direction),
    ));

    direction
        .then_ignore(whitespace())
        .then(int(10).map(|s: String| s.parse::<i64>().unwrap()))
        .map(|(f, n): (fn(i64) -> Direction, i64)| f(n))
        .separated_by(just("\n"))
        .allow_trailing()
        .then_ignore(end())
}

/// parses input into list of offsets
/// right and down are positive directions
pub fn parse(input: &str) -> Result<Vec<Direction>> {
    parser()
        .parse(input)
        .map_err(|errs| errs.into_iter().reduce(|acc, err| acc.merge(err)).unwrap())
        .map_err(|err| anyhow!("error parsing day 2 input: {:?}", err))
}

pub fn part1(input: &[Direction]) -> Result<i64> {
    let mut pos = (0, 0);
    for dir in input.iter() {
        match dir {
            Direction::Up(y) => pos.1 -= y,
            Direction::Down(y) => pos.1 += y,
            Direction::Forward(x) => pos.0 += x,
        }
    }

    Ok(pos.0 * pos.1)
}

pub fn part2(input: &[Direction]) -> Result<i64> {
    let mut aim = 0;
    let mut pos = (0, 0);
    for dir in input.iter() {
        match dir {
            Direction::Up(y) => aim -= y,
            Direction::Down(y) => aim += y,
            Direction::Forward(x) => {
                pos.0 += x;
                pos.1 += x * aim;
            }
        }
    }

    Ok(pos.0 * pos.1)
}

#[cfg(test)]
mod tests {
    use super::*;

    const INPUT : &str = 
            r"forward 5
down 5
forward 8
up 3
down 8
forward 2";

    #[test]
    fn test_parser() {
        use Direction::*;
        assert_eq!(
            parse("up 1\ndown 2\nforward 3\n").unwrap(),
            vec![
                Up(1), Down(2), Forward(3)
            ]
        )
    }

    #[test]
    fn test_part1() {
        let input = parse(
            INPUT
        )
        .expect("failed to parse input");
        assert_eq!(part1(&input).unwrap(), 150)
    }

    #[test]
    fn test_part2() {
        let input = parse(
            INPUT
        )
        .expect("failed to parse input");
        assert_eq!(part2(&input).unwrap(), 900)
    }
}
