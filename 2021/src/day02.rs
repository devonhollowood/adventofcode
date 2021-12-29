use anyhow::{anyhow, Result};
use chumsky::prelude::*;
use chumsky::text::*;
use nalgebra::{vector, Vector2};

type Vec2 = Vector2<i64>;

fn parser() -> impl Parser<char, Vec<Vec2>, Error = Simple<char>> {
    let direction = choice((
        just("up").to((|n: i64| vector![0, -n]) as fn(i64) -> Vec2),
        just("down").to((|n: i64| vector![0, n]) as fn(_) -> _),
        just("forward").to((|n: i64| vector![n, 0]) as fn(_) -> _),
    ));

    direction
        .then_ignore(whitespace())
        .then(int(10).map(|s: String| s.parse::<i64>().unwrap()))
        .map(|(f, n): (fn(i64) -> Vec2, i64)| f(n))
        .separated_by(just("\n"))
        .allow_trailing()
        .then_ignore(end())
}

/// parses input into list of offsets
/// right and down are positive directions
pub fn parse(input: &str) -> Result<Vec<Vector2<i64>>> {
    parser()
        .parse(input)
        .map_err(|errs| errs.into_iter().reduce(|acc, err| acc.merge(err)).unwrap())
        .map_err(|err| anyhow!("error parsing day 2 input: {:?}", err))
}

pub fn part1(input: &[Vector2<i64>]) -> Result<i64> {
    Ok(input.iter().sum::<Vector2<i64>>().iter().product())
}

pub fn part2(input: &[Vector2<i64>]) -> Result<String> {
    Ok("Not Implemented".into())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parser() {
        assert_eq!(
            parse("up 1\ndown 2\nforward 3\n").unwrap(),
            vec![
                vector![0, -1],
                vector![0, 2],
                vector![3, 0],
            ]
        )
    }

    #[test]
    fn test_part1() {
        let input = parse(
            r"forward 5
down 5
forward 8
up 3
down 8
forward 2",
        )
        .expect("failed to parse input");
        assert_eq!(part1(&input).unwrap(), 150)
    }
}
