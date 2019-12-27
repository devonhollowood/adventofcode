use std::collections::HashSet;
use std::path::PathBuf;
use structopt::StructOpt;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Position(isize, isize);

impl std::ops::Add<Offset> for Position {
    type Output = Position;

    fn add(self, rhs: Offset) -> Self::Output {
        let Position(x, y) = self;
        let Offset(dx, dy) = rhs;
        Position(x + dx, y + dy)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Offset(isize, isize);

impl std::ops::Sub for Position {
    type Output = Offset;

    fn sub(self, rhs: Self) -> Self::Output {
        let Position(x1, y1) = self;
        let Position(x2, y2) = rhs;
        Offset(x1 - x2, y1 - y2)
    }
}

fn between(a: Position, b: Position) -> Box<dyn Iterator<Item = Position>> {
    let Offset(dx, dy) = b - a;
    if dx == 0 {
        return Box::new((a.1.min(b.1) + 1..a.1.max(b.1)).map(move |y| Position(a.0, y)));
    }
    if dy == 0 {
        return Box::new((a.0.min(b.0) + 1..a.0.max(b.0)).map(move |x| Position(x, a.1)));
    }
    let gcd = num::Integer::gcd(&dx, &dy);
    Box::new((1..gcd).map(move |mult| a + Offset(dx / gcd * mult, dy / gcd * mult)))
}

fn visible_from(pos: Position, asteroids: &HashSet<Position>) -> usize {
    asteroids
        .iter()
        .copied()
        .filter(|asteroid| between(pos, *asteroid).all(|loc| !asteroids.contains(&loc)))
        .filter(|asteroid| *asteroid != pos)
        .count()
}

fn part1(asteroids: &HashSet<Position>) -> usize {
    asteroids
        .iter()
        .map(|pos| visible_from(*pos, asteroids))
        .max()
        .expect("Expected >0 asteroids")
}

fn parse(input: &str) -> HashSet<Position> {
    input
        .lines()
        .enumerate()
        .flat_map(|(y, line)| {
            line.trim()
                .chars()
                .enumerate()
                .filter_map(move |(x, chr)| match chr {
                    '#' => Some(Position(x as isize, y as isize)),
                    '.' => None,
                    _ => panic!("Unexpected character at {}, {}: {}", x, y, chr),
                })
        })
        .collect()
}

fn main() {
    let input =
        parse(&std::fs::read_to_string(Opt::from_args().input).expect("error reading file"));
    println!("Part 1: {}", part1(&input));
}

#[derive(StructOpt)]
struct Opt {
    #[structopt(parse(from_os_str))]
    input: PathBuf,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_between() {
        // change in x
        assert_eq!(
            between(Position(-2, 1), Position(2, 1)).collect::<HashSet<_>>(),
            (-1..=1).map(|x| Position(x, 1)).collect(),
        );
        // change in y
        assert_eq!(
            between(Position(1, 2), Position(1, -2)).collect::<HashSet<_>>(),
            (-1..=1).map(|y| Position(1, y)).collect(),
        );
        // no change
        assert_eq!(between(Position(0, 0), Position(0, 0)).count(), 0);
        // change in x and y
        assert_eq!(
            between(Position(0, 0), Position(-8, 12)).collect::<HashSet<_>>(),
            [Position(-2, 3), Position(-4, 6), Position(-6, 9)]
                .into_iter()
                .copied()
                .collect()
        );
    }

    #[test]
    fn test_example_1() {
        let examples = vec![
            (include_str!("../examples/1.txt"), Position(3, 4), 8),
            (include_str!("../examples/2.txt"), Position(5, 8), 33),
        ];
        for (input, best_pos, n_visible) in examples {
            let asteroids = parse(input);
            assert_eq!(
                asteroids
                    .iter()
                    .copied()
                    .max_by_key(|pos| { visible_from(*pos, &asteroids) }),
                Some(best_pos)
            );
            assert_eq!(part1(&asteroids), n_visible);
        }
    }
}
