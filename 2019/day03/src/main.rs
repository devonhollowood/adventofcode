use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;
use std::path::PathBuf;
use structopt::StructOpt;

type Position = (isize, isize);

#[derive(Debug, Clone, Copy)]
enum Direction {
    Right(isize),
    Up(isize),
}

#[derive(Debug, Clone, Copy)]
struct Segment {
    origin: Position,
    direction: Direction,
}

#[derive(Debug)]
struct Wire {
    directions: Vec<Direction>,
}

impl Wire {
    fn segments(&self) -> Vec<Segment> {
        use Direction::*;
        let mut current_loc = (0, 0);
        let mut segments = Vec::new();
        for dir in self.directions.iter() {
            segments.push(Segment {
                origin: current_loc,
                direction: *dir,
            });
            current_loc = match dir {
                Right(dx) => (current_loc.0 + dx, current_loc.1),
                Up(dy) => (current_loc.0, current_loc.1 + dy),
            }
        }
        segments
    }
}

#[derive(Debug)]
struct ParseError;

impl std::str::FromStr for Wire {
    type Err = ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use Direction::*;
        let mut directions = Vec::new();
        for segment in s.split(',') {
            let (dir, len) = segment.split_at(1);
            match (dir, len.parse::<isize>()) {
                ("R", Ok(len)) => directions.push(Right(len)),
                ("L", Ok(len)) => directions.push(Right(-len)),
                ("U", Ok(len)) => directions.push(Up(len)),
                ("D", Ok(len)) => directions.push(Up(-len)),
                _ => return Err(ParseError),
            }
        }
        Ok(Wire { directions })
    }
}

fn minmax(a: isize, b: isize) -> (isize, isize) {
    if a <= b {
        (a, b)
    } else {
        (b, a)
    }
}

fn intersection(a: Segment, b: Segment) -> Option<Position> {
    use Direction::*;
    match (a, b) {
        (
            Segment {
                origin: (ax, ay),
                direction: Right(dx),
            },
            Segment {
                origin: (bx, by),
                direction: Up(dy),
            },
        ) => {
            let ax_range = minmax(ax, ax + dx);
            let by_range = minmax(by, by + dy);
            if ax_range.0 < bx && bx < ax_range.1 && by_range.0 < ay && ay < by_range.1 {
                Some((bx, ay))
            } else {
                None
            }
        }
        (
            Segment {
                origin: (ax, ay),
                direction: Up(dy),
            },
            Segment {
                origin: (bx, by),
                direction: Right(dx),
            },
        ) => {
            let bx_range = minmax(bx, bx + dx);
            let ay_range = minmax(ay, ay + dy);
            if bx_range.0 < ax && ax < bx_range.1 && ay_range.0 < by && by < ay_range.1 {
                Some((ax, by))
            } else {
                None
            }
        }
        _ => None,
    }
}

fn manhattan_dist(a: &Position, b: &Position) -> isize {
    (a.0 - b.0).abs() + (a.1 - b.1).abs()
}

fn part1(first: &Wire, second: &Wire) -> isize {
    let first = first.segments();
    let second = second.segments();
    let closest = first
        .iter()
        .flat_map(|seg1| second.iter().map(|seg2| (seg1, seg2)).collect::<Vec<_>>())
        .filter_map(|(&a, &b)| intersection(a, b))
        .min_by_key(|pos| manhattan_dist(pos, &(0, 0)))
        .expect("No intersections found");
    manhattan_dist(&closest, &(0, 0))
}

fn main() {
    let handle = File::open(Opt::from_args().input).expect("error opening file");
    let input: Vec<_> = BufReader::new(handle)
        .lines()
        .map(|line| line.expect("Could not read input"))
        .map(|line| line.parse::<Wire>().expect("Could not parse wire"))
        .collect();
    assert_eq!(input.len(), 2);
    let first = input.get(0).unwrap();
    let second = input.get(1).unwrap();
    println!("Part 1: {}", part1(first, second));
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
    fn test_part1() {
        let expected = vec![
            (("R8,U5,L5,D3", "U7,R6,D4,L4"), 6),
            (
                (
                    "R75,D30,R83,U83,L12,D49,R71,U7,L72",
                    "U62,R66,U55,R34,D71,R55,D58,R83",
                ),
                159,
            ),
            (
                (
                    "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51",
                    "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7",
                ),
                135,
            ),
        ];
        for (input, output) in expected {
            assert_eq!(
                part1(&input.0.parse().unwrap(), &input.1.parse().unwrap()),
                output
            );
        }
    }
}
