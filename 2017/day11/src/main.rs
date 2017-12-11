extern crate structopt;
#[macro_use]
extern crate structopt_derive;

use structopt::StructOpt;
use std::fs::File;
use std::io::Read;

#[derive(Debug, Clone)]
struct Coordinate {
    x: isize,
    y: isize,
    z: isize,
}

impl Coordinate {
    fn origin() -> Coordinate {
        Coordinate { x: 0, y: 0, z: 0 }
    }
    fn travel(&self, dir: &Direction) -> Coordinate {
        use Direction::*;
        match *dir {
            N => Coordinate {
                x: self.x,
                y: self.y + 1,
                z: self.z - 1,
            },
            Ne => Coordinate {
                x: self.x + 1,
                y: self.y,
                z: self.z - 1,
            },
            Se => Coordinate {
                x: self.x + 1,
                y: self.y - 1,
                z: self.z,
            },
            S => Coordinate {
                x: self.x,
                y: self.y - 1,
                z: self.z + 1,
            },
            Sw => Coordinate {
                x: self.x - 1,
                y: self.y,
                z: self.z + 1,
            },
            Nw => Coordinate {
                x: self.x - 1,
                y: self.y + 1,
                z: self.z,
            },
        }
    }
    fn distance(&self, other: &Coordinate) -> isize {
        ((self.x - other.x).abs() + (self.y - other.y).abs()
            + (self.z - other.z).abs()) / 2
    }
}

#[derive(Debug)]
enum Direction {
    N,
    Ne,
    Se,
    S,
    Sw,
    Nw,
}

#[derive(Debug)]
struct DirectionParseError;

impl std::str::FromStr for Direction {
    type Err = DirectionParseError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use Direction::*;
        match s {
            "n" => Ok(N),
            "ne" => Ok(Ne),
            "se" => Ok(Se),
            "s" => Ok(S),
            "sw" => Ok(Sw),
            "nw" => Ok(Nw),
            _ => Err(DirectionParseError),
        }
    }
}

// solve parts 1 and 2
fn solve(input: &str) -> (isize, isize) {
    let (end, max_dist) = input
        .trim()
        .split(',')
        .map(|s| {
            s.parse::<Direction>()
                .expect(&format!("could not parse direction \"{}\"", s))
        })
        .fold((Coordinate::origin(), 0), |(loc, max_dist), dir| {
            let new_loc = loc.travel(&dir);
            let new_dist = new_loc.distance(&Coordinate::origin());
            (new_loc, max_dist.max(new_dist))
        });
    (end.distance(&Coordinate::origin()), max_dist)
}

fn main() {
    let opt = Opt::from_args();
    let mut contents = String::new();
    if opt.input == "-" {
        std::io::stdin()
            .read_to_string(&mut contents)
            .expect("could not read stdin");
    } else {
        let mut file = File::open(&opt.input)
            .expect(&format!("file {} not found", opt.input));
        file.read_to_string(&mut contents)
            .expect(&format!("could not read file {}", opt.input));
    }
    let solution = solve(&contents);
    println!("Part 1: {}", solution.0);
    println!("Part 2: {}", solution.1);
}

#[derive(StructOpt, Debug)]
#[structopt(name = "day10", about = "Advent of code 2017 day 10")]
struct Opt {
    #[structopt(help = "Input file")] input: String,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn part1_test() {
        assert_eq!(solve("ne,ne,ne").0, 3);
        assert_eq!(solve("ne,ne,sw,sw").0, 0);
        assert_eq!(solve("ne,ne,s,s").0, 2);
        assert_eq!(solve("se,sw,se,sw,sw").0, 3);
    }
}
