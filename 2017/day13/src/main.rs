extern crate structopt;
#[macro_use]
extern crate structopt_derive;

use structopt::StructOpt;
use std::collections::HashMap;
use std::fs::File;
use std::io::Read;
use std::path::PathBuf;

type Node = usize;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Direction {
    Up,
    Down,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Scanner {
    location: usize,
    range: usize,
    direction: Direction,
}

impl Scanner {
    fn advance(&mut self) {
        use Direction::*;
        if self.range == 0 {
            return;
        }
        if self.location == 0 {
            self.direction = Down;
        } else if self.location == self.range - 1 {
            self.direction = Up;
        }
        match self.direction {
            Up => self.location -= 1,
            Down => self.location += 1,
        }
    }
}

#[derive(Debug, Clone)]
struct Firewall {
    scanners: HashMap<Node, Scanner>,
    depth: usize,
}

impl Firewall {
    fn advance(&mut self) {
        for scanner in self.scanners.values_mut() {
            scanner.advance();
        }
    }
}

impl std::str::FromStr for Firewall {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut max_depth = 0;
        let mut scanners = HashMap::new();
        for line in s.lines() {
            let mut split = line.split(':');
            match (split.next(), split.next()) {
                (Some(depth_str), Some(range_str)) => {
                    let depth = depth_str
                        .trim()
                        .parse()
                        .map_err(|e| format!("{:?}", e))?;
                    let range = range_str
                        .trim()
                        .parse()
                        .map_err(|e| format!("{:?}", e))?;
                    if depth > max_depth {
                        max_depth = depth;
                    }
                    scanners.insert(
                        depth,
                        Scanner {
                            location: 0,
                            range: range,
                            direction: Direction::Down,
                        },
                    );
                }
                _ => Err(format!("Could not parse: {}", line))?,
            }
        }
        Ok(Firewall {
            scanners: scanners,
            depth: max_depth,
        })
    }
}

fn part1(firewall: &mut Firewall) -> usize {
    let mut severity = 0;
    for depth in 0..firewall.depth + 1 {
        {
            if let Some(scanner) = firewall.scanners.get(&depth) {
                if scanner.location == 0 {
                    severity += depth * scanner.range;
                }
            }
        }
        firewall.advance();
    }
    severity
}

fn main() {
    let opt = Opt::from_args();
    let mut contents = String::new();
    if opt.input.to_str() == Some("-") {
        std::io::stdin()
            .read_to_string(&mut contents)
            .expect("could not read stdin");
    } else {
        let mut file = File::open(&opt.input)
            .expect(&format!("file {} not found", opt.input.display()));
        file.read_to_string(&mut contents)
            .expect(&format!("could not read file {}", opt.input.display()));
    }
    let firewall: Firewall = contents.parse().expect("Error parsing input");
    println!("Part 1: {}", part1(&mut firewall.clone()));
}

#[derive(StructOpt, Debug)]
#[structopt(name = "day13", about = "Advent of code 2017 day 13")]
struct Opt {
    #[structopt(help = "Input file", parse(from_os_str))] input: PathBuf,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn part1_test() {
        let mut firewall = "0: 3\n1: 2\n4: 4\n6: 4".parse().unwrap();
        assert_eq!(part1(&mut firewall), 24);
    }
}
