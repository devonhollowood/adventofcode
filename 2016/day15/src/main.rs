extern crate clap;
extern crate regex;

#[macro_use]
extern crate lazy_static;

use std::io::prelude::*;
use std::fs::File;
use regex::Regex;

type Time = usize;
type Position = usize;

#[derive(Debug)]
struct Disk {
    n_positions: usize,
    initial: Position,
}

impl Disk {
    fn at(&self, t: Time) -> Position {
        (t + self.initial) % self.n_positions
    }
}

impl std::str::FromStr for Disk {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        lazy_static! {
            static ref DISK_RE: Regex =
                Regex::new(r"Disc #\d+ has (\d+) positions; at time=0, it is at position (\d+).").unwrap();
        }
        if let Some(cap) = DISK_RE.captures(s) {
            Ok(Disk {
                n_positions: cap[1].parse().expect("bad regex"),
                initial: cap[2].parse().expect("bad regex"),
            })
        } else {
            Err("invalid disk".into())
        }
    }
}

fn gcd(mut a: usize, mut b: usize) -> usize {
    while b != 0 {
        let temp = b;
        b = a % b;
        a = temp;
    }
    a
}

fn lcm(a: usize, b: usize) -> usize {
    a * b / gcd(a, b)
}

fn solve(disks: &[Disk]) -> Time {
    if disks.is_empty() {
        return 0;
    }
    let mut current_period = disks[0].n_positions;
    let mut time = disks[0].n_positions - disks[0].initial - 1;
    for (disk_num, disk) in disks.iter().enumerate().map(|(idx, disk)| (idx + 1, disk)) {
        while disk.at(time + disk_num) != 0 {
            time += current_period;
        }
        current_period = lcm(current_period, disk.n_positions);
    }
    time
}

fn parse_args() -> std::io::Result<String> {
    let matches = clap::App::new("Day 15")
        .author("Devon Hollowood")
        .arg(clap::Arg::with_name("disks")
            .index(1)
            .short("f")
            .long("disks")
            .help("file containing disks for puzzle. Reads from stdin otherwise")
            .takes_value(true))
        .get_matches();
    let source = matches.value_of_os("disks");
    let mut contents = String::new();
    match source {
        Some(filename) => {
            let mut input = File::open(filename)?;
            input.read_to_string(&mut contents)?;
        }
        None => {
            let mut input = std::io::stdin();
            input.read_to_string(&mut contents)?;
        }
    }
    Ok(contents)
}

fn main() {
    let input = parse_args().unwrap_or_else(|err| panic!("Error reading args: {}", err));
    let disks: Vec<Disk> = input.lines()
        .map(|line| line.trim().parse())
        .collect::<Result<_, _>>()
        .unwrap_or_else(|err| panic!("Error parsing disks: {}", err));
    println!("part 1 solution: drop at t = {} seconds", solve(&disks))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn example() {
        let disks = [Disk {
                         n_positions: 5,
                         initial: 4,
                     },
                     Disk {
                         n_positions: 2,
                         initial: 1,
                     }];
        assert_eq!(solve(&disks), 5);
    }
}
