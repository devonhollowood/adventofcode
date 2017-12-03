extern crate itertools;

extern crate structopt;
#[macro_use]
extern crate structopt_derive;

use itertools::Itertools;
use structopt::StructOpt;
use std::fs::File;
use std::io::Read;

fn part1(input: &str) -> u64 {
    input
        .lines()
        .map(|line| {
            let (min, max) = line.split_whitespace()
                .filter_map(|entry| entry.parse::<u64>().ok())
                .minmax()
                .into_option()
                .expect(&format!("Could not get min / max for {}", line));
            max - min
        })
        .sum()
}

fn part2(input: &str) -> u64 {
    input
        .lines()
        .map(|line| {
            let values: Vec<_> = line.split_whitespace()
                .filter_map(|entry| entry.parse::<u64>().ok())
                .collect();
            let mut result = None;
            'outer: for a in &values {
                for b in &values {
                    if a != b && a % b == 0 {
                        result = Some(a / b);
                        break 'outer;
                    }
                }
            }
            result
        })
        .filter_map(|x| x)
        .sum()
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
    println!("Part 1: {}", part1(&contents));
    println!("Part 2: {}", part2(&contents));
}

#[derive(StructOpt, Debug)]
#[structopt(name = "day02", about = "Advent of code 2017 day 02")]
struct Opt {
    #[structopt(help = "Input file")] input: String,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn part1_test() {
        assert_eq!(part1("5 1 9 5\n7 5 3\n2 4 6 8"), 18)
    }

    #[test]
    fn part2_test() {
        assert_eq!(part2("5 9 2 8\n9 4 7 3\n3 8 6 5"), 9)
    }
}
