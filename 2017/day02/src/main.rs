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
}

#[derive(StructOpt, Debug)]
#[structopt(name = "day01", about = "Advent of code 2017 Day 01")]
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
}
