extern crate itertools;

extern crate structopt;
#[macro_use]
extern crate structopt_derive;

use itertools::Itertools;
use structopt::StructOpt;
use std::fs::File;
use std::io::Read;

fn part1(input: &str) -> u32 {
    input
        .chars()
        .chain(input.chars().next())
        .filter_map(|c| c.to_digit(10))
        .tuple_windows()
        .map(|(x, y)| if x == y { x } else { 0 })
        .sum()
}

fn part2(input: &str) -> u32 {
    let size = input.chars().filter_map(|c| c.to_digit(10)).count();
    let rotated_iter = input.chars().filter_map(|c| c.to_digit(10)).cycle();
    input
        .chars()
        .chain(input.chars().next())
        .filter_map(|c| c.to_digit(10))
        .tuple_windows()
        .map(|(x, y)| if x == y { x } else { 0 })
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
#[structopt(name = "day01", about = "Advent of code 2017 Day 01")]
struct Opt {
    #[structopt(help = "Input file")] input: String,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn part1_test() {
        assert_eq!(part1("1122"), 3);
        assert_eq!(part1("1111"), 4);
        assert_eq!(part1("1234"), 0);
        assert_eq!(part1("91212129"), 9);
    }
}
