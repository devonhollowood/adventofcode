extern crate structopt;
#[macro_use]
extern crate structopt_derive;

use structopt::StructOpt;
use std::fs::File;
use std::io::Read;

fn part1(input: &str) -> u64 {
    let mut jumps = input
        .lines()
        .map(|line| line.parse::<i64>().expect("Could not parse line"))
        .collect::<Vec<_>>();
    let mut idx: i64 = 0;
    let mut count = 0;
    while idx >= 0 && (idx as usize) < jumps.len() {
        let diff = jumps[idx as usize];
        jumps[idx as usize] += 1;
        idx += diff;
        count += 1;
    }
    count
}

fn part2(input: &str) -> u64 {
    let mut jumps = input
        .lines()
        .map(|line| line.parse::<i64>().expect("Could not parse line"))
        .collect::<Vec<_>>();
    let mut idx: i64 = 0;
    let mut count = 0;
    while idx >= 0 && (idx as usize) < jumps.len() {
        let diff = jumps[idx as usize];
        if diff >= 3 {
            jumps[idx as usize] -= 1;
        } else {
            jumps[idx as usize] += 1;
        }
        idx += diff;
        count += 1;
    }
    count
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
#[structopt(name = "day05", about = "Advent of code 2017 day 05")]
struct Opt {
    #[structopt(help = "Input file")] input: String,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn part1_test() {
        assert_eq!(part1("0\n3\n0\n1\n-3"), 5)
    }

    #[test]
    fn part2_test() {
        assert_eq!(part2("0\n3\n0\n1\n-3"), 10)
    }
}
