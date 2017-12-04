extern crate itertools;
extern crate structopt;
#[macro_use]
extern crate structopt_derive;

use itertools::Itertools;
use structopt::StructOpt;
use std::fs::File;
use std::collections::BTreeSet;
use std::io::Read;

fn valid(passphrase: &str) -> bool {
    let mut used = BTreeSet::new();
    for word in passphrase.split_whitespace() {
        if used.contains(word) {
            return false;
        }
        used.insert(word);
    }
    true
}

fn valid2(passphrase: &str) -> bool {
    let mut used = BTreeSet::new();
    for word in passphrase.split_whitespace() {
        let canonical = word.chars().sorted();
        if used.contains(&canonical) {
            return false;
        }
        used.insert(canonical);
    }
    true
}

fn part1(input: &str) -> usize {
    input.lines().filter(|line| valid(line)).count()
}

fn part2(input: &str) -> usize {
    input.lines().filter(|line| valid2(line)).count()
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
#[structopt(name = "day04", about = "Advent of code 2017 day 04")]
struct Opt {
    #[structopt(help = "Input file")] input: String,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn valid_test() {
        assert_eq!(valid("aa bb cc dd ee"), true);
        assert_eq!(valid("aa bb cc dd aa"), false);
        assert_eq!(valid("aa bb cc dd aaa"), true);
    }

    #[test]
    fn valid2_test() {
        assert_eq!(valid2("abcde fghij"), true);
        assert_eq!(valid2("abcde xyz ecdab"), false);
        assert_eq!(valid2("a ab abc abd abf abj"), true);
        assert_eq!(valid2("iiii oiii ooii oooi oooo"), true);
        assert_eq!(valid2("oiii ioii iioi iiio"), false);
    }
}
