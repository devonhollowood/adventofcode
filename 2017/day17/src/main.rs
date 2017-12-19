extern crate num_bigint;
extern crate num_traits;

extern crate structopt;
#[macro_use]
extern crate structopt_derive;

use num_bigint::BigUint;
use num_traits::{pow, One, Zero};
use structopt::StructOpt;

fn part1(skip: usize) -> usize {
    let mut spinlock = vec![0];
    let mut current_pos = 1;
    for item in 1..2018 {
        current_pos = (current_pos + skip + 1) % spinlock.len();
        spinlock.insert(current_pos, item);
    }
    spinlock[(current_pos + 1) % spinlock.len()]
}

fn part2(skip: usize) -> usize {
    let mut len = 1;
    let mut current_pos = 1;
    let mut after_zero = 0;
    let mut item = 0;
    while item <= 50_000_000 {
        current_pos = (current_pos + skip + 1) % len;
        if current_pos == 0 {
            after_zero = item
        }
        len += 1; // as if we are inserting
        item += 1;
        let remaining = len - 1 - current_pos;
        let ignore = if remaining % (skip + 1) == 0 {
            (remaining / (skip + 1)).max(1) - 1
        } else {
            remaining / (skip + 1)
        };
        item += ignore;
        len += ignore;
        current_pos += ignore * (skip + 1);
    }
    after_zero
}

fn bonus() -> BigUint {
    let skip = 47usize;
    let mut len = BigUint::one();
    let mut current_pos = BigUint::one();
    let mut after_zero_sum = BigUint::zero();
    let mut item = BigUint::one();
    while item.clone() <= pow(BigUint::from(10u32), 100) {
        current_pos = (current_pos.clone() + skip + 1usize) % len.clone();
        if current_pos == BigUint::zero() {
            after_zero_sum += item.clone();
        }
        len += 1usize; // as if we are inserting
        item += 1usize;
        let remaining = len.clone() - 1usize - current_pos.clone();
        let ignore = if remaining < BigUint::from(skip + 1) {
            BigUint::zero()
        } else if remaining.clone() % (skip + 1) == BigUint::zero() {
            (remaining / (skip + 1)) - BigUint::one()
        } else {
            remaining / (skip + 1)
        };
        item += ignore.clone();
        len += ignore.clone();
        current_pos += ignore * (skip + 1);
    }
    after_zero_sum
}

fn main() {
    let opt = Opt::from_args();
    println!(
        "Part 1: {}",
        part1(opt.input.parse().expect("Could not read input"))
    );
    println!(
        "Part 2: {}",
        part2(opt.input.parse().expect("Could not read input"))
    );
    println!("bonus: {}", bonus());
}

#[derive(StructOpt, Debug)]
#[structopt(name = "day13", about = "Advent of code 2017 day 13")]
struct Opt {
    #[structopt(help = "Input")] input: String,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn part1_test() {
        assert_eq!(part1(3), 638);
    }
}
