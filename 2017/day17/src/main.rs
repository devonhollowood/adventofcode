extern crate structopt;
#[macro_use]
extern crate structopt_derive;

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
