extern crate structopt;
#[macro_use]
extern crate structopt_derive;

use structopt::StructOpt;
use std::collections::BTreeMap;
use std::fs::File;
use std::io::Read;

fn distribute(blocks: &mut [usize]) {
    if blocks.is_empty() {
        return;
    }
    let (mut idx, mut nblocks) = blocks
        .iter()
        .cloned()
        .enumerate()
        .rev() // need to reverse since max_by_key() returns the last element,
               // and we want the first
        .max_by_key(|&(_, val)| val)
        .expect("blocks should not have been empty");
    blocks[idx] = 0;
    idx = (idx + 1) % blocks.len();
    while nblocks > 0 {
        blocks[idx] += 1;
        nblocks -= 1;
        idx = (idx + 1) % blocks.len();
    }
}

fn cycle_detect(input: &str) -> (usize, usize) {
    let mut states = BTreeMap::new();
    let mut blocks = input
        .split_whitespace()
        .map(|s| s.parse().expect("Could not parse"))
        .collect::<Vec<usize>>();
    let mut count = 0;
    while !states.contains_key(&blocks) {
        states.insert(blocks.clone(), count);
        distribute(&mut blocks);
        count += 1;
    }
    (count, count - states[&blocks])
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
    let cycles = cycle_detect(&contents);
    println!("Part 1: {}", cycles.0);
    println!("Part 2: {}", cycles.1);
}

#[derive(StructOpt, Debug)]
#[structopt(name = "day06", about = "Advent of code 2017 day 06")]
struct Opt {
    #[structopt(help = "Input file")] input: String,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn distribute_test() {
        let mut input = [0, 2, 7, 0];
        distribute(&mut input);
        assert_eq!(input, [2, 4, 1, 2]);
        distribute(&mut input);
        assert_eq!(input, [3, 1, 2, 3]);
        distribute(&mut input);
        assert_eq!(input, [0, 2, 3, 4]);
        distribute(&mut input);
        assert_eq!(input, [1, 3, 4, 1]);
        distribute(&mut input);
        assert_eq!(input, [2, 4, 1, 2]);
    }

    #[test]
    fn cycle_detect_test() {
        assert_eq!(cycle_detect("0 2 7 0"), (5, 4));
    }
}
