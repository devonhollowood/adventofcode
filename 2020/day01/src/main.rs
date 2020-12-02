use std::collections::BTreeSet;
use std::io::prelude::*;
use std::io::BufReader;
use std::iter::FromIterator;
use std::num::ParseIntError;
use std::path::PathBuf;
use structopt::StructOpt;

fn part1(input: &[i64]) -> Option<i64> {
    let numbers = BTreeSet::from_iter(input);
    for number in numbers.iter().cloned() {
        if numbers.contains(&(2020 - number)) {
            return Some(number * (2020 - number));
        }
    }
    None
}

fn part2(input: &[i64]) -> Option<i64> {
    let numbers = BTreeSet::from_iter(input);
    for a in numbers.iter().cloned() {
        for b in numbers.iter().cloned().filter(|&n| a + n <= 2020) {
            if numbers.contains(&(2020 - a - b)) {
                return Some(a * b * (2020 - a - b));
            }
        }
    }
    None
}

fn read_input(raw_input: &str) -> Result<Vec<i64>, ParseIntError> {
    raw_input.lines().map(|line| line.parse::<i64>()).collect()
}

fn main() {
    let opt = Opt::from_args();
    let file = std::fs::File::open(opt.input).expect("Could not open file");
    let mut buf_reader = BufReader::new(file);
    let mut contents = String::new();
    buf_reader
        .read_to_string(&mut contents)
        .expect("Could not read file to string");
    let input = read_input(&contents).expect("Error reading input");

    println!("Part 1: {}", part1(&input).expect("Solution not found"));
    println!("Part 2: {}", part2(&input).expect("Solution not found"));
}

#[derive(Debug, StructOpt)]
#[structopt(name = "day01", about = "Advent of Code 2020 Day 01")]
struct Opt {
    #[structopt(parse(from_os_str))]
    input: PathBuf,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        let input = vec![1721, 979, 366, 299, 675, 1456];
        assert_eq!(part1(&input), Some(514579));
    }

    #[test]
    fn test_part2() {
        let input = vec![1721, 979, 366, 299, 675, 1456];
        assert_eq!(part2(&input), Some(241861950));
    }
}
