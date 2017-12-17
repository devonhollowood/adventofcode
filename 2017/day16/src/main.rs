extern crate structopt;
#[macro_use]
extern crate structopt_derive;

use structopt::StructOpt;
use std::fs::File;
use std::io::Read;
use std::path::PathBuf;

type Position = usize;
type Name = char;

enum Operation {
    Spin(Position),
    Exchange(Position, Position),
    Partner(Name, Name),
}

impl std::str::FromStr for Operation {
    type Err = String;
    fn from_str(input: &str) -> Result<Self, Self::Err> {
        let mut chars = input.trim().chars();
        match chars.next() {
            Some('s') => chars.as_str().parse().map(Operation::Spin).map_err(
                |_| format!("Could not parse position from {}", chars.as_str()),
            ),
            Some('x') => {
                let mut split = chars.as_str().split('/');
                match (split.next(), split.next()) {
                    (Some(a), Some(b)) => Ok(Operation::Exchange(
                        a.parse().map_err(|_| {
                            format!("Could not parse {} as position", a)
                        })?,
                        b.parse().map_err(|_| {
                            format!("Could not parse {} as position", b)
                        })?,
                    )),
                    _ => Err(format!("Could not parse {} as Operation", input)),
                }
            },
            Some('p') => {
                let mut split = chars.as_str().split('/');
                match (split.next(), split.next()) {
                    (Some(a), Some(b)) => Ok(Operation::Partner(
                        a.parse().map_err(|_| {
                            format!("Could not parse {} as name", a)
                        })?,
                        b.parse().map_err(|_| {
                            format!("Could not parse {} as name", b)
                        })?,
                    )),
                    _ => Err(format!("Could not parse {} as Operation", input)),
                }
            }
            _ => Err(format!("Could not parse {} as Operation", input)),
        }
    }
}

fn cycle_len(initial: &str, input: &str) -> usize {
    let mut cycle_len = 1;
    let mut changed = part1(initial, input);
    while changed != initial {
        changed = part1(&changed, input);
        cycle_len += 1;
    }
    cycle_len
}

fn part1(initial: &str, input: &str) -> String {
    let mut result = initial.to_owned();
    let instructions: Vec<Operation> =
        input.split(',').map(|s| s.parse().unwrap()).collect();
    for instr in instructions {
        match instr {
            Operation::Spin(p) => {
                result = {
                    let split = result.split_at(result.len() - p);
                    let mut swapped = split.1.to_owned();
                    swapped.push_str(split.0);
                    swapped
                }
            }
            Operation::Exchange(a, b) => {
                unsafe { result.as_bytes_mut() }.swap(a, b);
            }
            Operation::Partner(a, b) => {
                let loc_a =
                    result.find(a).expect(&format!("Could not find {}", a));
                let loc_b =
                    result.find(b).expect(&format!("Could not find {}", b));
                unsafe { result.as_bytes_mut() }.swap(loc_a, loc_b);
            }
        }
    }
    result
}

fn part2(initial: &str, input: &str, count: usize) -> String {
    let cycle = cycle_len(initial, input);
    let mut result = initial.to_owned();
    for _ in 0..(count % cycle) {
        result = part1(&result, input);
    }
    result
}

fn main() {
    let opt = Opt::from_args();
    let mut contents = String::new();
    if opt.input.to_str() == Some("-") {
        std::io::stdin()
            .read_to_string(&mut contents)
            .expect("could not read stdin");
    } else {
        let mut file = File::open(&opt.input)
            .expect(&format!("file {} not found", opt.input.display()));
        file.read_to_string(&mut contents)
            .expect(&format!("could not read file {}", opt.input.display()));
    }
    println!("Part 1: {}", part1("abcdefghijklmnop", &contents));
    println!("Part 2: {}", part2("abcdefghijklmnop", &contents, 1_000_000_000));
}

#[derive(StructOpt, Debug)]
#[structopt(name = "day13", about = "Advent of code 2017 day 13")]
struct Opt {
    #[structopt(help = "Input file", parse(from_os_str))] input: PathBuf,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn part1_test() {
        assert_eq!(part1("abcde", "s1,x3/4,pe/b"), "baedc");
    }

    #[test]
    fn part4_test() {
        assert_eq!(part2("abcde", "s1,x3/4,pe/b", 2), "ceadb");
    }
}
