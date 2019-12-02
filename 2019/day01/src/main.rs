use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;
use std::path::PathBuf;
use structopt::StructOpt;

fn part1(input: &[isize]) -> isize {
    input.iter().map(|val| val / 3 - 2).sum()
}

fn extra_fuel(mass: isize) -> isize {
    let mut total = 0;
    let mut remainder = mass;
    loop {
        remainder = remainder / 3 - 2;
        if remainder > 0 {
            total += remainder;
        } else {
            break;
        }
    }
    total
}

fn part2(input: &[isize]) -> isize {
    input
        .iter()
        .map(|val| val / 3 - 2)
        .map(|mass| mass + extra_fuel(mass))
        .sum()
}

fn main() {
    let handle = File::open(Opt::from_args().input).expect("error opening file");
    let input: Vec<_> = BufReader::new(handle)
        .lines()
        .map(|line| line.expect("error reading input"))
        .map(|line| {
            line.parse::<isize>()
                .unwrap_or_else(|err| panic!("could not parse line \"{}\": {:?}", line, err))
        })
        .collect();
    println!("Part 1: {}", part1(&input));
    println!("Part 2: {}", part2(&input));
}

#[derive(StructOpt)]
struct Opt {
    #[structopt(parse(from_os_str))]
    input: PathBuf,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part2() {
        let expected = vec![(14, 2), (1969, 966), (100756, 50346)];
        for (input, output) in expected {
            assert_eq!(extra_fuel(input), output);
        }
    }
}
