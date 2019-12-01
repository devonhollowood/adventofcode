use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;
use std::path::PathBuf;
use structopt::StructOpt;

fn part1(input: &[isize]) -> isize {
    input.iter().map(|val| val / 3 - 2).sum()
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
}

#[derive(StructOpt)]
struct Opt {
    #[structopt(parse(from_os_str))]
    input: PathBuf,
}
