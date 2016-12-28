extern crate clap;
extern crate regex;

#[macro_use]
extern crate lazy_static;

use std::io::prelude::*;
use std::fs::File;
use regex::Regex;
use std::collections::BTreeSet;

type IP = u32;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct IPRange {
    low: IP,
    high: IP,
}

impl IPRange {
    fn contains(&self, ip: IP) -> bool {
        self.low <= ip && ip <= self.high
    }
}

impl std::str::FromStr for IPRange {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        lazy_static! {
            static ref STRING_RE: Regex =
                Regex::new(r"(\d+)-(\d+)").expect("bad regex STRING_RE");
        }
        if let Some(cap) = STRING_RE.captures(s) {
            Ok(IPRange {
                low: cap[1].parse().expect("bug in STRING_RE low capture"),
                high: cap[2].parse().expect("bug in STRING_RE high capture"),
            })
        } else {
            Err(format!("Could not parse IPRange from {}", s))
        }
    }
}

impl std::fmt::Display for IPRange {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        write!(f, "{}-{}", self.low, self.high)
    }
}

fn parse_args() -> std::io::Result<String> {
    let matches = clap::App::new("Day 20")
        .author("Devon Hollowood")
        .arg(clap::Arg::with_name("IP ranges")
            .index(1)
            .help("file containing IP ranges for puzzle. Reads from stdin otherwise"))
        .get_matches();
    let source = matches.value_of_os("IP ranges");
    let mut contents = String::new();
    match source {
        Some(filename) => {
            let mut input = File::open(filename)?;
            input.read_to_string(&mut contents)?;
        }
        None => {
            let mut input = std::io::stdin();
            input.read_to_string(&mut contents)?;
        }
    }
    Ok(contents)
}

fn minimum_valid(forbidden: &BTreeSet<IPRange>) -> IP {
    let mut ip = 0;
    for range in forbidden.iter() {
        if range.contains(ip) {
            ip = range.high + 1;
        }
    }
    ip
}

fn main() {
    let input = parse_args().unwrap_or_else(|err| panic!("Error reading args: {}", err));
    let forbidden: BTreeSet<IPRange> = input.lines()
        .map(|line| line.trim().parse())
        .collect::<Result<_, _>>()
        .unwrap_or_else(|err| panic!("Error parsing ranges: {}", err));
    println!("minimum valid IP: {}", minimum_valid(&forbidden));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn example() {
        let ranges = ["5-8", "0-2", "4-7"].iter().map(|r| r.parse().unwrap()).collect();
        assert_eq!(minimum_valid(&ranges), 3);
    }
}
