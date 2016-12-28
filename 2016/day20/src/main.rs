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
    fn new(low: IP, high: IP) -> IPRange {
        IPRange {
            low: low,
            high: high,
        }
    }
    fn full() -> IPRange {
        IPRange {
            low: 0,
            high: IP::max_value(),
        }
    }
    fn contains(&self, ip: IP) -> bool {
        self.low <= ip && ip <= self.high
    }
    fn minimum_valid(&self, forbidden: &BTreeSet<IPRange>) -> Option<IP> {
        let mut ip = self.low;
        for range in forbidden.iter() {
            if range.contains(ip) {
                if range.high < self.high {
                    ip = range.high + 1;
                } else {
                    return None;
                }
            }
        }
        Some(ip)
    }

    fn number_allowed(&self, forbidden: &BTreeSet<IPRange>) -> u32 {
        let mut ip = self.low;
        let mut count = 0;
        for range in forbidden.iter() {
            if ip < range.low {
                count += range.low - ip;
            }
            if ip <= range.high {
                if range.high >= self.high {
                    return count;
                } else {
                    ip = range.high + 1;
                }
            }
        }
        count += self.high - ip + 1; // count last one, too
        count
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


fn main() {
    let input = parse_args().unwrap_or_else(|err| panic!("Error reading args: {}", err));
    let forbidden: BTreeSet<IPRange> = input.lines()
        .map(|line| line.trim().parse())
        .collect::<Result<_, _>>()
        .unwrap_or_else(|err| panic!("Error parsing ranges: {}", err));
    let range = IPRange::full();
    match range.minimum_valid(&forbidden) {
        Some(ip) => println!("minimum valid IP: {}", ip),
        None => println!("No valid IPs found!"),
    }
    println!("number of valid IPs: {}", range.number_allowed(&forbidden));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn min_valid_example() {
        let forbidden = ["5-8", "0-2", "4-7"].iter().map(|r| r.parse().unwrap()).collect();
        assert_eq!(IPRange::new(0, 10).minimum_valid(&forbidden), Some(3));
    }

    #[test]
    fn num_allowed_example() {
        let forbidden = ["5-8", "0-2", "4-7"].iter().map(|r| r.parse().unwrap()).collect();
        assert_eq!(IPRange::new(0, 10).number_allowed(&forbidden), 2);
    }

    #[test]
    fn num_allowed_complex() {
        let forbidden = ["3-5", "2-6", "8-10", "9-11"]
            .iter()
            .map(|r| r.parse().unwrap())
            .collect();
        assert_eq!(IPRange::new(0, 12).number_allowed(&forbidden), 4);
    }
}
