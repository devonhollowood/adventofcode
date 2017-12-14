extern crate structopt;
#[macro_use]
extern crate structopt_derive;

use structopt::StructOpt;
use std::fs::File;
use std::io::Read;
use std::path::PathBuf;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Scanner {
    location: usize,
    range: usize,
}

impl Scanner {
    fn location_at(&self, time: usize) -> usize {
        if self.range == 0 {
            return 0;
        }
        let cycle_loc = time % (self.range * 2 - 2);
        if cycle_loc > self.range {
            cycle_loc - self.range
        } else {
            cycle_loc
        }
    }
}

#[derive(Debug, Clone)]
struct Firewall {
    scanners: Vec<(usize, Scanner)>,
    depth: usize,
}

impl Firewall {
    fn severity(&self) -> usize {
        let mut severity = 0;
        for &(depth, ref scanner) in &self.scanners {
            if scanner.location_at(depth) == 0 {
                severity += scanner.range * depth;
            }
        }
        severity
    }
    fn check(&self, delay: usize) -> bool {
        for &(depth, ref scanner) in &self.scanners {
            if scanner.location_at(delay + depth) == 0 {
                return false;
            }
        }
        true
    }
}

impl std::str::FromStr for Firewall {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut max_depth = 0;
        let mut scanners = Vec::new();
        for line in s.lines() {
            let mut split = line.split(':');
            match (split.next(), split.next()) {
                (Some(depth_str), Some(range_str)) => {
                    let depth = depth_str
                        .trim()
                        .parse()
                        .map_err(|e| format!("{:?}", e))?;
                    let range = range_str
                        .trim()
                        .parse()
                        .map_err(|e| format!("{:?}", e))?;
                    if depth > max_depth {
                        max_depth = depth;
                    }
                    scanners.push((
                        depth,
                        Scanner {
                            location: 0,
                            range: range,
                        },
                    ));
                }
                _ => Err(format!("Could not parse: {}", line))?,
            }
        }
        Ok(Firewall {
            scanners: scanners,
            depth: max_depth,
        })
    }
}

fn part1(firewall: &Firewall) -> usize {
    firewall.severity()
}

fn part2(firewall: &Firewall) -> usize {
    let mut delay = 0;
    loop {
        if firewall.check(delay) {
            return delay;
        }
        delay += 1;
    }
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
    let firewall: Firewall = contents.parse().expect("Error parsing input");
    println!("Part 1: {}", part1(&firewall));
    println!("Part 2: {}", part2(&firewall));
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
        let firewall = "0: 3\n1: 2\n4: 4\n6: 4".parse().unwrap();
        assert_eq!(part1(&firewall), 24);
    }

    #[test]
    fn part2_test() {
        let firewall = "0: 3\n1: 2\n4: 4\n6: 4".parse().unwrap();
        assert_eq!(part2(&firewall), 10);
    }
}
