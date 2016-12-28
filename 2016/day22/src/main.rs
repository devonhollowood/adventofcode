extern crate clap;
extern crate regex;

#[macro_use]
extern crate lazy_static;

use std::io::prelude::*;
use std::fs::File;
use regex::Regex;
use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Position {
    x: usize,
    y: usize,
}

#[derive(Debug)]
struct NodeInfo {
    size: usize,
    used: usize,
    avail: usize,
    use_percent: usize,
}

#[derive(Debug)]
struct NodeListing {
    pos: Position,
    info: NodeInfo,
}

impl std::str::FromStr for NodeListing {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        lazy_static! {
            static ref NODE_RE: Regex =
                Regex::new(
                    r"/dev/grid/node-x(\d+)-y(\d+)\s+(\d+)T\s+(\d+)T\s+(\d+)T\s+(\d+)%")
                .expect("bad regex NODE_RE");
        }
        if let Some(cap) = NODE_RE.captures(s) {
            Ok(NodeListing {
                pos: Position {
                    x: cap[1].parse().unwrap(),
                    y: cap[2].parse().unwrap(),
                },
                info: NodeInfo {
                    size: cap[3].parse().unwrap(),
                    used: cap[4].parse().unwrap(),
                    avail: cap[5].parse().unwrap(),
                    use_percent: cap[6].parse().unwrap(),
                }
            })
        } else {
            Err(format!("Could not parse NodeListing from {}", s))
        }
    }
}

fn valid_pairs(nodes: &HashMap<Position, NodeInfo>) -> Vec<(Position, Position)> {
    let mut pairs = Vec::new();
    for (&pos_a, ref info_a) in nodes.iter() {
        for (&pos_b, ref info_b) in nodes.iter() {
            if info_a.used != 0 && pos_a != pos_b && info_a.used <= info_b.avail {
                pairs.push((pos_a, pos_b));
            }
        }
    }
    pairs
}

fn parse_args() -> std::io::Result<String> {
    let matches = clap::App::new("Day 22")
        .author("Devon Hollowood")
        .arg(clap::Arg::with_name("nodes")
            .index(1)
            .help("file containing output of nodes `df`. Reads from stdin otherwise"))
        .get_matches();
    let source = matches.value_of_os("nodes");
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
    let nodes: HashMap<Position, NodeInfo> = input.lines()
        .map(|line| line.trim().parse::<NodeListing>())
        .filter_map(|r| r.ok())
        .map(|listing| (listing.pos, listing.info))
        .collect();
    println!("number of valid node pairs: {}", valid_pairs(&nodes).len());
}
