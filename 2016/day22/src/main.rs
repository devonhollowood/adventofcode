extern crate clap;
extern crate regex;

#[macro_use]
extern crate lazy_static;

use std::io::prelude::*;
use std::fs::File;
use regex::Regex;
use std::collections::{HashMap, VecDeque};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Position {
    x: usize,
    y: usize,
}

impl std::fmt::Display for Position {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        write!(f, "node-y{}-x{}", self.y, self.x)
    }
}

impl Position {
    fn neighbors(&self) -> Vec<Position> {
        let &Position { x, y } = self;
        let mut neighbors = vec![Position { x: x + 1, y: y }, Position { x: x, y: y + 1 }];
        if x != 0 {
            neighbors.push(Position { x: x - 1, y: y });
        }
        if y != 0 {
            neighbors.push(Position { x: x, y: y - 1 });
        }
        neighbors
    }
}

#[derive(Debug, Clone)]
struct NodeInfo {
    size: usize,
    used: usize,
    avail: usize,
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
                    r"/dev/grid/node-x(\d+)-y(\d+)\s+(\d+)T\s+(\d+)T\s+(\d+)T\s+\d+%")
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
                },
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

fn possible_moves(nodes: &HashMap<Position, NodeInfo>) -> Vec<(Position, Position)> {
    let mut possibilities = Vec::new();
    for (&pos_a, ref info_a) in nodes.iter() {
        if info_a.used == 0 {
            // no point in moving no data
            continue;
        }
        for pos_b in pos_a.neighbors() {
            if let Some(info_b) = nodes.get(&pos_b) {
                if info_a.used <= info_b.avail {
                    possibilities.push((pos_a, pos_b));
                }
            }
        }
    }
    possibilities
}

fn move_data(nodes: &mut HashMap<Position, NodeInfo>, from: Position, to: Position) {
    let from_info = nodes.get(&from)
        .unwrap_or_else(|| panic!("Invalid move: {} does not exist", from))
        .clone();
    if let Some(ref mut to_info) = nodes.get_mut(&to) {
        if from_info.used > to_info.avail {
            panic!("Invalid move: {} (avail: {}) cannot hold {} (used: {})",
                   to,
                   to_info.avail,
                   from,
                   from_info.avail);
        }
        to_info.used += from_info.used;
        to_info.avail -= from_info.used;
    } else {
        panic!("Invalid move: {} does not exist", to);
    }
    let mut from_info_mut = nodes.get_mut(&from).unwrap();
    from_info_mut.avail = from_info.size;
    from_info_mut.used = 0;
}

fn solve(nodes: &HashMap<Position, NodeInfo>) -> Option<Vec<(Position, Position)>> {
    let mut state_queue = VecDeque::new();
    let initial_target_loc = nodes.keys()
        .filter(|pos| pos.y == 0)
        .max_by_key(|pos| pos.x)
        .expect("Invalid target state")
        .clone();
    state_queue.push_back((nodes.clone(), initial_target_loc, Vec::new()));
    while let Some((state, target_loc, moves)) = state_queue.pop_front() {
        // print!("considering target = {} moves = [", target_loc);
        // for &(from, to) in &moves {
        //    print!("{} -> {}, ", from, to);
        // }
        // println!("] ({} moves from here.)", possible_moves(&state).len());
        for (from, to) in possible_moves(&state) {
            let mut new_state = state.clone();
            let mut new_moves = moves.clone();
            new_moves.push((from, to));
            move_data(&mut new_state, from, to);
            let new_target_loc = if from == target_loc { to } else { target_loc };
            if new_target_loc.x == 0 && new_target_loc.y == 0 {
                return Some(new_moves);
            }
            state_queue.push_back((new_state, new_target_loc, new_moves));
        }
    }
    None
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
    if let Some(solution) = solve(&nodes) {
        println!("Solution:");
        for mov in &solution {
            println!("{} -> {}", mov.0, mov.1);
        }
        println!("Total moves taken: {}", solution.len())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn example() {
        let nodes = ["/dev/grid/node-x0-y0   10T    8T     2T   80%",
                     "/dev/grid/node-x0-y1   11T    6T     5T   54%",
                     "/dev/grid/node-x0-y2   32T   28T     4T   87%",
                     "/dev/grid/node-x1-y0    9T    7T     2T   77%",
                     "/dev/grid/node-x1-y1    8T    0T     8T    0%",
                     "/dev/grid/node-x1-y2   11T    7T     4T   63%",
                     "/dev/grid/node-x2-y0   10T    6T     4T   60%",
                     "/dev/grid/node-x2-y1    9T    8T     1T   88%",
                     "/dev/grid/node-x2-y2    9T    6T     3T   66%"]
            .into_iter()
            .map(|s| s.parse::<NodeListing>().unwrap())
            .map(|listing| (listing.pos, listing.info))
            .collect();
        assert_eq!(solve(&nodes).unwrap().len(), 7);
    }
}
