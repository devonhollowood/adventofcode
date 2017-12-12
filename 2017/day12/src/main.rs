extern crate structopt;
#[macro_use]
extern crate structopt_derive;

use structopt::StructOpt;
use std::collections::{HashMap, HashSet};
use std::fs::File;
use std::io::Read;
use std::path::PathBuf;

type Node = usize;

fn parse(input: &str) -> HashMap<Node, Vec<Node>> {
    input
        .lines()
        .map(|line| {
            let mut split = line.split("<->");
            match (split.next(), split.next()) {
                (Some(node), Some(neighbors)) => (
                    node.trim()
                        .parse()
                        .expect(&format!("Could not parse {} as node", node)),
                    neighbors
                        .split(',')
                        .map(|neighbor| {
                            neighbor.trim().parse().expect(&format!(
                                "Could not parse {} as node",
                                neighbor
                            ))
                        })
                        .collect(),
                ),
                _ => panic!(format!("Could not parse line {}", line)),
            }
        })
        .collect()
}

fn node_group(
    start: Node,
    nodemap: &HashMap<Node, Vec<Node>>,
) -> HashSet<Node> {
    let mut reachable = HashSet::<Node>::new();
    let mut outer_edge = HashSet::<Node>::new();
    outer_edge.insert(start);
    while !outer_edge.is_empty() {
        let mut new_edge = HashSet::new();
        for item in outer_edge.drain() {
            if !reachable.contains(&item) {
                reachable.insert(item);
                new_edge.extend(nodemap[&item].iter());
            }
        }
        outer_edge = new_edge;
    }
    reachable
}

fn part1(nodemap: &HashMap<Node, Vec<Node>>) -> usize {
    node_group(0, nodemap).len()
}

fn part2(nodemap: &HashMap<Node, Vec<Node>>) -> usize {
    let mut visited = HashSet::<Node>::new();
    let mut groups: Vec<HashSet<Node>> = Vec::new();
    for start in nodemap.keys().cloned() {
        if visited.contains(&start) {
            continue;
        }
        let group = node_group(start, nodemap);
        for node in &group {
            visited.insert(node.clone());
        }
        groups.push(group);
    }
    groups.len()
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
    let nodemap = parse(&contents);
    println!("Part 1: {}", part1(&nodemap));
    println!("Part 2: {}", part2(&nodemap));
}

#[derive(StructOpt, Debug)]
#[structopt(name = "day12", about = "Advent of code 2017 day 12")]
struct Opt {
    #[structopt(help = "Input file", parse(from_os_str))] input: PathBuf,
}

#[cfg(test)]
mod tests {
    use super::*;

    static TEST_INPUT: &str = "0 <-> 2\n\
                               1 <-> 1\n\
                               2 <-> 0, 3, 4\n\
                               3 <-> 2, 4\n\
                               4 <-> 2, 3, 6\n\
                               5 <-> 6\n\
                               6 <-> 4, 5\n";

    #[test]
    fn part1_test() {
        let nodemap = parse(TEST_INPUT);
        assert_eq!(part1(&nodemap), 6);
    }

    #[test]
    fn part2_test() {
        let nodemap = parse(TEST_INPUT);
        assert_eq!(part2(&nodemap), 2);
    }
}
