extern crate structopt;
#[macro_use]
extern crate structopt_derive;

use structopt::StructOpt;
use std::collections::{HashMap, HashSet};
use std::fs::File;
use std::io::Read;

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
                _ => panic!(format!("Could not parse line")),
            }
        })
        .collect()
}

fn part1(nodemap: &HashMap<Node, Vec<Node>>) -> usize {
    let mut reachable = HashSet::<Node>::new();
    let mut outer_edge = HashSet::<Node>::new();
    outer_edge.insert(0);
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
    reachable.len()
}

fn main() {
    let opt = Opt::from_args();
    let mut contents = String::new();
    if opt.input == "-" {
        std::io::stdin()
            .read_to_string(&mut contents)
            .expect("could not read stdin");
    } else {
        let mut file = File::open(&opt.input)
            .expect(&format!("file {} not found", opt.input));
        file.read_to_string(&mut contents)
            .expect(&format!("could not read file {}", opt.input));
    }
    let nodemap = parse(&contents);
    println!("Part 1: {}", part1(&nodemap));
}

#[derive(StructOpt, Debug)]
#[structopt(name = "day12", about = "Advent of code 2017 day 12")]
struct Opt {
    #[structopt(help = "Input file")] input: String,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn part1_test() {
        let nodemap = parse(
            "0 <-> 2\n\
             1 <-> 1\n\
             2 <-> 0, 3, 4\n\
             3 <-> 2, 4\n\
             4 <-> 2, 3, 6\n\
             5 <-> 6\n\
             6 <-> 4, 5\n",
        );
        assert_eq!(part1(&nodemap), 6);
    }
}
