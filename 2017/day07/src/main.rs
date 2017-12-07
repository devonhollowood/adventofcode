extern crate structopt;
#[macro_use]
extern crate structopt_derive;

extern crate regex;

use structopt::StructOpt;
use regex::Regex;
use std::collections::{HashMap, HashSet};
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct Program {
    name: String,
    weight: u64,
    above: Vec<String>,
}

type Tower = HashMap<String, Program>;

fn parse_input(input: &str) -> Tower {
    let line_re = Regex::new(concat!(
        r"^(?P<name>\w+)\s+\((?P<weight>\d+)\)(:?\s+->\s+)?",
        r"(?P<above>(?:\w+,\s+)*\w*)$"
    )).unwrap();
    let mut tower = Tower::new();
    for line in input.lines() {
        let caps = line_re
            .captures(line)
            .expect(&format!("Could not parse line: {}", line));
        let name: String = caps["name"].into();
        let weight = caps["weight"].parse().expect("bad weight regex");
        let above = caps["above"]
            .replace(",", "")
            .split_whitespace()
            .map(|s| s.into())
            .collect();
        tower.insert(
            name.clone(),
            Program {
                name: name,
                weight: weight,
                above: above,
            },
        );
    }
    tower
}

fn bottom_program(tower: &Tower) -> String {
    let mut possibilities: HashSet<_> = tower.keys().cloned().collect();
    for prog in tower.values() {
        for above in &prog.above {
            possibilities.remove(above);
        }
    }
    if possibilities.len() != 1 {
        panic!("remaining possibilities: {:?}", possibilities);
    }
    possibilities.into_iter().next().unwrap()
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
    let tower = parse_input(&contents);
    println!("Part 1: {}", bottom_program(&tower));
}

#[derive(StructOpt, Debug)]
#[structopt(name = "day07", about = "Advent of code 2017 day 07")]
struct Opt {
    #[structopt(help = "Input file")] input: String,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn bottom_program_test() {
        let tower = parse_input(
            "pbga (66)\n\
             xhth (57)\n\
             ebii (61)\n\
             havc (66)\n\
             ktlj (57)\n\
             fwft (72) -> ktlj, cntj, xhth\n\
             qoyq (66)\n\
             padx (45) -> pbga, havc, qoyq\n\
             tknk (41) -> ugml, padx, fwft\n\
             jptl (61)\n\
             ugml (68) -> gyxo, ebii, jptl\n\
             gyxo (61)\n\
             cntj (57)",
        );
        assert_eq!(bottom_program(&tower), "tknk");
    }
}
