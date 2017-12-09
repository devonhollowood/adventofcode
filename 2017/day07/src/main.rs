extern crate structopt;
#[macro_use]
extern crate structopt_derive;

extern crate regex;

use structopt::StructOpt;
use regex::Regex;
use std::collections::{HashMap, HashSet};
use std::fs::File;
use std::hash::Hash;
use std::io::Read;

#[derive(Debug)]
struct Program {
    name: String,
    weight: Weight,
    above: Vec<String>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
struct Correction {
    name: String,
    old_weight: Weight,
    new_weight: Weight,
}

#[derive(Debug, PartialEq, Eq, Clone)]
enum Balance {
    Balanced(Weight),
    Unbalanced(Correction),
}

type Weight = u64;
type Tower = HashMap<String, Program>;

fn parse_input(input: &str) -> Tower {
    let line_re = Regex::new(concat!(
        r"^(?P<name>\w+)\s+\((?P<weight>\d+)\)(:?\s+->\s+)?",
        r"(?P<above>(?:\w+,\s+)*\w*)$"
    )).unwrap();
    let mut tower = Tower::new();
    for line in input.lines() {
        let caps = line_re.captures(line).expect(&format!(
            "Could not parse line: {}",
            line
        ));
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

fn partition_by<Iter, F, Item, Key>(items: Iter, f: F) -> Vec<Item>
where
    Item: Hash + Eq + Clone,
    Key: Hash + Eq + Clone,
    F: Fn(Item) -> Key,
    Iter: Iterator<Item = Item>,
{
    let mut keys: HashMap<Key, Item> = HashMap::new();
    let mut counts: HashMap<Key, usize> = HashMap::new();
    for item in items {
        keys.insert(f(item.clone()), item.clone());
        *counts.entry(f(item.clone())).or_insert(0) += 1
    }
    let mut key_vec: Vec<Key> = keys.keys().cloned().collect();
    (&mut key_vec).sort_unstable_by_key(|item| counts[item]);
    key_vec
        .into_iter()
        .map(|key| keys.remove(&key).expect("key not in vec"))
        .collect()
}

fn balance(node: &str, tower: &Tower) -> Balance {
    let prog = &tower[node];
    let mut sub_balances = HashMap::new();
    for balance in prog.above.iter().map(
        |child| (child, balance(child, tower)),
    )
    {
        match balance {
            (child, Balance::Balanced(weight)) => {
                sub_balances.insert(child, weight);
            }
            // we are told that there is at most one unbalanced child node
            (_, correction) => return correction,
        }
    }
    let partitioned = partition_by(sub_balances.iter(), |(_, weight)| weight);
    match partitioned.len() {
        0 | 1 => Balance::Balanced(
            prog.weight + sub_balances.values().sum::<Weight>(),
        ),
        2 => {
            let wrong_stack_weight = partitioned[0].1;
            let right_stack_weight = partitioned[1].1;
            let name: String = (*partitioned[0].0).to_owned();
            let old_weight = tower[&name].weight;
            Balance::Unbalanced(Correction {
                name: name,
                old_weight: old_weight,
                new_weight: old_weight + right_stack_weight -
                    wrong_stack_weight,
            })
        }
        _ => panic!(format!("too many unbalanced nodes: {:?}", partitioned)),
    }
}

fn main() {
    let opt = Opt::from_args();
    let mut contents = String::new();
    if opt.input == "-" {
        std::io::stdin().read_to_string(&mut contents).expect(
            "could not read stdin",
        );
    } else {
        let mut file = File::open(&opt.input).expect(&format!(
            "file {} not found",
            opt.input
        ));
        file.read_to_string(&mut contents).expect(&format!(
            "could not read file {}",
            opt.input
        ));
    }
    let tower = parse_input(&contents);
    let bottom = bottom_program(&tower);
    println!("Part 1: {}", bottom);
    match balance(&bottom, &tower) {
        Balance::Balanced(_) => println!("Part 2: Already balanced"),
        Balance::Unbalanced(correction) => {
            println!(
                "Part 2: {}: {} -> {}",
                correction.name,
                correction.old_weight,
                correction.new_weight
            )
        }
    }
}

#[derive(StructOpt, Debug)]
#[structopt(name = "day07", about = "Advent of code 2017 day 07")]
struct Opt {
    #[structopt(help = "Input file")]
    input: String,
}

#[cfg(test)]
mod tests {
    use super::*;

    static TEST_INPUT: &str = "pbga (66)\n\
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
         cntj (57)";

    #[test]
    fn bottom_program_test() {
        let tower = parse_input(TEST_INPUT);
        assert_eq!(bottom_program(&tower), "tknk");
    }

    #[test]
    fn balance_test() {
        let tower = parse_input(TEST_INPUT);
        assert_eq!(balance("ugml", &tower), Balance::Balanced(251));
        assert_eq!(balance("padx", &tower), Balance::Balanced(243));
        assert_eq!(balance("fwft", &tower), Balance::Balanced(243));
        assert_eq!(
            balance("tknk", &tower),
            Balance::Unbalanced(Correction {
                name: "ugml".to_owned(),
                old_weight: 68,
                new_weight: 60,
            })
        );
    }
}
