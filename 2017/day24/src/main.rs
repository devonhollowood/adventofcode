extern crate structopt;
#[macro_use]
extern crate structopt_derive;

use structopt::StructOpt;
use std::collections::BTreeSet;
use std::fs::File;
use std::io::Read;
use std::path::PathBuf;

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct Component {
    input: usize,
    output: usize,
}

impl Component {
    fn flip(&self) -> Component {
        Component {
            input: self.output,
            output: self.input,
        }
    }
}

fn parse(input: &str) -> BTreeSet<Component> {
    let mut components = BTreeSet::new();
    for line in input.lines() {
        let (i, o) = line.split_at(
            line.find('/').expect(&format!("invalid line: {}", line)),
        );
        let new_comp = Component {
            input: i.trim().parse().expect(&format!("invalid size: {}", i)),
            output: o[1..]
                .trim()
                .parse()
                .expect(&format!("invalid size: {}", o)),
        };
        assert!(
            !components.contains(&new_comp)
                && !components.contains(&new_comp.flip()),
            "Repeat component found! You'll need a better algorithm."
        );
        components.insert(new_comp);
    }
    components
}

fn max_strength(start: usize, components: &BTreeSet<Component>) -> usize {
    components
        .iter()
        .filter_map(|component| {
            if component.input == start {
                let mut new_set = components.clone();
                new_set.remove(component);
                Some(
                    max_strength(component.output, &new_set) + component.input
                        + component.output,
                )
            } else if component.output == start {
                let mut new_set = components.clone();
                new_set.remove(component);
                Some(
                    max_strength(component.input, &new_set) + component.input
                        + component.output,
                )
            } else {
                None
            }
        })
        .max()
        .unwrap_or_default()
}

fn part1(components: &BTreeSet<Component>) -> usize {
    max_strength(0, components)
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
    let components = parse(&contents);
    println!("Part 1: {}", part1(&components));
}

#[derive(StructOpt, Debug)]
#[structopt(name = "day24", about = "Advent of code 2017 day 24")]
struct Opt {
    #[structopt(help = "Input file", parse(from_os_str))]
    input: PathBuf,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn part1_test() {
        let input = concat!(
            "0/2\n",
            "2/2\n",
            "2/3\n",
            "3/4\n",
            "3/5\n",
            "0/1\n",
            "10/1\n",
            "9/10"
        );
        let components = parse(input);
        assert_eq!(part1(&components), 31);
    }
}
