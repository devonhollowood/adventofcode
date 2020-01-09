use std::collections::{HashMap, HashSet};
use std::path::PathBuf;
use structopt::StructOpt;

type Chemical = String;

#[derive(Debug, Clone)]
struct Reaction {
    inputs: HashMap<Chemical, isize>,
    output: Chemical,
    output_count: isize,
}

fn dfs_order<T, F, IterT>(initial: T, next: F) -> Vec<T>
where
    T: Eq + Clone + std::hash::Hash,
    F: Fn(&T) -> IterT,
    IterT: IntoIterator<Item = T>,
{
    let mut stack = vec![initial];
    let mut visited = HashSet::new();
    let mut results = Vec::new();
    while let Some(node) = stack.pop() {
        if visited.contains(&node) {
            // our second time visiting, so we've already processed all of the children nodes
            results.push(node);
            continue;
        }
        visited.insert(node.clone());
        stack.extend(
            next(&node)
                .into_iter()
                .filter(|node| !visited.contains(node)),
        );
        stack.push(node);
    }
    results
}

fn part1(input: &[Reaction]) -> isize {
    // assumption: all outputs are unique. But let's check that.
    let endings: HashSet<_> = input.iter().map(|rxn| rxn.output.clone()).collect();
    assert_eq!(input.len(), endings.len());
    let cookbook: HashMap<_, _> = input
        .iter()
        .cloned()
        .map(|rxn| (rxn.output, (rxn.inputs, rxn.output_count)))
        .collect();

    let mut ore_count = 0;
    let mut stack = vec![("FUEL".to_string(), 1)];
    while let Some((output, needed)) = stack.pop() {
        dbg!(&output);
        dbg!(needed);
        assert!(needed > 0);
        if output == "ORE" {
            ore_count += needed;
            continue;
        }
        let (ref ingredients, output_count) = cookbook[&output];
        let mut multiplier = needed / output_count;
        if needed % output_count != 0 {
            multiplier += 1;
        }
        for (ingredient, count) in ingredients.iter() {
            stack.push((ingredient.clone(), count * multiplier))
        }
    }
    ore_count
}

fn main() {
    let input = parser::parse(
        &std::fs::read_to_string(Opt::from_args().input).expect("error reading file"),
    );
    println!("Part 1: {}", part1(&input));
}

mod parser {
    use super::{Chemical, Reaction};
    use nom::bytes::complete::tag;
    use nom::character::complete::{alpha1, digit1, line_ending};
    use nom::combinator::{map, map_res};
    use nom::multi::{separated_list, separated_nonempty_list};
    use nom::sequence::separated_pair;
    use std::collections::HashMap;

    type IResult<'a, O> = nom::IResult<&'a str, O>;

    pub(super) fn parse(input: &str) -> Vec<Reaction> {
        let (rest, result) = separated_list(line_ending, reaction)(input)
            .unwrap_or_else(|err| panic!("Parser failure: {}", err));
        assert!(
            ["", "\n", "\r\n"].contains(&rest),
            "Unexpected trailing input: {:?}",
            rest
        );
        result
    }

    fn reaction(input: &str) -> IResult<Reaction> {
        map(
            separated_pair(chemical_inputs, tag(" => "), chemical_count),
            |(inputs, (output, output_count))| Reaction {
                inputs,
                output,
                output_count,
            },
        )(input)
    }

    fn chemical_inputs(input: &str) -> IResult<HashMap<Chemical, isize>> {
        let (rest, counts) = separated_nonempty_list(tag(", "), chemical_count)(input)?;
        Ok((rest, counts.into_iter().collect()))
    }

    fn chemical_count(input: &str) -> IResult<(Chemical, isize)> {
        let (rest, (count, chem)) =
            separated_pair(map_res(digit1, |s: &str| s.parse()), tag(" "), alpha1)(input)?;
        Ok((rest, (chem.into(), count)))
    }
}

#[derive(StructOpt)]
struct Opt {
    #[structopt(parse(from_os_str))]
    input: PathBuf,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!(part1(&parser::parse(include_str!("../examples/1.txt"))), 31);
        assert_eq!(
            part1(&parser::parse(include_str!("../examples/2.txt"))),
            165
        );
        assert_eq!(
            part1(&parser::parse(include_str!("../examples/3.txt"))),
            13_312
        );
        assert_eq!(
            part1(&parser::parse(include_str!("../examples/4.txt"))),
            180_697
        );
        assert_eq!(
            part1(&parser::parse(include_str!("../examples/5.txt"))),
            2_210_736
        );
    }
}
