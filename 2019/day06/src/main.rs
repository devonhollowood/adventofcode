use std::collections::HashMap;
use std::path::PathBuf;
use structopt::StructOpt;

// keys orbit values
type OrbitMap = HashMap<String, String>;

fn part1(input: &OrbitMap) -> usize {
    input
        .iter()
        .map(|(_, orbited)| {
            std::iter::successors(Some(orbited), |orbited| input.get(*orbited)).count()
        })
        .sum()
}

fn part2(input: &OrbitMap) -> usize {
    let you_ancestors_dist: HashMap<_, _> =
        std::iter::successors(input.get("YOU"), |orbited| input.get(*orbited))
            .enumerate()
            .map(|(dist, loc)| (loc, dist))
            .collect();
    for (santa_dist, santa_ancestor) in
        std::iter::successors(input.get("SAN"), |orbited| input.get(*orbited)).enumerate()
    {
        if let Some(you_dist) = you_ancestors_dist.get(santa_ancestor) {
            return you_dist + santa_dist;
        }
    }
    panic!("No common ancestor found");
}

fn parse(input: &str) -> OrbitMap {
    input
        .lines()
        .filter(|line| !line.is_empty())
        .map(|line| {
            let names: Vec<_> = line.trim().split(')').collect();
            assert_eq!(names.len(), 2);
            (names[1].into(), names[0].into())
        })
        .collect()
}

fn main() {
    let input =
        parse(&std::fs::read_to_string(Opt::from_args().input).expect("error reading file"));
    println!("Part 1: {}", part1(&input));
    println!("Part 2: {}", part2(&input));
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
    fn test_part1_simple() {
        let input = parse("A)B\nB)C\nC)D\n");
        assert_eq!(part1(&input), 6);
    }

    #[test]
    fn test_part1_complex() {
        let example = parse(include_str!("example.txt"));
        assert_eq!(part1(&example), 42);
    }

    #[test]
    fn test_part2() {
        let example = parse(include_str!("example2.txt"));
        assert_eq!(part2(&example), 4);
    }
}
