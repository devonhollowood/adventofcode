use anyhow::Result;
use itertools::Itertools;

pub fn parse(input: &str) -> Result<Vec<&str>> {
    Ok(input.lines().collect())
}

fn priority(c: char) -> usize {
    let priority = if c.is_ascii_lowercase() {
        c as u8 - b'a' + 1
    } else {
        c as u8 - b'A' + 27
    };
    priority as usize
}

pub fn part1(input: &[&str]) -> usize {
    input
        .iter()
        .map(|line| line.split_at(line.len() / 2))
        .flat_map(|(lhs, rhs)| lhs.chars().filter(|c| rhs.contains(*c)).unique())
        .map(priority)
        .sum()
}

pub fn part2(input: &[&str]) -> usize {
    input
        .iter()
        .chunks(3)
        .into_iter()
        .flat_map(|chunks| {
            let chunks: Vec<_> = chunks.collect();
            chunks[0]
                .chars()
                .filter(move |c| chunks[1].contains(*c) && chunks[2].contains(*c))
                .unique()
        })
        .map(priority)
        .sum()
}

#[cfg(test)]
mod tests {
    use super::*;

    const INPUT: &str = "vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw";

    #[test]
    fn test_part1() {
        let parsed = parse(INPUT).unwrap();
        assert_eq!(part1(&parsed), 157);
    }

    #[test]
    fn test_part2() {
        let parsed = parse(INPUT).unwrap();
        assert_eq!(part2(&parsed), 70);
    }
}
