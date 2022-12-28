use std::str::FromStr;

use anyhow::{Context, Result};

#[derive(Debug, Clone, Copy)]
pub struct Range {
    start: usize,
    end: usize,
}

impl Range {
    fn contains(&self, other: &Range) -> bool {
        self.start <= other.start && self.end >= other.end
    }

    fn overlaps(&self, other: &Range) -> bool {
        self.start <= other.start && self.end >= other.start
            || self.start <= other.end && self.end >= other.end
    }
}

impl FromStr for Range {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (start, end) = s
            .split_once('-')
            .with_context(|| format!("{} did not contain a -", s))?;
        Ok(Range {
            start: start.parse()?,
            end: end.parse()?,
        })
    }
}

fn parse_line(line: &str) -> Result<(Range, Range)> {
    let (first, second) = line
        .split_once(',')
        .with_context(|| format!("{} did not contain a ,", line))?;
    Ok((first.parse()?, second.parse()?))
}

pub fn parse(input: &str) -> Result<Vec<(Range, Range)>> {
    input.lines().map(parse_line).collect()
}

pub fn part1(input: &[(Range, Range)]) -> usize {
    input
        .iter()
        .filter(|(first, second)| first.contains(second) || second.contains(first))
        .count()
}

pub fn part2(input: &[(Range, Range)]) -> usize {
    input
        .iter()
        .filter(|(first, second)| first.overlaps(second) || second.overlaps(first))
        .count()
}

#[cfg(test)]
mod tests {
    use super::*;

    const INPUT: &str = "2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8";

    #[test]
    fn test_part1() {
        let parsed = parse(INPUT).unwrap();
        assert_eq!(part1(&parsed), 2);
    }

    #[test]
    fn test_part2() {
        let parsed = parse(INPUT).unwrap();
        assert_eq!(part2(&parsed), 4);
    }
}
