use anyhow::{anyhow, Result};
use std::convert::TryInto;

#[derive(Debug, Clone)]
pub struct Entry {
    input: [String; 10],
    output: [String; 4],
}

pub fn parse(input: &str) -> Result<Vec<Entry>> {
    let mut entries = Vec::new();
    for line in input.lines() {
        if line.trim().is_empty() {
            continue;
        }
        let (i, o) = line
            .split_once("|")
            .ok_or_else(|| anyhow!("line did not contain a |: {}", line))?;
        let i_vec: Vec<String> = i
            .trim()
            .split_ascii_whitespace()
            .map(|i| i.to_owned())
            .collect();
        let o_vec: Vec<String> = o
            .trim()
            .split_ascii_whitespace()
            .map(|i| i.to_owned())
            .collect();
        let input = i_vec
            .try_into()
            .map_err(|v: Vec<_>| anyhow!("{}: expected 10 values but got {}", i, v.len()))?;
        let output = o_vec
            .try_into()
            .map_err(|v: Vec<_>| anyhow!("{}: expected 10 values but got {}", i, v.len()))?;
        entries.push(Entry { input, output });
    }
    Ok(entries)
}

pub fn part1(input: &[Entry]) -> usize {
    input
        .iter()
        .flat_map(|e| e.output.iter())
        .filter(|signal| [2, 4, 3, 7].contains(&signal.len()))
        .count()
}

pub fn part2(_input: &[Entry]) -> &'static str {
    "<TODO>"
}

#[cfg(test)]
mod tests {
    use super::*;

    const INPUT: &str = include_str!("test-data/day08.txt");

    #[test]
    fn test_parse() {
        let parsed = parse(INPUT).unwrap();
        assert_eq!(parsed.len(), 10);
        assert_eq!(
            parsed[0].input,
            [
                "be", "cfbegad", "cbdgef", "fgaecd", "cgeb", "fdcge", "agebfd", "fecdb", "fabcd",
                "edb"
            ]
        );
        assert_eq!(parsed[0].output, ["fdgacbe", "cefdb", "cefbgd", "gcbe"]);
    }

    #[test]
    fn test_part1() {
        let parsed = parse(INPUT).unwrap();
        assert_eq!(part1(&parsed), 26);
    }
}
