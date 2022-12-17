use std::collections::BinaryHeap;

use anyhow::{Context, Result};

type Elf = Vec<usize>;

pub fn parse(input: &str) -> Result<Vec<Elf>> {
    let mut elves = Vec::new();
    let mut elf = Elf::new();
    for line in input.lines() {
        if line.is_empty() {
            if !elf.is_empty() {
                elves.push(std::mem::take(&mut elf));
            }
            continue;
        }
        let calories = line.parse().with_context(|| format!("parsing {}", line))?;
        elf.push(calories);
    }
    if !elf.is_empty() {
        elves.push(elf);
    }
    Ok(elves)
}

pub fn part1(input: &[Elf]) -> usize {
    input
        .iter()
        .map(|elf| elf.iter().sum::<usize>())
        .max()
        .expect("expected at least one elf")
}

pub fn part2(input: &[Elf]) -> usize {
    let mut heap = BinaryHeap::new();
    for elf in input {
        heap.push(elf.iter().sum::<usize>());
    }
    let mut sum = 0;
    for _ in 0..3 {
        sum += heap.pop().unwrap_or_default();
    }
    sum
}

#[cfg(test)]
mod tests {
    use super::*;

    const INPUT: &str = "1000
2000
3000

4000

5000
6000

7000
8000
9000

10000";

    #[test]
    fn test_parse() {
        assert_eq!(
            parse(INPUT).unwrap(),
            vec![
                vec![1000, 2000, 3000],
                vec![4000],
                vec![5000, 6000],
                vec![7000, 8000, 9000],
                vec![10000]
            ]
        );
    }

    #[test]
    fn test_part1() {
        let parsed = parse(INPUT).unwrap();
        assert_eq!(part1(&parsed), 24000);
    }

    #[test]
    fn test_part2() {
        let parsed = parse(INPUT).unwrap();
        assert_eq!(part2(&parsed), 45000);
    }
}
