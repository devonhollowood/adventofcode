use anyhow::{Context, Result};
use itertools::Itertools;

pub fn parse(input: &str) -> Result<Vec<i32>> {
    input
        .lines()
        .map(|line| line.parse().with_context(|| format!("parsing {}", line)))
        .collect()
}

pub fn part1(input: &[i32]) -> usize {
    input.iter().tuple_windows().filter(|(a, b)| b > a).count()
}

pub fn part2(input: &[i32]) -> usize {
    input
        .iter()
        .tuple_windows()
        .map(|(a, b, c)| a + b + c)
        .tuple_windows()
        .filter(|(a, b)| b > a)
        .count()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        let input = vec![199, 200, 208, 210, 200, 207, 240, 269, 260, 263];
        assert_eq!(part1(&input), 7);
    }

    #[test]
    fn test_part2() {
        let input = vec![199, 200, 208, 210, 200, 207, 240, 269, 260, 263];
        assert_eq!(part2(&input), 5);
    }
}
