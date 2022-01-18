use anyhow::{Context, Result};
use itertools::Itertools;

pub fn parse(input: &str) -> Result<Vec<i32>> {
    input
        .split(',')
        .map(|i| {
            i.trim()
                .parse()
                .with_context(|| format!("could not parse {} as integer", i))
        })
        .collect()
}

fn fuel_at_1(crabs: &[i32], position: i32) -> i32 {
    crabs.iter().map(|c| (c - position).abs()).sum()
}

fn fuel_at_2(crabs: &[i32], position: i32) -> i32 {
    crabs
        .iter()
        .map(|c| {
            let dist = (c - position).abs();
            dist * (dist + 1) / 2
        })
        .sum()
}

pub fn part1(input: &[i32]) -> i32 {
    let mut input = input.to_vec();

    input.sort_unstable();

    // the median crab is the optimal position
    let median = if input.len() % 2 != 0 {
        input[input.len() / 2]
    } else {
        (input[input.len() / 2 - 1] + input[input.len() / 2]) / 2
    };

    fuel_at_1(&input, median)
}

pub fn part2(input: &[i32]) -> i32 {
    // now it is a little trickier, so let's just brute force it
    let (min, max) = input.iter().cloned().minmax().into_option().unwrap();
    (min..=max)
        .into_iter()
        .map(|x| fuel_at_2(input, x))
        .min()
        .unwrap()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse() {
        let input = "16,1,2,0,4,2,7,1,2,14";
        assert_eq!(parse(input).unwrap(), vec![16, 1, 2, 0, 4, 2, 7, 1, 2, 14]);
    }

    #[test]
    fn test_part1() {
        let input = [16, 1, 2, 0, 4, 2, 7, 1, 2, 14];
        assert_eq!(part1(&input), 37);
    }

    #[test]
    fn test_part2() {
        let input = [16, 1, 2, 0, 4, 2, 7, 1, 2, 14];
        assert_eq!(part2(&input), 168);
    }
}
