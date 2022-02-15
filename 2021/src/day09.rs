use std::collections::HashSet;

use anyhow::{anyhow, Result};
use itertools::Itertools;
use nalgebra::DMatrix;

pub fn parse(input: &str) -> Result<DMatrix<i64>> {
    let nrows = input.lines().filter(|l| !l.is_empty()).count();
    if nrows == 0 {
        return Err(anyhow!("input was blank"));
    }
    let ncols = input
        .lines()
        .find(|l| !l.is_empty())
        .unwrap()
        .chars()
        .filter(|c| c.is_digit(10))
        .count();
    Ok(DMatrix::<i64>::from_iterator(
        ncols,
        nrows,
        input
            .chars()
            .filter(|c| c.is_digit(10))
            .map(|c| c.to_digit(10).unwrap() as i64),
    )
    .transpose())
}

fn neighbor_points(matrix: &DMatrix<i64>, pos: (usize, usize)) -> Vec<(usize, usize)> {
    let (r, c) = pos;
    [
        (r.wrapping_sub(1), c),
        (r, c.wrapping_sub(1)),
        (r, c + 1),
        (r + 1, c),
    ]
    .into_iter()
    .filter(|n| matrix.get(*n).is_some())
    .collect()
}

fn neighbor_values(matrix: &DMatrix<i64>, pos: (usize, usize)) -> Vec<i64> {
    let (r, c) = pos;
    [
        (r.wrapping_sub(1), c),
        (r, c.wrapping_sub(1)),
        (r, c + 1),
        (r + 1, c),
    ]
    .into_iter()
    .filter_map(|n| matrix.get(n))
    .copied()
    .collect()
}

pub fn part1(input: &DMatrix<i64>) -> i64 {
    (0..input.nrows())
        .cartesian_product(0..input.ncols())
        .filter_map(|(r, c)| {
            let point = (r, c);
            let height = input.get(point)?;
            if neighbor_values(input, point)
                .into_iter()
                .all(|n| n > *height)
            {
                Some(height + 1)
            } else {
                None
            }
        })
        .sum()
}

fn basin_size(map: &DMatrix<i64>, point: (usize, usize)) -> usize {
    let mut stack = vec![point];
    let mut size = 0;
    let mut visited = HashSet::new();
    while let Some(p) = stack.pop() {
        if visited.contains(&p) {
            continue;
        }
        match map.get(p) {
            Some(9) => continue,
            None => continue,
            _ => (),
        }
        visited.insert(p);
        size += 1;
        stack.extend(neighbor_points(map, p));
    }
    size
}

pub fn part2(input: &DMatrix<i64>) -> usize {
    let mut sizes: Vec<usize> = (0..input.nrows())
        .cartesian_product(0..input.ncols())
        .filter_map(|(r, c)| {
            let point = (r, c);
            let height = input.get(point)?;
            if neighbor_values(input, point)
                .into_iter()
                .all(|n| n > *height)
            {
                Some(point)
            } else {
                None
            }
        })
        .map(|p| basin_size(input, p))
        .collect();
    sizes.sort_unstable();
    sizes[sizes.len() - 3..].iter().product()
}

#[cfg(test)]
mod tests {
    use super::*;

    const INPUT: &str = r"
    2199943210
    3987894921
    9856789892
    8767896789
    9899965678";

    #[test]
    fn test_part1() {
        let parsed = parse(INPUT).unwrap();
        assert_eq!(part1(&parsed), 15);
    }

    #[test]
    fn test_part2() {
        let parsed = parse(INPUT).unwrap();
        assert_eq!(part2(&parsed), 1134);
    }
}
