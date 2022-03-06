use std::collections::HashSet;

use anyhow::{anyhow, Result};
use nalgebra::DMatrix;

pub fn parse(input: &str) -> Result<DMatrix<u8>> {
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
    Ok(DMatrix::<u8>::from_iterator(
        ncols,
        nrows,
        input
            .chars()
            .filter(|c| c.is_digit(10))
            .map(|c| c.to_digit(10).unwrap() as u8),
    )
    .transpose())
}

fn neighbor_points(matrix: &DMatrix<u8>, pos: (usize, usize)) -> Vec<(usize, usize)> {
    let (r, c) = pos;
    [
        (r.wrapping_sub(1), c.wrapping_sub(1)),
        (r.wrapping_sub(1), c),
        (r.wrapping_sub(1), c + 1),
        (r, c.wrapping_sub(1)),
        (r, c + 1),
        (r + 1, c.wrapping_sub(1)),
        (r + 1, c),
        (r + 1, c + 1),
    ]
    .into_iter()
    .filter(|n| matrix.get(*n).is_some())
    .collect()
}

fn flash(state: &mut DMatrix<u8>, pos: (usize, usize)) {
    for neighbor in neighbor_points(state, pos) {
        state[neighbor] += 1;
    }
}

fn step(state: &mut DMatrix<u8>) -> usize {
    let ones = DMatrix::<u8>::repeat(state.nrows(), state.ncols(), 1);
    *state += ones;
    let mut flashed = HashSet::new();
    loop {
        let mut any_flashed = false;
        for r in 0..state.nrows() {
            for c in 0..state.ncols() {
                if state[(r, c)] > 9 && !flashed.contains(&(r, c)) {
                    flash(state, (r, c));
                    flashed.insert((r, c));
                    any_flashed = true;
                }
            }
        }
        if !any_flashed {
            break;
        }
    }
    for pos in &flashed {
        state[*pos] = 0;
    }
    flashed.len()
}

pub fn part1(input: &DMatrix<u8>) -> usize {
    let mut input = input.clone();
    let mut sum = 0;
    for _ in 0..100 {
        sum += step(&mut input);
    }
    sum
}

pub fn part2(input: &DMatrix<u8>) -> usize {
    let mut input = input.clone();
    for n in 1..=usize::MAX {
        if step(&mut input) == input.len() {
            return n;
        }
    }
    panic!("not found!")
}

#[cfg(test)]
mod tests {
    use super::*;

    const INPUT: &str = r"
5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526";

    #[test]
    fn test_part1() {
        let parsed = parse(INPUT).unwrap();
        assert_eq!(part1(&parsed), 1656);
    }

    #[test]
    fn test_part2() {
        let parsed = parse(INPUT).unwrap();
        assert_eq!(part2(&parsed), 195);
    }
}
