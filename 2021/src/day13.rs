use anyhow::{anyhow, Context, Result};
use itertools::Itertools;
use std::collections::BTreeSet;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Default)]
pub struct Point(i32, i32);

#[derive(Debug, Clone)]
pub enum FoldDirection {
    Left,
    Up,
}

#[derive(Debug, Clone)]
pub struct Fold {
    line: i32,
    direction: FoldDirection,
}

#[derive(Debug, Clone)]
pub struct Input {
    points: BTreeSet<Point>,
    folds: Vec<Fold>,
}

pub fn parse(input: &str) -> Result<Input> {
    let mut points = BTreeSet::new();
    let mut folds = Vec::new();
    for line in input.lines().map(|l| l.trim()).filter(|l| !l.is_empty()) {
        if let Some(suffix) = line.strip_prefix("fold along ") {
            let (dir_str, fold_line_str) = suffix
                .split_once("=")
                .ok_or_else(|| anyhow!("Invalid fold line: {}", line))?;
            let fold_line = fold_line_str
                .parse()
                .with_context(|| format!("could not parse {} as fold line", fold_line_str))?;
            let dir = match dir_str {
                "x" => FoldDirection::Left,
                "y" => FoldDirection::Up,
                _ => return Err(anyhow!("could not parse {} as direction", dir_str)),
            };
            folds.push(Fold {
                line: fold_line,
                direction: dir,
            });
            continue;
        }

        // parsing a coordinate line
        let (x, y) = line
            .split_once(",")
            .ok_or_else(|| anyhow!("Invalid coordinate line: {}", line))?;

        let point = Point(
            x.parse()
                .with_context(|| format!("could not parse {} as x-coordinate", x))?,
            y.parse()
                .with_context(|| format!("could not parse {} as y-coordinate", y))?,
        );
        if !points.insert(point) {
            return Err(anyhow!("duplicate point: ({})", line));
        }
    }
    Ok(Input { points, folds })
}

fn apply_folds_to_point(mut point: Point, folds: &[Fold]) -> Point {
    for fold in folds {
        match fold.direction {
            FoldDirection::Left => {
                if point.0 > fold.line {
                    let delta = point.0 - fold.line;
                    point.0 = fold.line - delta;
                }
            }
            FoldDirection::Up => {
                if point.1 > fold.line {
                    let delta = point.1 - fold.line;
                    point.1 = fold.line - delta;
                }
            }
        }
    }
    point
}

fn apply_folds_to_paper(paper: &BTreeSet<Point>, folds: &[Fold]) -> BTreeSet<Point> {
    paper
        .iter()
        .copied()
        .map(|p| apply_folds_to_point(p, folds))
        .collect()
}

fn display(points: &BTreeSet<Point>) -> String {
    let (l_bound, r_bound) = points
        .iter()
        .copied()
        .minmax_by_key(|p| p.0)
        .into_option()
        .map(|(a, b)| (a.0, b.0))
        .unwrap_or_default();
    let (u_bound, d_bound) = points
        .iter()
        .copied()
        .minmax_by_key(|p| p.1)
        .into_option()
        .map(|(a, b)| (a.1, b.1))
        .unwrap_or_default();
    let width = r_bound - l_bound;
    let height = d_bound - u_bound;
    let mut grid = Vec::new();
    for y in 0..=height {
        for x in 0..=width {
            grid.push(if points.contains(&Point(x, y)) {
                b'#'
            } else {
                b'.'
            });
        }
        grid.push(b'\n');
    }
    String::from_utf8(grid).unwrap()
}

pub fn part1(input: &Input) -> usize {
    apply_folds_to_paper(&input.points, &input.folds[..1]).len()
}

pub fn part2(input: &Input) -> String {
    format!(
        "\n{}",
        display(&apply_folds_to_paper(&input.points, &input.folds))
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    const INPUT: &str = r"
6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5
    ";

    #[test]
    fn test_part1() {
        let parsed = parse(INPUT).unwrap();
        assert_eq!(part1(&parsed), 17);
    }
}
