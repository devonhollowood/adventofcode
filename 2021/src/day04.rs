use anyhow::{anyhow, Context, Result};
use itertools::Itertools;
use nalgebra::Matrix5;

#[derive(Debug, Clone)]
pub struct Game {
    draws: Vec<u32>,
    boards: Vec<Board>,
}

#[derive(Debug, Clone)]
pub struct Board {
    squares: Matrix5<u32>,
}

#[derive(Debug, Clone)]
struct WinInfo {
    draw_idx: usize,
    score: u32,
}

impl Board {
    fn winning_index(&self, draws: &[u32]) -> Option<WinInfo> {
        // 1 if corresponding position in board is marked, 0 otherwise
        let mut marked = Matrix5::<u32>::zeros();
        for (draw_idx, &draw) in draws.iter().enumerate() {
            // search for match
            if let Some(found_idx) = self.squares.data.as_slice().iter().position(|x| *x == draw) {
                // mark match
                marked.data.as_mut_slice()[found_idx] = 1;

                // return win info if we win
                if marked.row_sum().as_slice().contains(&5)
                    || marked.column_sum().as_slice().contains(&5)
                {
                    let score = self
                        .squares
                        .component_mul(&(Matrix5::<u32>::repeat(1) - marked))
                        .sum()
                        * draw;
                    return Some(WinInfo { draw_idx, score });
                }
            }
        }
        None
    }
}

fn parse_nums(line: &str) -> Result<Vec<u32>> {
    line.split_ascii_whitespace()
        .map(|n| {
            n.parse::<u32>()
                .with_context(|| format!("parsing draw: {}", n))
        })
        .collect()
}

pub fn parse(input: &str) -> Result<Game> {
    let lines: Vec<&str> = input
        .lines()
        .map(|l| l.trim())
        .filter(|l| !l.is_empty())
        .collect();

    if lines.is_empty() {
        return Err(anyhow!("no non-empty lines"));
    }

    if lines.len() % 5 != 1 {
        return Err(anyhow!("incomplete board"));
    }

    let draws = parse_nums(&lines[0].replace(',', " "))?;

    let mut boards = Vec::new();
    for board_chunk in lines[1..].chunks(5) {
        let rows = board_chunk
            .iter()
            .map(|line| parse_nums(line))
            .collect::<Result<Vec<Vec<u32>>>>()?;
        let squares = Matrix5::<u32>::from_iterator(rows.into_iter().flatten()).transpose();
        boards.push(Board { squares });
    }

    Ok(Game { draws, boards })
}

pub fn part1(input: &Game) -> u32 {
    input
        .boards
        .iter()
        .filter_map(|b| b.winning_index(&input.draws))
        .sorted_by_key(|winner| winner.draw_idx)
        .next()
        .expect("nobody won!")
        .score
}

pub fn part2(input: &Game) -> u32 {
    input
        .boards
        .iter()
        .filter_map(|b| b.winning_index(&input.draws))
        .sorted_by_key(|winner| winner.draw_idx)
        .last()
        .expect("nobody won!")
        .score
}

#[cfg(test)]
mod tests {

    const INPUT: &str = r"
7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7

    ";

    use super::*;

    #[test]
    fn test_parse() {
        let parsed = parse(INPUT).unwrap();
        assert_eq!(
            parsed.draws,
            vec![
                7, 4, 9, 5, 11, 17, 23, 2, 0, 14, 21, 24, 10, 16, 13, 6, 15, 25, 12, 22, 18, 20, 8,
                19, 3, 26, 1
            ]
        );

        assert_eq!(parsed.boards.len(), 3);

        assert_eq!(
            parsed.boards[0].squares,
            Matrix5::<u32>::new(
                22, 13, 17, 11, 0, //
                8, 2, 23, 4, 24, //
                21, 9, 14, 16, 7, //
                6, 10, 3, 18, 5, //
                1, 12, 20, 15, 19, //
            )
        );
    }

    #[test]
    fn test_part1() {
        let parsed = parse(INPUT).unwrap();
        assert_eq!(part1(&parsed), 4512);
    }

    #[test]
    fn test_part2() {
        let parsed = parse(INPUT).unwrap();
        assert_eq!(part2(&parsed), 1924);
    }
}
