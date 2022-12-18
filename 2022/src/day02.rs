use std::str::FromStr;

use anyhow::{bail, Context, Result};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Throw {
    Rock,
    Paper,
    Scissors,
}

impl Throw {
    pub fn beats(&self, other: Throw) -> bool {
        use Throw::*;
        matches!(
            (self, other),
            (Rock, Scissors) | (Scissors, Paper) | (Paper, Rock)
        )
    }

    pub fn score(&self) -> isize {
        use Throw::*;
        match self {
            Rock => 1,
            Paper => 2,
            Scissors => 3,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Game {
    enemy: Throw,
    yours: Throw,
}

impl Game {
    fn score(&self) -> isize {
        let outcome = if self.yours.beats(self.enemy) {
            6
        } else if self.enemy.beats(self.yours) {
            0
        } else {
            3
        };
        outcome + self.yours.score()
    }
}

impl FromStr for Game {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use Throw::*;
        let split = s
            .split_once(' ')
            .with_context(|| format!("expected space in [{}]", s))?;
        let enemy = match split.0 {
            "A" => Rock,
            "B" => Paper,
            "C" => Scissors,
            _ => bail!("bad enemy throw: {}", split.0),
        };
        let yours = match split.1 {
            "X" => Rock,
            "Y" => Paper,
            "Z" => Scissors,
            _ => bail!("bad self throw: {}", split.0),
        };
        Ok(Game { enemy, yours })
    }
}

pub fn parse(input: &str) -> Result<Vec<Game>> {
    input.lines().map(|line| line.parse()).collect()
}

pub fn part1(input: &[Game]) -> isize {
    input.iter().map(|game| game.score()).sum()
}

pub fn part2(input: &[Game]) -> isize {
    todo!()
}

#[cfg(test)]
mod tests {
    use super::*;

    const INPUT: &str = "A Y\nB X\nC Z";

    #[test]
    fn test_parse() {
        use Throw::*;
        assert_eq!(
            parse(INPUT).unwrap(),
            vec![
                Game {
                    enemy: Rock,
                    yours: Paper
                },
                Game {
                    enemy: Paper,
                    yours: Rock
                },
                Game {
                    enemy: Scissors,
                    yours: Scissors
                },
            ]
        );
    }

    #[test]
    fn test_part1() {
        let parsed = parse(INPUT).unwrap();
        assert_eq!(part1(&parsed), 15);
    }

    #[test]
    fn test_part2() {
        todo!()
        // let parsed = parse(INPUT).unwrap();
        // assert_eq!(part2(&parsed), 45000);
    }
}
