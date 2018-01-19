extern crate structopt;
#[macro_use]
extern crate structopt_derive;

use std::collections::BTreeMap;
use std::default::Default;
use structopt::StructOpt;
use std::fs::File;
use std::io::Read;
use std::path::PathBuf;

#[derive(Debug, PartialEq, Eq, Clone, Copy, PartialOrd, Ord, Hash)]
struct Position(isize, isize);

impl Position {
    fn go(&self, dir: Direction) -> Position {
        use Direction::*;
        match dir {
            Up => Position(self.0 + 1, self.1),
            Right => Position(self.0, self.1 + 1),
            Down => Position(self.0 - 1, self.1),
            Left => Position(self.0, self.1 - 1),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum Direction {
    Up,
    Right,
    Down,
    Left,
}

impl Direction {
    fn turn_left(self) -> Direction {
        use Direction::*;
        match self {
            Up => Left,
            Right => Up,
            Down => Right,
            Left => Down,
        }
    }
    fn turn_right(self) -> Direction {
        use Direction::*;
        match self {
            Up => Right,
            Right => Down,
            Down => Left,
            Left => Up,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum NodeState {
    Clean,
    Weakened,
    Infected,
    Flagged,
}

impl NodeState {
    fn progress(&mut self) {
        use NodeState::*;
        *self = match *self {
            Clean => Weakened,
            Weakened => Infected,
            Infected => Flagged,
            Flagged => Clean,
        };
    }
    fn toggle(&mut self) {
        use NodeState::*;
        *self = match *self {
            Clean => Infected,
            Weakened => panic!("Cannot toggle weakened state"),
            Infected => Clean,
            Flagged => panic!("Cannot toggle flagged state"),
        };
    }
}

impl Default for NodeState {
    fn default() -> Self {
        NodeState::Clean
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
struct Virus {
    loc: Position,
    facing: Direction,
}

impl Virus {
    /// Have virus act as in part 1. Returns true if virus infects a new cell
    fn act1(&mut self, infected: &mut Board) -> bool {
        use NodeState::*;
        let loc = self.loc;
        let state = infected.get(&loc).cloned().unwrap_or_default();
        // turn
        match state {
            Clean => {
                self.facing = self.facing.turn_left();
            }
            Infected => {
                self.facing = self.facing.turn_right();
            }
            _ => panic!("Invalid state for part 1"),
        }
        // infect
        infected.entry(loc).or_insert(Clean).toggle();
        if infected[&loc] == Clean {
            infected.remove(&loc);
        }
        // move
        self.loc = loc.go(self.facing);
        state != Infected
            && infected.get(&loc).cloned().unwrap_or_default() == Infected
    }
}

type Board = BTreeMap<Position, NodeState>; // set of infected nodes

fn parse(input: &str) -> (Virus, Board) {
    let bools: Vec<bool> = input
        .bytes()
        .filter_map(|b| match b {
            b'#' => Some(true),
            b'.' => Some(false),
            _ => None,
        })
        .collect();
    let side_len = (bools.len() as f64).sqrt() as usize;
    if side_len * side_len != bools.len() {
        panic!("unsquare grid");
    }
    if side_len % 2 == 0 {
        panic!("center undefined");
    }
    let offset = (side_len / 2) as isize;
    let mut board = BTreeMap::new();
    for idx in bools.into_iter().enumerate().filter_map(|(idx, b)| {
        if b {
            Some(idx)
        } else {
            None
        }
    }) {
        let r = offset as isize - (idx / side_len) as isize;
        let c = (idx % side_len) as isize - offset;
        board.insert(Position(r, c), NodeState::Infected);
    }
    (
        Virus {
            loc: Position(0, 0),
            facing: Direction::Up,
        },
        board,
    )
}

fn part1(mut virus: Virus, mut board: Board) -> usize {
    let mut count = 0;
    for _ in 0..10_000 {
        if virus.act1(&mut board) {
            count += 1;
        }
    }
    count
}

fn main() {
    let opt = Opt::from_args();
    let mut contents = String::new();
    if opt.input.to_str() == Some("-") {
        std::io::stdin()
            .read_to_string(&mut contents)
            .expect("could not read stdin");
    } else {
        let mut file = File::open(&opt.input)
            .expect(&format!("file {} not found", opt.input.display()));
        file.read_to_string(&mut contents)
            .expect(&format!("could not read file {}", opt.input.display()));
    }
    let (virus, board) = parse(&contents);
    println!("Part 1: {}", part1(virus, board));
}

#[derive(StructOpt, Debug)]
#[structopt(name = "day22", about = "Advent of code 2017 day 22")]
struct Opt {
    #[structopt(help = "Input file", parse(from_os_str))]
    input: PathBuf,
}

#[cfg(test)]
mod tests {
    use super::*;

    static INPUT: &str = concat!("..#\n", "#..\n", "...\n");

    #[test]
    fn parse_test() {
        use NodeState::Infected;
        assert_eq!(
            parse(INPUT),
            (
                Virus {
                    loc: Position(0, 0),
                    facing: Direction::Up,
                },
                vec![(Position(1, 1), Infected), (Position(0, -1), Infected)]
                    .into_iter()
                    .collect()
            )
        );
    }

    #[test]
    fn act_test() {
        let (mut virus, mut board) = parse(INPUT);
        assert_eq!(virus.act1(&mut board), true);
        assert_eq!(
            virus,
            Virus {
                loc: Position(0, -1),
                facing: Direction::Left,
            }
        );
        assert_eq!(board, parse(concat!("..#\n", "##.\n", "...\n")).1);
    }

    #[test]
    fn part1_test() {
        let (virus, board) = parse(INPUT);
        assert_eq!(part1(virus, board), 5587);
    }
}
