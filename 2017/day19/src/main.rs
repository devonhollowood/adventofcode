extern crate structopt;
#[macro_use]
extern crate structopt_derive;

use structopt::StructOpt;
use std::fs::File;
use std::io::Read;
use std::path::PathBuf;

struct OutOfBounds;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Position {
    // positions are (y, x). y increases downward, x increases right
    current: (usize, usize),
    bounds: (usize, usize),
}

impl Position {
    fn go(&self, dir: Direction) -> Result<(usize, usize), OutOfBounds> {
        use Direction::*;
        match dir {
            Up => if self.current.0 == 0 {
                Err(OutOfBounds)
            } else {
                Ok((self.current.0 - 1, self.current.1))
            },
            Down => if self.current.0 == self.bounds.0 {
                Err(OutOfBounds)
            } else {
                Ok((self.current.0 + 1, self.current.1))
            },
            Left => if self.current.1 == 0 {
                Err(OutOfBounds)
            } else {
                Ok((self.current.0, self.current.1 - 1))
            },
            Right => if self.current.1 == self.bounds.1 {
                Err(OutOfBounds)
            } else {
                Ok((self.current.0, self.current.1 + 1))
            },
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Direction {
    Up,
    Down,
    Left,
    Right,
}

impl Direction {
    fn reverse(&self) -> Direction {
        use Direction::*;
        match *self {
            Up => Down,
            Down => Up,
            Left => Right,
            Right => Left,
        }
    }
}

fn traverse(input: &str) -> (String, usize) {
    use Direction::*;
    let map: Vec<Vec<char>> =
        input.lines().map(|l| l.chars().collect()).collect();
    let mut position = Position {
        current: (0, map[0].iter().position(|&ch| ch == '|').unwrap()),
        bounds: (map.len(), map[0].len()),
    };
    let mut direction = Down;
    let mut letters = String::new();
    let mut steps = 1;
    while let Ok(pos) = position.go(direction) {
        steps += 1;
        position.current = pos;
        match map[pos.0][pos.1] {
            '|' | '-' => {}
            ' ' => panic!(format!(
                "ruh roh! ({}, {})",
                position.current.0, position.current.1
            )),
            ch => {
                if ch != '+' {
                    letters.push(ch)
                }
                match vec![Up, Down, Left, Right]
                    .into_iter()
                    .filter(|&d| d != direction.reverse())
                    .filter(|&d| {
                        if let Ok(p) = position.go(d) {
                            map[p.0][p.1] != ' '
                        } else {
                            false
                        }
                    })
                    .next()
                {
                    Some(dir) => direction = dir,
                    None => break,
                }
            }
        }
    }
    (letters, steps)
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
    let (letters, steps) = traverse(&contents);
    println!("Part 1: {}", letters);
    println!("Part 2: {}", steps);
}

#[derive(StructOpt, Debug)]
#[structopt(name = "day13", about = "Advent of code 2017 day 13")]
struct Opt {
    #[structopt(help = "Input file", parse(from_os_str))] input: PathBuf,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn traverse_test() {
        let input = concat!(
            "     |          \n",
            "     |  +--+    \n",
            "     A  |  C    \n",
            " F---|----E|--+ \n",
            "     |  |  |  D \n",
            "     +B-+  +--+ \n",
            "                ",
        );
        assert_eq!(traverse(input), ("ABCDEF".into(), 38));
    }
}
