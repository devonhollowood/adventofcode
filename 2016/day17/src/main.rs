extern crate clap;
extern crate octavo;

use octavo::digest::prelude::*;
use std::collections::VecDeque;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Point {
    x: usize,
    y: usize,
}

impl Point {
    fn new(x: usize, y: usize) -> Self {
        Point { x: x, y: y }
    }
    fn make_move(&self, mov: Move) -> Point {
        use Move::*;
        match mov {
            Up => {
                Point {
                    x: self.x,
                    y: self.y - 1,
                }
            }
            Down => {
                Point {
                    x: self.x,
                    y: self.y + 1,
                }
            }
            Left => {
                Point {
                    x: self.x - 1,
                    y: self.y,
                }
            }
            Right => {
                Point {
                    x: self.x + 1,
                    y: self.y,
                }
            }
        }
    }
}

impl std::fmt::Display for Point {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        write!(f, "({}, {})", self.x, self.y)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Move {
    Up,
    Down,
    Left,
    Right,
}

impl Move {
    fn as_char(&self) -> char {
        use Move::*;
        match *self {
            Up => 'U',
            Down => 'D',
            Left => 'L',
            Right => 'R',
        }
    }
    fn hits_wall(&self, pos: Point) -> bool {
        use Move::*;
        match *self {
            Up => pos.y == 0,
            Down => pos.y == 3,
            Left => pos.x == 0,
            Right => pos.x == 3,
        }
    }
}

fn md5(input: &[u8]) -> String {
    use std::fmt::Write;
    let mut output_buf = vec![0u8; Md5::output_bytes()];
    let mut digest = Md5::default();
    digest.update(input);
    digest.result(&mut output_buf);
    let mut output = String::with_capacity(output_buf.len() * 2);
    for ch in output_buf {
        write!(output, "{:02x}", ch).expect("could not write to output_buf");
    }
    output
}


fn possible_moves(pos: Point, history: &[Move], key: &str) -> Vec<Move> {
    use Move::*;
    let mut hash_input = key.to_owned();
    hash_input.extend(history.iter().map(|mov| mov.as_char()));
    let hash = md5(hash_input.as_bytes());
    let mut possibilities = Vec::new();
    for (dir, ch) in [Up, Down, Left, Right].into_iter().zip(hash.chars()) {
        match ch {
            'b'...'f' => {
                if !dir.hits_wall(pos) {
                    possibilities.push(*dir);
                }
            }
            _ => continue,
        }
    }
    possibilities
}

fn shortest_path(start: Point, end: Point, key: &str) -> Option<Vec<Move>> {
    let mut move_queue = VecDeque::new();
    move_queue.push_back((start, Vec::new()));
    while let Some((pos, moves)) = move_queue.pop_front() {
        for mov in possible_moves(pos, &moves, key) {
            let mut new_moves = moves.clone();
            new_moves.push(mov);
            let new_pos = pos.make_move(mov);
            if new_pos == end {
                return Some(new_moves);
            }
            move_queue.push_back((new_pos, new_moves));
        }
    }
    None
}

fn parse_args() -> String {
    let matches = clap::App::new("Day 17")
        .author("Devon Hollowood")
        .arg(clap::Arg::with_name("key")
            .index(1)
            .help("key for generating puzzle")
            .required(true)
            .takes_value(true))
        .get_matches();
    matches.value_of("key").unwrap().to_owned()
}

fn main() {
    let key = parse_args();
    println!("part 1 path:");
    match shortest_path(Point::new(0, 0), Point::new(3, 3), &key) {
        Some(path) => {
            for mov in &path {
                print!("{}", mov.as_char());
            }
            println!();
        }
        None => println!("No path found!"),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const START: Point = Point { x: 0, y: 0 };
    const END: Point = Point { x: 3, y: 3 };

    fn read_moves(s: &str) -> Vec<Move> {
        use Move::*;
        let mut moves = Vec::with_capacity(s.len());
        for ch in s.chars() {
            match ch {
                'U' => moves.push(Up),
                'D' => moves.push(Down),
                'L' => moves.push(Left),
                'R' => moves.push(Right),
                _ => panic!("bad char: {}", ch),
            }
        }
        moves
    }

    #[test]
    fn example_dead_end() {
        assert_eq!(shortest_path(START, END, "hijkl"), None);
    }

    #[test]
    fn example1() {
        assert_eq!(shortest_path(START, END, "ihgpwlah"),
                   Some(read_moves("DDRRRD")));
    }

    #[test]
    fn example2() {
        assert_eq!(shortest_path(START, END, "kglvqrro"),
                   Some(read_moves("DDUDRLRRUDRD")));
    }

    #[test]
    fn example3() {
        assert_eq!(shortest_path(START, END, "ulqzkmiv"),
                   Some(read_moves("DRURDRUDDLLDLUURRDULRLDUUDDDRR")));
    }
}
