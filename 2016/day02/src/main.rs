extern crate clap;

use std::io::prelude::*;
use std::io::{Error, ErrorKind};
use std::fs::File;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Direction {
    Up,
    Down,
    Left,
    Right,
}

struct Key(u8);

impl Key {
    fn new() -> Key {
        Key(5)
    }
    fn shift(&self, dir: Direction) -> Key {
        use Direction::*;
        let &Key(n) = self;
        match dir {
            Up =>
                match n {
                    1 | 2 | 3 => Key(n),
                    _ => Key(n - 3),
                },
            Down =>
                match n {
                    7 | 8 | 9 => Key(n),
                    _ => Key(n + 3),
                },
            Left =>
                match n {
                    1 | 4 | 7 => Key(n),
                    _ => Key(n - 1),
                },
            Right =>
                match n {
                    3 | 6 | 9 => Key(n),
                    _ => Key(n + 1),
                },
        }
    }
}

fn invalid_data(msg: &str) -> Error {
    Error::new(ErrorKind::InvalidData, msg)
}

fn parse_args() -> std::io::Result<Box<Read>> {
    let source = clap::App::new("Day 02")
        .author("Devon Hollowood")
        .arg(clap::Arg::with_name("instructions")
             .index(1)
             .short("f")
             .long("instructions")
             .help("file to read instructions from. Reads from stdin otherwise")
             .takes_value(true)
        )
        .get_matches()
        .value_of_os("instructions")
        .map(|str| str.to_owned());
    match source {
        Some(filename) => Ok(Box::new(File::open(filename)?)),
        None => Ok(Box::new(std::io::stdin())),
    }
}

fn read_instructions<R: Read>(source: &mut R)
    -> std::io::Result<Vec<Vec<Direction>>>
{
    use Direction::*;
    let mut contents = String::new();
    source.read_to_string(&mut contents)?;
    contents.lines().map(
        |line| line.trim().chars().map(
            |c| match c {
                'U' => Ok(Up),
                'D' => Ok(Down),
                'L' => Ok(Left),
                'R' => Ok(Right),
                _ => Err(invalid_data(&format!("Invalid direction: {}", c))),
            }
        ).collect()
    ).collect()
}

fn main() {
    let mut source = parse_args()
        .unwrap_or_else(|err| panic!("Error reading file: {}", err));
    let mut keycode = String::new();
    for key_instructions in read_instructions(&mut source)
        .unwrap_or_else(|err| panic!("Error reading instructions: {}", err))
    {
        let mut current_key = Key::new();
        for instruction in key_instructions {
            current_key = current_key.shift(instruction);
        }
        keycode.push((current_key.0 + '0' as u8) as char);
    }
    println!("Keycode: {}", keycode);
}
