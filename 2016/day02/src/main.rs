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

struct Key(char);

impl Key {
    fn new() -> Key {
        Key('5')
    }
    fn shift_1(&self, dir: Direction) -> Key {
        use Direction::*;
        let &Key(n) = self;
        match dir {
            Up =>
                match n as u8 - '0' as u8 {
                    1 | 2 | 3 => Key(n),
                    _ => Key((n as u8 - 3) as char),
                },
            Down =>
                match n as u8 - '0' as u8 {
                    7 | 8 | 9 => Key(n),
                    _ => Key((n as u8 + 3) as char),
                },
            Left =>
                match n as u8 - '0' as u8 {
                    1 | 4 | 7 => Key(n),
                    _ => Key((n as u8 - 1) as char),
                },
            Right =>
                match n as u8 - '0' as u8 {
                    3 | 6 | 9 => Key(n),
                    _ => Key((n as u8 + 1) as char),
                },
        }
    }
    fn shift_2(&self, dir: Direction) -> Key {
        use Direction::*;
        let &Key(key) = self;
        match dir {
            Up =>
                match key {
                    '1' | '2' | '4' | '5' | '9' => Key(key),
                    '3' => Key('1'),
                    '6' => Key('2'),
                    '7' => Key('3'),
                    '8' => Key('4'),
                    'A' => Key('6'),
                    'B' => Key('7'),
                    'C' => Key('8'),
                    'D' => Key('B'),
                    _ => panic!("Invalid key: {}", key)
                },
            Down =>
                match key {
                    '5' | '9' | 'A' | 'C' | 'D' => Key(key),
                    '1' => Key('3'),
                    '2' => Key('6'),
                    '3' => Key('7'),
                    '4' => Key('8'),
                    '6' => Key('A'),
                    '7' => Key('B'),
                    '8' => Key('C'),
                    'B' => Key('D'),
                    _ => panic!("Invalid key: {}", key)
                },
            Left =>
                match key {
                    '1' | '2' | '5' | 'A' | 'D' => Key(key),
                    '3' | '4' => Key((key as u8 - 1) as char),
                    '6' ... '9' => Key((key as u8 - 1) as char),
                    'B' | 'C' => Key((key as u8 - 1) as char),
                    _ => panic!("Invalid key: {}", key)
                },
            Right =>
                match key {
                    '1' | '4' | '9' | 'C' | 'D' => Key(key),
                    '2' | '3' => Key((key as u8 + 1) as char),
                    '5' ... '8' => Key((key as u8 + 1) as char),
                    'A' | 'B' => Key((key as u8 + 1) as char),
                    _ => panic!("Invalid key: {}", key)
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
    let mut keycode_1 = String::new();
    let mut keycode_2 = String::new();
    for key_instructions in read_instructions(&mut source)
        .unwrap_or_else(|err| panic!("Error reading instructions: {}", err))
    {
        let mut current_key_1 = Key::new();
        let mut current_key_2 = Key::new();
        for instruction in key_instructions {
            current_key_1 = current_key_1.shift_1(instruction);
            current_key_2 = current_key_2.shift_2(instruction);
        }
        keycode_1.push(current_key_1.0);
        keycode_2.push(current_key_2.0);
    }
    println!("Keycode 1: {}", keycode_1);
    println!("Keycode 2: {}", keycode_2);
}
