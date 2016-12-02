extern crate clap;

use std::io::prelude::*;
use std::fs::File;

#[derive(Clone, Copy, Debug)]
enum Turn {
    Left,
    Right,
}

#[derive(Clone, Copy, Debug)]
enum Direction {
    North,
    East,
    South,
    West,
}

impl Direction {
    fn rotate(self, turn: Turn) -> Direction {
        use Turn::*;
        use Direction::*;
        match turn {
            Left => match self {
                North => West,
                East => North,
                South => East,
                West => South,
            },
            Right => match self {
                North => East,
                East => South,
                South => West,
                West => North,
            },
        }
    }
}

#[derive(Clone, Copy, Debug)]
struct Position {
    x: i64,
    y: i64,
}

#[derive(Clone, Copy, Debug)]
struct Traveller {
    pos: Position,
    facing: Direction,
}

impl Traveller {
    fn new() -> Traveller {
        Traveller { pos: Position {x: 0, y: 0}, facing: Direction::North }
    }
    fn rotate(&mut self, turn: Turn) {
        self.facing = self.facing.rotate(turn);
    }
    fn travel(&mut self, distance: u64) {
        match self.facing {
            Direction::North => self.pos.y += distance as i64,
            Direction::East => self.pos.x += distance as i64,
            Direction::South => self.pos.y -= distance as i64,
            Direction::West => self.pos.x -= distance as i64,
        }
    }
    fn distance_from_origin(&self) -> u64 {
        self.pos.x.wrapping_abs() as u64 + self.pos.y.wrapping_abs() as u64
    }
}

struct Instruction {
    rotation: Turn,
    distance: u64,
}

impl std::str::FromStr for Instruction {
    type Err = std::io::Error;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let s = s.trim();
        fn invalid_data(desc: &str) -> std::io::Error {
           std::io::Error::new(std::io::ErrorKind::InvalidData, desc)
        }
        if s.len() == 0 {
            return Err(invalid_data("Zero-length instruction"));
        }
        let (rot_str, dist_str) = s.split_at(1);
        let rot = match rot_str {
            "L" => Turn::Left,
            "R" => Turn::Right,
            _ => return Err(invalid_data(&format!("Invalid rotation: {}", rot_str))),
        };
        let dist = dist_str.parse()
            .map_err(|e| invalid_data(
                &format!("Invalid distance: {} ({})", dist_str, e)))?;
        Ok(Instruction { rotation: rot, distance: dist })
    }
}

fn parse_args() -> std::io::Result<Box<Read>> {
    let source = clap::App::new("Day 01")
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

fn read_instructions<R: Read>(source: &mut R) -> std::io::Result<Vec<Instruction>> {
    let mut instruction_str = String::new();
    source.read_to_string(&mut instruction_str)?;
    instruction_str.split(", ").map(|instr| instr.parse()).collect()
}

fn main() {
    let mut source = parse_args()
        .unwrap_or_else(|err| panic!("Error reading file: {}", err));
    let mut santa = Traveller::new();
    for instruction in read_instructions(&mut source)
        .unwrap_or_else(|err| panic!("Error reading instructions: {}", err))
    {
        santa.rotate(instruction.rotation);
        santa.travel(instruction.distance);
    }
    println!("Distance traveled: {}", santa.distance_from_origin());
}
