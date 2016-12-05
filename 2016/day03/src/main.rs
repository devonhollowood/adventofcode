extern crate clap;

use std::io::prelude::*;
use std::io::{Error, ErrorKind};
use std::fs::File;

struct Triangle(u64, u64, u64);

impl Triangle {
    fn is_valid(&self) -> bool {
        let &Triangle(a, b, c) = self;
        let mut s = [a, b, c];
        s.sort();
        s[0] + s[1] > s[2]
    }
}

fn invalid_data(msg: &str) -> Error {
    Error::new(ErrorKind::InvalidData, msg)
}

fn parse_args() -> std::io::Result<Box<Read>> {
    let source = clap::App::new("Day 03")
        .author("Devon Hollowood")
        .arg(clap::Arg::with_name("triangles")
             .index(1)
             .short("f")
             .long("triangles")
             .help("file to read triangles from. Reads from stdin otherwise")
             .takes_value(true)
        )
        .get_matches()
        .value_of_os("triangles")
        .map(|str| str.to_owned());
    match source {
        Some(filename) => Ok(Box::new(File::open(filename)?)),
        None => Ok(Box::new(std::io::stdin())),
    }
}

fn read_triangles<R: Read>(source: &mut R)
    -> std::io::Result<Vec<Triangle>>
{
    let mut contents = String::new();
    println!("Reading contents");
    source.read_to_string(&mut contents)?;
    println!("Processing");
    contents.lines().map(
        |line| {
            let sides: Vec<u64> = line.split_whitespace()
                .map(|side| {
                    side.parse()
                        .map_err(|_| invalid_data(&format!("Bad side: {}", side)))
                }).collect::<Result<_, _>>()?;
            println!("Read {}", line);
            if sides.len() != 3 {
                Err(invalid_data(&format!("Invalid line: {}", line)))
            } else {
                Ok(Triangle(sides[0], sides[1], sides[2]))
            }
        }
    ).collect()
}

fn main() {
    let mut source = parse_args()
        .unwrap_or_else(|err| panic!("Error reading file: {}", err));
    let n_valid = read_triangles(&mut source)
        .unwrap_or_else(|err| panic!("Error reading triangles: {}", err))
        .iter()
        .filter(|t| t.is_valid())
        .count();
    println!("# of valid triangles: {}", n_valid);
}
