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

fn read_triangles_horizontal(contents: &str) -> std::io::Result<Vec<Triangle>> {
    contents.lines().map(
        |line| {
            let sides: Vec<u64> = line.split_whitespace()
                .map(|side| {
                    side.parse()
                        .map_err(|_| invalid_data(&format!("Bad side: {}", side)))
                }).collect::<Result<_, _>>()?;
            if sides.len() != 3 {
                Err(invalid_data(&format!("Invalid line: {}", line)))
            } else {
                Ok(Triangle(sides[0], sides[1], sides[2]))
            }
        }
    ).collect()
}

fn read_triangles_vertical(contents: &str) -> std::io::Result<Vec<Triangle>> {
    let mut vecs = Vec::new();
    for line in contents.lines() {
        let sides: Vec<u64> = line.split_whitespace()
            .map(|side| {
                side.parse()
                    .map_err(|_| invalid_data(&format!("Bad side: {}", side)))
            }).collect::<Result<_, _>>()?;
        if vecs.len() < sides.len() {
            vecs.resize(sides.len(), Vec::new());
        }
        for (n, val) in sides.into_iter().enumerate() {
            vecs[n].push(val);
        }
    }
    let side_list: Vec<_> =
        vecs.into_iter().fold(Vec::new(), |mut v1, v2| { v1.extend(v2); v1 });
    let mut triangles = Vec::new();
    for sides in side_list.chunks(3) {
        if sides.len() != 3 {
            return Err(invalid_data("Incomplete triangle"))
        } else {
            triangles.push(Triangle(sides[0], sides[1], sides[2]));
        }
    }
    Ok(triangles)
}

fn main() {
    let mut source = parse_args()
        .unwrap_or_else(|err| panic!("Error reading file: {}", err));
    let mut contents = String::new();
    source.read_to_string(&mut contents).expect("Could not read input");
    let n_valid_horizontal = read_triangles_horizontal(&contents)
        .unwrap_or_else(|err| panic!("Error reading triangles: {}", err))
        .iter()
        .filter(|t| t.is_valid())
        .count();
    let n_valid_vertical = read_triangles_vertical(&contents)
        .unwrap_or_else(|err| panic!("Error reading triangles: {}", err))
        .iter()
        .filter(|t| t.is_valid())
        .count();
    println!("# of valid triangles (horizontal): {}", n_valid_horizontal);
    println!("# of valid triangles (vertical): {}", n_valid_vertical);
}
