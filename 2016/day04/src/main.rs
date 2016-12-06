extern crate clap;

use std::io::prelude::*;
use std::io::{Error, ErrorKind};
use std::fs::File;
use std::collections::hash_map::HashMap;
use std::ascii::AsciiExt;
use std::cmp::Ordering;

#[derive(Debug)]
struct Room {
    name: String,
    id: u64,
    checksum: String,
}

impl Room {
    fn is_valid(&self) -> bool {
        self.checksum == self.calculate_checksum()
    }
    fn calculate_checksum(&self) -> String {
        let mut letter_freqs = HashMap::new();
        for letter in self.name.chars() {
            if letter.is_ascii() && letter.is_lowercase() {
                *letter_freqs.entry(letter).or_insert(0) += 1;
            }
        }
        let mut letters: Vec<char> = letter_freqs.keys().cloned().collect();
        letters.sort_by(
            // want descending order in frequency
            |a, b| match letter_freqs[b].cmp(&letter_freqs[a]) {
                Ordering::Equal => a.cmp(b),
                other => other,
            }
        );
        letters.into_iter().take(5).collect()
    }
    fn decrypt(&self) -> String {
        fn rotate(ch: char, dist: u64) -> char {
            match ch {
                '-' => ' ',
                'a'...'z' =>
                    ((dist + ch as u64 - 'a' as u64) % 26 + 'a' as u64) as u8 as char,
                _ => ch,
            }
        }
        self.name.chars().map(|ch| rotate(ch, self.id)).collect()
    }
}

fn invalid_data(msg: &str) -> Error {
    Error::new(ErrorKind::InvalidData, msg)
}

fn parse_args() -> std::io::Result<Box<Read>> {
    let source = clap::App::new("Day 04")
        .author("Devon Hollowood")
        .arg(clap::Arg::with_name("rooms")
             .index(1)
             .short("f")
             .long("rooms")
             .help("file to read rooms from. Reads from stdin otherwise")
             .takes_value(true)
        )
        .get_matches()
        .value_of_os("rooms")
        .map(|str| str.to_owned());
    match source {
        Some(filename) => Ok(Box::new(File::open(filename)?)),
        None => Ok(Box::new(std::io::stdin())),
    }
}

fn read_rooms<R: Read>(source: &mut R) -> std::io::Result<Vec<Room>> {
    let mut contents = String::new();
    source.read_to_string(&mut contents)?;
    contents.lines().map(
        |line| {
            let line = line.trim();
            let (name, rest) = line.split_at(
                line.find(|ch: char| !(ch.is_ascii() && ch.is_lowercase() || ch == '-'))
                    .ok_or(invalid_data(&format!("Unnamed room: {}", line)))?
            );
            if !name.ends_with('-') {
                return Err(invalid_data(&format!("Malformed line: {}", line)));
            }
            let name = &name[0..name.len()-1];
            let (id, rest) = rest.split_at(
                rest.find(|ch: char| !ch.is_digit(10))
                    .ok_or(invalid_data(&format!("No-id room: {}", line)))?
            );
            if rest.len() != 7
                || !rest.starts_with('[')
                || !rest.ends_with(']')
                || !rest[1..6].chars().all(|ch| ch.is_ascii() && ch.is_lowercase())
            {
                return Err(invalid_data(&format!("Malformed checksum: {}", rest)));
            }
            let checksum = &rest[1..6];
            Ok(Room{
                name: name.to_owned(),
                id: id.parse().expect(&format!("somehow got non-numeric id: {}", id)),
                checksum: checksum.to_owned(),
            })
        }
    ).collect()
}

fn main() {
    let mut source = parse_args()
        .unwrap_or_else(|err| panic!("Error reading file: {}", err));
    let rooms = read_rooms(&mut source)
        .unwrap_or_else(|err| panic!("Error reading rooms: {}", err));
    let valid_room_sum: u64 = rooms.iter()
        .filter(|room| room.is_valid())
        .map(|room| room.id)
        .sum();
    println!("Sum of valid room numbers: {}", valid_room_sum);
    for room in rooms.iter().filter(|room| room.is_valid()) {
        println!("{}: {}", room.id, room.decrypt());
    }
}
