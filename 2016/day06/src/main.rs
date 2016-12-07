extern crate clap;

use std::io::prelude::*;
use std::fs::File;
use std::collections::HashMap;

fn parse_args() -> std::io::Result<Box<Read>> {
    let source = clap::App::new("Day 06")
        .author("Devon Hollowood")
        .arg(clap::Arg::with_name("messages")
             .index(1)
             .short("f")
             .long("messages")
             .help("file to read rooms from. Reads from stdin otherwise")
             .takes_value(true)
        )
        .get_matches()
        .value_of_os("messages")
        .map(|str| str.to_owned());
    match source {
        Some(filename) => Ok(Box::new(File::open(filename)?)),
        None => Ok(Box::new(std::io::stdin())),
    }
}

fn read_messages<R: Read>(source: &mut R) -> std::io::Result<Vec<String>> {
    let mut contents = String::new();
    source.read_to_string(&mut contents)?;
    Ok(contents.lines().map(|line| line.trim().to_owned()).collect())
}

fn unfuzz(messages: &[String]) -> String {
    if messages.is_empty() {
        return String::new();
    }
    let mut freqs = Vec::new();
    for message in messages.iter() {
        for (idx, ch) in message.char_indices() {
            if freqs.len() <= idx {
                freqs.resize(idx + 1, HashMap::new());
            }
            *freqs[idx].entry(ch).or_insert(0) += 1;
        }
    }
    freqs.iter().map(
        |set| set.iter().max_by_key(|&(_, v)| v).unwrap().0
    ).cloned().collect()
}

fn main() {
    let mut source = parse_args()
        .unwrap_or_else(|err| panic!("Error reading file: {}", err));
    let messages = read_messages(&mut source)
        .unwrap_or_else(|err| panic!("Error reading messages: {}", err));
    println!("Unfuzzed message: {}", unfuzz(&messages));
}
