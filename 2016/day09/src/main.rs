#[macro_use]
extern crate clap;

use std::io::prelude::*;
use std::fs::File;

#[derive(Debug)]
struct ReadError {
    reason: String,
}

impl ReadError {
    fn new(error: &str, position: usize) -> ReadError {
        ReadError {
            reason: format!("Error at position {}: {}", position, error),
        }
    }
}

fn read_usize(input: &[u8],
              position: usize)
              -> Result<(usize, usize), ReadError> {
    let mut end = 0;
    while (input[end] as char).is_digit(10) {
        end += 1;
    }
    let read = std::str::from_utf8(&input[..end])
        .map_err(|e| ReadError::new(&format!("Bad read: {}", e), position))?
        .parse()
        .map_err(|e| ReadError::new(&format!("Bad read: {}", e), position))?;
    Ok((read, end))
}

fn decompress(input: &[u8]) -> Result<Vec<u8>, ReadError> {
    let mut out = Vec::new();
    let mut idx = 0;
    while idx < input.len() {
        if input[idx] == '(' as u8 {
            idx += 1;
            if idx >= input.len() {
                return Err(ReadError::new("Unexpected end", idx));
            }
            let (rep_length, offset) = read_usize(&input[idx..], idx)?;
            idx += offset;
            if idx >= input.len() {
                return Err(ReadError::new("Unexpected end", idx));
            }
            if input[idx] as char != 'x' {
                return Err(ReadError::new(&format!("Expected 'x', got {}",
                                                   input[idx]),
                                          idx));
            }
            idx += 1;
            let (n_reps, offset) = read_usize(&input[idx..], idx)?;
            idx += offset;
            if idx >= input.len() {
                return Err(ReadError::new("Unexpected end", idx));
            }
            if input[idx] as char != ')' {
                return Err(ReadError::new(&format!("Expected ')', got {}",
                                                   input[idx]),
                                          idx));
            }
            idx += 1;
            if idx + rep_length > input.len() {
                return Err(ReadError::new("Unexpected end", idx));
            }
            for _ in 0..n_reps {
                out.extend_from_slice(&input[idx..idx + rep_length]);
            }
            idx += rep_length;
        } else {
            out.push(input[idx]);
            idx += 1;
        }
    }
    Ok(out)
}

fn parse_args() -> std::io::Result<String> {
    let matches = clap::App::new("Day 09")
        .author("Devon Hollowood")
        .arg(clap::Arg::with_name("file")
            .index(1)
            .short("f")
            .long("file")
            .help("file to decompress. Reads from stdin otherwise")
            .takes_value(true))
        .get_matches();
    let source = matches.value_of_os("file");
    let mut contents = String::new();
    match source {
        Some(filename) => {
            let mut input = File::open(filename)?;
            input.read_to_string(&mut contents)?;
        }
        None => {
            let mut input = std::io::stdin();
            input.read_to_string(&mut contents)?;
        }
    }
    Ok(contents)
}

fn main() {
    let compressed = parse_args()
        .unwrap_or_else(|err| panic!("Error reading input: {}", err));
    let decompressed = match decompress(compressed.as_bytes()) {
        Ok(d) => d,
        Err(e) => panic!(e.reason),
    };
    println!("Decompressed length: {}",
             decompressed.iter()
                 .filter(|c| !(**c as char).is_whitespace())
                 .count());
}

#[cfg(test)]
mod tests {
    use super::decompress;

    #[test]
    fn t1() {
        assert_eq!(&decompress(b"ADVENT").unwrap(), b"ADVENT")
    }

    #[test]
    fn t2() {
        assert_eq!(&decompress(b"A(1x5)BC").unwrap(), b"ABBBBBC")
    }

    #[test]
    fn t3() {
        assert_eq!(&decompress(b"(3x3)XYZ").unwrap(), b"XYZXYZXYZ")
    }

    #[test]
    fn t4() {
        assert_eq!(&decompress(b"A(2x2)BCD(2x2)EFG").unwrap(), b"ABCBCDEFEFG")
    }

    #[test]
    fn t5() {
        assert_eq!(&decompress(b"(6x1)(1x3)A").unwrap(), b"(1x3)A")
    }

    #[test]
    fn t6() {
        assert_eq!(&decompress(b"X(8x2)(3x3)ABCY").unwrap(),
                   b"X(3x3)ABC(3x3)ABCY")
    }
}
