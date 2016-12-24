#[macro_use]
extern crate clap;

use std::io::prelude::*;
use std::fs::File;
use std::error::Error;

#[derive(Debug, PartialEq, Eq)]
enum ParseExpansionError {
    BadChar { expected: char, got: char },
    BadInt(std::num::ParseIntError),
    NotEnoughInput,
}

impl std::fmt::Display for ParseExpansionError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        match *self {
            ParseExpansionError::BadChar { expected: ref e, got: ref g } => {
                write!(f, "Expected {}, got {}", e, g)
            }
            ParseExpansionError::BadInt(ref e) => write!(f, "{}", e),
            ParseExpansionError::NotEnoughInput => {
                write!(f, "Not enough input")
            }
        }
    }
}

impl Error for ParseExpansionError {
    fn description(&self) -> &str {
        "Could not parse Expansion"
    }
    fn cause(&self) -> Option<&Error> {
        match *self {
            ParseExpansionError::BadInt(ref e) => Some(e),
            _ => None,
        }
    }
}

impl From<std::num::ParseIntError> for ParseExpansionError {
    fn from(err: std::num::ParseIntError) -> Self {
        ParseExpansionError::BadInt(err)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Expansion {
    width: usize,
    repetitions: usize,
}

fn read_expansion(input: &[u8])
                  -> Result<(Expansion, usize), ParseExpansionError> {
    use ParseExpansionError::*;
    // read '('
    if input.len() == 0 {
        return Err(NotEnoughInput);
    } else if input[0] != b'(' {
        return Err(BadChar {
            expected: '(',
            got: input[0] as char,
        });
    }
    // read `width`
    let mut idx = 1;
    while idx < input.len() && (input[idx] as char).is_digit(10) {
        idx += 1;
    }
    let width = std::str::from_utf8(&input[1..idx]).unwrap().parse()?;
    // read 'x'
    if input.len() < idx + 1 {
        return Err(NotEnoughInput);
    } else if input[idx] != b'x' {
        return Err(BadChar {
            expected: 'x',
            got: input[idx] as char,
        });
    }
    // read `repetitions`
    idx += 1;
    let repititions_start = idx;
    while idx < input.len() && (input[idx] as char).is_digit(10) {
        idx += 1;
    }
    let repetitions =
        std::str::from_utf8(&input[repititions_start..idx]).unwrap().parse()?;
    if input.len() < idx + 1 {
        return Err(NotEnoughInput);
    } else if input[idx] != b')' {
        return Err(BadChar {
            expected: ')',
            got: input[idx] as char,
        });
    }
    Ok((Expansion {
        width: width,
        repetitions: repetitions,
    },
        idx + 1))
}

fn decompress_a(input: &[u8]) -> Vec<u8> {
    let mut out = Vec::new();
    let mut idx = 0;
    while idx < input.len() {
        if let Ok((Expansion { width, repetitions }, exp_len)) =
               read_expansion(&input[idx..]) {
            idx += exp_len;
            for _ in 0..repetitions {
                out.extend_from_slice(&input[idx..idx +
                                                  std::cmp::min(width,
                                                                input.len() -
                                                                idx)]);
            }
            idx += width;
        } else {
            out.push(input[idx]);
            idx += 1;
        }
    }
    out
}

fn decompressed_b_len(input: &[u8]) -> usize {
    let mut len = 0;
    let mut idx = 0;
    while idx < input.len() {
        if let Ok((Expansion { width, repetitions }, exp_len)) =
               read_expansion(&input[idx..]) {
            idx += exp_len;
            let sub_len =
                decompressed_b_len(&input[idx..idx +
                                               std::cmp::min(width,
                                                             input.len() -
                                                             idx)]);
            len += sub_len * repetitions;
            idx += width;
        } else {
            if !(input[idx] as char).is_whitespace() {
                len += 1;
            }
            idx += 1;
        }
    }
    len
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
    let decompressed = decompress_a(compressed.as_bytes());
    println!("Decompressed length (first algorithm): {}",
             decompressed.iter()
                 .filter(|c| !(**c as char).is_whitespace())
                 .count());
    println!("Decompressed length (second algorithm): {}",
             decompressed_b_len(compressed.as_bytes()));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn decompress_a_1() {
        assert_eq!(&decompress_a(b"ADVENT"), b"ADVENT")
    }

    #[test]
    fn decompress_a_2() {
        assert_eq!(&decompress_a(b"A(1x5)BC"), b"ABBBBBC")
    }

    #[test]
    fn decompress_a_3() {
        assert_eq!(&decompress_a(b"(3x3)XYZ"), b"XYZXYZXYZ")
    }

    #[test]
    fn decompress_a_4() {
        assert_eq!(&decompress_a(b"A(2x2)BCD(2x2)EFG"), b"ABCBCDEFEFG")
    }

    #[test]
    fn decompress_a_5() {
        assert_eq!(&decompress_a(b"(6x1)(1x3)A"), b"(1x3)A")
    }

    #[test]
    fn decompress_a_6() {
        assert_eq!(&decompress_a(b"X(8x2)(3x3)ABCY"), b"X(3x3)ABC(3x3)ABCY")
    }

    #[test]
    fn decompress_a_not_enough_input() {
        assert_eq!(&decompress_a(b"(3x3)XY"), b"XYXYXY");
        assert_eq!(&decompress_a(b"(3x3)"), b"")
    }

    #[test]
    fn read_expansion_success() {
        assert_eq!(read_expansion(b"(123x456)foo"),
                   Ok((Expansion {
                       width: 123,
                       repetitions: 456,
                   },
                       9)))
    }

    #[test]
    fn read_expansion_not_enough_input() {
        assert_eq!(read_expansion(b""),
                   Err(ParseExpansionError::NotEnoughInput));
        assert_eq!(read_expansion(b"(1"),
                   Err(ParseExpansionError::NotEnoughInput));
        assert_eq!(read_expansion(b"(1x2"),
                   Err(ParseExpansionError::NotEnoughInput));
    }

    #[test]
    fn read_expansion_expected() {
        assert_eq!(read_expansion(b"1"),
                   Err(ParseExpansionError::BadChar {
                       expected: '(',
                       got: '1',
                   }));
        assert_eq!(read_expansion(b"(1,"),
                   Err(ParseExpansionError::BadChar {
                       expected: 'x',
                       got: ',',
                   }));
        assert_eq!(read_expansion(b"(1x2x"),
                   Err(ParseExpansionError::BadChar {
                       expected: ')',
                       got: 'x',
                   }));
    }

    #[test]
    fn read_expansion_bad_int() {
        let empty_int_error = "".parse::<usize>().unwrap_err();
        assert_eq!(read_expansion(b"(x456)"),
                   Err(ParseExpansionError::BadInt(empty_int_error.clone())));
        assert_eq!(read_expansion(b"(123x)"),
                   Err(ParseExpansionError::BadInt(empty_int_error)));
    }

    #[test]
    fn decompress_b_len_1() {
        assert_eq!(decompressed_b_len(b"(3x3)XYZ"), 9);
    }

    #[test]
    fn decompress_b_len_2() {
        assert_eq!(decompressed_b_len(b"X(8x2)(3x3)ABCY"), 20);
    }

    #[test]
    fn decompress_b_len_3() {
        assert_eq!(decompressed_b_len(b"(27x12)(20x12)(13x14)(7x10)(1x12)A"),
                   241920);
    }

    #[test]
    fn decompress_b_len_4() {
        assert_eq!(decompressed_b_len(
                b"(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN"),
                   445);
    }
}
