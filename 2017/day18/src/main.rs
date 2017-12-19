extern crate structopt;
#[macro_use]
extern crate structopt_derive;

use structopt::StructOpt;
use std::collections::HashMap;
use std::error::Error;
use std::fs::File;
use std::io::Read;
use std::path::PathBuf;

type Literal = i64;
type Address = char;

#[derive(Debug)]
struct ParseError {
    bad_val: String,
    underlying: Option<Box<Error>>,
}

impl ParseError {
    fn new(
        bad_val: std::borrow::Cow<str>,
        underlying: Option<Box<Error>>,
    ) -> ParseError {
        ParseError {
            bad_val: bad_val.into_owned(),
            underlying: underlying,
        }
    }
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        write!(f, "{}", self.description())
    }
}

impl Error for ParseError {
    fn description(&self) -> &str {
        "Could not parse the value as asked"
    }

    fn cause(&self) -> Option<&Error> {
        match self.underlying {
            Some(ref e) => Some(e.as_ref()),
            None => None,
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum Value {
    Literal(Literal),
    Address(Address),
}

impl std::str::FromStr for Value {
    type Err = ParseError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        i64::from_str_radix(s, 10).map(Value::Literal).or_else(|_| {
            if s.len() == 1 {
                Ok(Value::Address(s.chars().next().unwrap()))
            } else {
                Err(ParseError::new(s.into(), None))
            }
        })
    }
}

enum Operation {
    Sound(Value),
    Set(Address, Value),
    Add(Address, Value),
    Mul(Address, Value),
    Mod(Address, Value),
    Recover(Value),
    Jgz(Value, Value),
}

impl std::str::FromStr for Operation {
    type Err = ParseError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use Operation::*;
        let err = ParseError::new(s.into(), None);
        let parts = s.split_whitespace().collect::<Vec<_>>();
        let assert_len = |len: usize| {
            if parts.len() == len {
                Ok(())
            } else {
                Err(ParseError::new(s.into(), None))
            }
        };
        match parts.first() {
            Some(&"snd") => {
                assert_len(2)?;
                Ok(Sound(parts[1].parse()?))
            }
            Some(&"set") => {
                assert_len(3)?;
                Ok(Set(parts[1].chars().next().ok_or(err)?, parts[2].parse()?))
            }
            Some(&"add") => {
                assert_len(3)?;
                Ok(Add(parts[1].chars().next().ok_or(err)?, parts[2].parse()?))
            }
            Some(&"mul") => {
                assert_len(3)?;
                Ok(Mul(parts[1].chars().next().ok_or(err)?, parts[2].parse()?))
            }
            Some(&"mod") => {
                assert_len(3)?;
                Ok(Mod(parts[1].chars().next().ok_or(err)?, parts[2].parse()?))
            }
            Some(&"rcv") => {
                assert_len(2)?;
                Ok(Recover(parts[1].parse()?))
            }
            Some(&"jgz") => {
                assert_len(3)?;
                Ok(Jgz(parts[1].parse()?, parts[2].parse()?))
            }
            _ => Err(ParseError::new(s.into(), None)),
        }
    }
}

fn parse(input: &str) -> Result<Vec<Operation>, ParseError> {
    input.lines().map(|line| line.parse()).collect()
}

fn part1(instructions: &[Operation]) -> Option<Literal> {
    let mut computer = HashMap::new();
    let mut sounds = Vec::new();
    let mut instr = 0i64;
    let get = |v: Value, cpu: &HashMap<Address, Literal>| match v {
        Value::Literal(l) => l,
        Value::Address(a) => *cpu.get(&a).unwrap_or(&0),
    };
    while 0 <= instr && (instr as usize) < instructions.len() {
        use Operation::*;
        match instructions[instr as usize] {
            Sound(v) => sounds.push(get(v, &computer)),
            Set(addr, v) => {
                *computer.entry(addr).or_insert(0) = get(v, &computer)
            }
            Add(addr, v) => {
                *computer.entry(addr).or_insert(0) += get(v, &computer)
            }
            Mul(addr, v) => {
                *computer.entry(addr).or_insert(0) *= get(v, &computer)
            }
            Mod(addr, v) => {
                *computer.entry(addr).or_insert(0) %= get(v, &computer)
            }
            Recover(v) => {
                if get(v, &computer) != 0 {
                    return sounds.last().cloned();
                }
            }
            Jgz(v, jump) => {
                if get(v, &computer) > 0 {
                    instr += get(jump, &computer);
                    continue;
                }
            }
        }
        instr += 1;
    }
    None
}

fn main() {
    let opt = Opt::from_args();
    let mut contents = String::new();
    if opt.input.to_str() == Some("-") {
        std::io::stdin()
            .read_to_string(&mut contents)
            .expect("could not read stdin");
    } else {
        let mut file = File::open(&opt.input)
            .expect(&format!("file {} not found", opt.input.display()));
        file.read_to_string(&mut contents)
            .expect(&format!("could not read file {}", opt.input.display()));
    }
    let instructions = parse(&contents).expect("Could not parse instructions");
    println!(
        "Part 1: {}",
        part1(&instructions).expect("Could not recover a sound!")
    );
}

#[derive(StructOpt, Debug)]
#[structopt(name = "day13", about = "Advent of code 2017 day 13")]
struct Opt {
    #[structopt(help = "Input file", parse(from_os_str))] input: PathBuf,
}

#[cfg(test)]
mod tests {
    use super::*;

    static INPUT: &str = "set a 1\n\
                          add a 2\n\
                          mul a a\n\
                          mod a 5\n\
                          snd a\n\
                          set a 0\n\
                          rcv a\n\
                          jgz a -1\n\
                          set a 1\n\
                          jgz a -2";

    #[test]
    fn part1_test() {
        let instructions = parse(INPUT).unwrap();
        assert_eq!(part1(&instructions), Some(4));
    }
}
