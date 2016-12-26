#[macro_use]
extern crate clap;

#[macro_use]
extern crate lazy_static;

extern crate regex;

use std::io::prelude::*;
use std::fs::File;
use regex::Regex;

type Register = usize;

#[derive(Debug, Clone, Copy)]
enum Value {
    Reg(Register),
    Lit(i64),
}

#[derive(Debug, Clone, Copy)]
enum Instruction {
    Cpy(Value, Register),
    Inc(Register),
    Dec(Register),
    Jnz(Value, isize),
}

impl std::str::FromStr for Instruction {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use Instruction::*;
        lazy_static! {
            static ref CPY_RE: Regex =
                Regex::new(r"cpy (?P<val>[a-d]|\d+) (?P<reg>[a-d])").unwrap();
            static ref INC_RE: Regex =
                Regex::new(r"inc (?P<reg>[a-d])").unwrap();
            static ref DEC_RE: Regex =
                Regex::new(r"dec (?P<reg>[a-d])").unwrap();
            static ref JNZ_RE: Regex =
                Regex::new(r"jnz (?P<val>[a-d]|\d+) (?P<shift>-?\d+)").unwrap();
        }
        fn read_reg(s: &str) -> Register {
            (s.as_bytes()[0] - b'a') as usize
        }
        fn read_val(s: &str) -> Value {
            if let Ok(lit) = s.parse() {
                Value::Lit(lit)
            } else {
                Value::Reg(read_reg(s))
            }
        }
        if let Some(cap) = CPY_RE.captures(s) {
            let val = read_val(&cap["val"]);
            let reg = read_reg(&cap["reg"]);
            Ok(Cpy(val, reg))
        } else if let Some(cap) = INC_RE.captures(s) {
            let reg = read_reg(&cap["reg"]);
            Ok(Inc(reg))
        } else if let Some(cap) = DEC_RE.captures(s) {
            let reg = read_reg(&cap["reg"]);
            Ok(Dec(reg))
        } else if let Some(cap) = JNZ_RE.captures(s) {
            let val = read_val(&cap["val"]);
            let shift = cap["shift"].parse().unwrap();
            Ok(Jnz(val, shift))
        } else {
            Err(format!("Invalid instruction: {}", s))
        }
    }
}

fn compute(tape: &[Instruction]) -> [i64; 4] {
    use Instruction::*;
    use Value::*;
    let mut registers = [0i64; 4];
    let mut ip = 0; // instruction pointer
    while ip < tape.len() {
        match tape[ip] {
            Cpy(Reg(src), dst) => registers[dst] = registers[src],
            Cpy(Lit(val), dst) => registers[dst] = val,
            Inc(reg) => registers[reg] += 1,
            Dec(reg) => registers[reg] -= 1,
            Jnz(val, shift) => {
                let cmp = match val {
                    Reg(reg) => registers[reg],
                    Lit(lit) => lit,
                };
                if cmp != 0 {
                    if shift < 0 {
                        ip -= (-shift) as usize;
                    } else {
                        ip += shift as usize;
                    }
                    continue;
                }
            }
        }
        ip += 1;
    }
    registers
}

fn parse_args() -> std::io::Result<String> {
    let matches = clap::App::new("Day 12")
        .author("Devon Hollowood")
        .arg(clap::Arg::with_name("instructions")
            .index(1)
            .short("f")
            .long("instructions")
            .help("file containing instructions to compute. Reads from stdin otherwise")
            .takes_value(true))
        .get_matches();
    let source = matches.value_of_os("instructions");
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
    let input = parse_args().unwrap_or_else(|err| panic!("Error reading args: {}", err));
    let instructions: Vec<_> = input.lines()
        .map(|line| line.trim().parse())
        .collect::<Result<_, _>>()
        .unwrap_or_else(|err| panic!("Error parsing instructions: {}", err));
    let final_state = compute(&instructions);
    println!("part 1 final state: {:?}", final_state);
}
