#[macro_use]
extern crate clap;

#[macro_use]
extern crate lazy_static;

extern crate regex;

use std::io::prelude::*;
use std::fs::File;
use regex::Regex;
use std::collections::HashSet;

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
    Out(Register),
}

impl std::str::FromStr for Instruction {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use Instruction::*;
        lazy_static! {
            static ref CPY_RE: Regex =
                Regex::new(r"cpy (?P<val>[a-d]|-?\d+) (?P<reg>[a-d])")
                .expect("Bad regex CPY_RE");
            static ref INC_RE: Regex =
                Regex::new(r"inc (?P<reg>[a-d])").expect("Bad regex INC_RE");
            static ref DEC_RE: Regex =
                Regex::new(r"dec (?P<reg>[a-d])").expect("Bad regex DEC_RE");
            static ref JNZ_RE: Regex =
                Regex::new(r"jnz (?P<val>[a-d]|-?\d+) (?P<shift>-?\d+)")
                .expect("Bad regex JNZ_RE");
            static ref OUT_RE: Regex =
                Regex::new(r"out (?P<reg>[a-d])").expect("Bad regex OUT_RE");
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
        } else if let Some(cap) = OUT_RE.captures(s) {
            let reg = read_reg(&cap["reg"]);
            Ok(Out(reg))
        } else {
            Err(format!("Invalid instruction: {}", s))
        }
    }
}

#[derive(Debug)]
struct ComputerStream {
    registers: [i64; 4],
    tape: Vec<Instruction>,
    ip: usize,
}

impl ComputerStream {
    fn new<I>(register_a: i64, instructions: I) -> ComputerStream
        where I: IntoIterator<Item = Instruction>
    {
        ComputerStream {
            registers: [register_a, 0, 0, 0],
            tape: instructions.into_iter().collect(),
            ip: 0,
        }
    }
    fn registers(&self) -> [i64; 4] {
        self.registers
    }
}

impl Iterator for ComputerStream {
    type Item = i64;
    fn next(&mut self) -> Option<Self::Item> {
        use Instruction::*;
        use Value::*;
        while self.ip < self.tape.len() {
            match self.tape[self.ip] {
                Cpy(Reg(src), dst) => {
                    self.registers[dst] = self.registers[src];
                    self.ip += 1;
                }
                Cpy(Lit(val), dst) => {
                    self.registers[dst] = val;
                    self.ip += 1;
                }
                Inc(reg) => {
                    self.registers[reg] += 1;
                    self.ip += 1;
                }
                Dec(reg) => {
                    self.registers[reg] -= 1;
                    self.ip += 1;
                }
                Jnz(val, shift) => {
                    let cmp = match val {
                        Reg(reg) => self.registers[reg],
                        Lit(lit) => lit,
                    };
                    if cmp != 0 {
                        if shift < 0 {
                            self.ip -= (-shift) as usize;
                        } else {
                            self.ip += shift as usize;
                        }
                    } else {
                        self.ip += 1;
                    }
                }
                Out(reg) => {
                    self.ip += 1;
                    return Some(self.registers[reg]);
                }
            }
        }
        None
    }
}

fn makes_suitable_clock<I>(seed: i64, instructions: I) -> bool
    where I: IntoIterator<Item = Instruction>
{
    let mut stream = ComputerStream::new(seed, instructions);
    let mut expected = 0;
    let mut states = HashSet::new();
    while let Some(val) = stream.next() {
        if val != expected {
            return false;
        }
        let new_state = stream.registers();
        if states.contains(&new_state) {
            return true;
        }
        states.insert(new_state);
        expected = if expected == 0 { 1 } else { 0 };
    }
    false
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
    for seed in 1.. {
        if makes_suitable_clock(seed, instructions.iter().cloned()) {
            println!("Part 1 seed: {}", seed);
            break;
        }
    }
}
