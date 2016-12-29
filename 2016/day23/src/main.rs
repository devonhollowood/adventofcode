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
    Cpy(Value, Value),
    Inc(Register),
    Dec(Register),
    Jnz(Value, Value),
    Tgl(Register),
}

impl std::str::FromStr for Instruction {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use Instruction::*;
        lazy_static! {
            static ref CPY_RE: Regex =
                Regex::new(r"cpy (?P<from>[a-d]|-?\d+) (?P<to>[a-d]|-?\d+)")
                .expect("Bad regex: CPY_RE");
            static ref INC_RE: Regex =
                Regex::new(r"inc (?P<reg>[a-d])")
                .expect("Bad regex: INC_RE");
            static ref DEC_RE: Regex =
                Regex::new(r"dec (?P<reg>[a-d])")
                .expect("Bad regex: DEC_RE");
            static ref JNZ_RE: Regex =
                Regex::new(r"jnz (?P<val>[a-d]|-?\d+) (?P<shift>[a-d]|-?\d+)")
                .expect("Bad regex: JNZ_RE");
            static ref TGL_RE: Regex =
                Regex::new(r"tgl (?P<reg>[a-d])")
                .expect("Bad regex: TGL_RE");
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
            let from = read_val(&cap["from"]);
            let to = read_val(&cap["to"]);
            Ok(Cpy(from, to))
        } else if let Some(cap) = INC_RE.captures(s) {
            let reg = read_reg(&cap["reg"]);
            Ok(Inc(reg))
        } else if let Some(cap) = DEC_RE.captures(s) {
            let reg = read_reg(&cap["reg"]);
            Ok(Dec(reg))
        } else if let Some(cap) = JNZ_RE.captures(s) {
            let val = read_val(&cap["val"]);
            let shift = read_val(&cap["shift"]);
            Ok(Jnz(val, shift))
        } else if let Some(cap) = TGL_RE.captures(s) {
            let reg = read_reg(&cap["reg"]);
            Ok(Tgl(reg))
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
    let mut tape: Vec<_> = tape.iter().cloned().collect();
    while ip < tape.len() {
        match tape[ip] {
            Cpy(Reg(src), Reg(dst)) => registers[dst] = registers[src],
            Cpy(Lit(val), Reg(dst)) => registers[dst] = val,
            Cpy(_, Lit(_)) => {}
            Inc(reg) => registers[reg] += 1,
            Dec(reg) => registers[reg] -= 1,
            Jnz(cmp_val, shift_val) => {
                let cmp = match cmp_val {
                    Reg(reg) => registers[reg],
                    Lit(lit) => lit,
                };
                let shift = match shift_val {
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
            Tgl(reg) => {
                let target = ip as i64 + registers[reg];
                if target > 0 && target < tape.len() as i64 {
                    let target = target as usize;
                    match tape[target] {
                        Cpy(src, dst) => tape[target] = Jnz(src, dst),
                        Inc(a) => tape[target] = Dec(a),
                        Dec(a) => tape[target] = Inc(a),
                        Jnz(val, shift) => tape[target] = Cpy(val, shift),
                        Tgl(a) => tape[target] = Inc(a),
                    }
                }
            }
        }
        ip += 1;
    }
    registers
}

fn parse_args() -> std::io::Result<String> {
    let matches = clap::App::new("Day 23")
        .author("Devon Hollowood")
        .arg(clap::Arg::with_name("instructions")
            .help("file containing instructions to compute. Reads from stdin otherwise"))
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
    let mut instructions: Vec<_> = input.lines()
        .map(|line| line.trim().parse())
        .collect::<Result<_, _>>()
        .unwrap_or_else(|err| panic!("Error parsing instructions: {}", err));
    instructions.insert(0, Instruction::Cpy(Value::Lit(7), Value::Reg(0)));
    let final_state_1 = compute(&instructions);
    println!("part 1 final state: {:?}", final_state_1);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn example() {
        let instructions: Vec<_> = ["cpy 2 a", "tgl a", "tgl a", "tgl a", "cpy 1 a", "dec a",
                                    "dec a"]
            .into_iter()
            .map(|s| s.parse().unwrap())
            .collect();
        assert_eq!(compute(&instructions)[0], 3)
    }
}
