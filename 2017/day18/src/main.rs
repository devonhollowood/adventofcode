extern crate structopt;
#[macro_use]
extern crate structopt_derive;

use structopt::StructOpt;
use std::collections::HashMap;
use std::collections::VecDeque;
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

#[derive(Debug, Clone, Copy)]
enum Operation {
    Snd(Value),
    Set(Address, Value),
    Add(Address, Value),
    Mul(Address, Value),
    Mod(Address, Value),
    Rcv(Address),
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
                Ok(Snd(parts[1].parse()?))
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
                Ok(Rcv(parts[1].chars().next().ok_or(err)?))
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
    let mut snds = Vec::new();
    let mut instr = 0i64;
    let get = |v: Value, cpu: &HashMap<Address, Literal>| match v {
        Value::Literal(l) => l,
        Value::Address(a) => *cpu.get(&a).unwrap_or(&0),
    };
    while 0 <= instr && (instr as usize) < instructions.len() {
        use Operation::*;
        match instructions[instr as usize] {
            Snd(v) => snds.push(get(v, &computer)),
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
            Rcv(v) => {
                if get(Value::Address(v), &computer) != 0 {
                    return snds.last().cloned();
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Status {
    Running,
    Waiting,
    Terminated,
}

#[derive(Debug)]
struct Computer {
    id: u8,
    status: Status,
    instruction_ptr: usize,
    registers: HashMap<Address, Literal>,
    queue: VecDeque<Literal>,
}

impl Computer {
    fn new(id: u8) -> Computer {
        Computer {
            id: id,
            status: Status::Running,
            instruction_ptr: 0,
            registers: std::iter::once(('p', Literal::from(id))).collect(),
            queue: VecDeque::new(),
        }
    }
    fn lookup(&self, val: Value) -> Literal {
        match val {
            Value::Literal(l) => l,
            Value::Address(a) => *self.registers.get(&a).unwrap_or(&0),
        }
    }
    // returns Some(send) when sending a value
    fn perform(&mut self, op: Operation) -> Option<Literal> {
        //println!("{:?}", self);
        assert_eq!(self.status, Status::Running);
        use Operation::*;
        let mut ret = None;
        let mut incr = true;
        match op {
            Snd(v) => {
                let send = self.lookup(v);
                ret = Some(send)
            }
            Set(addr, v) => {
                *self.registers.entry(addr).or_insert(0) = self.lookup(v);
            }
            Add(addr, v) => {
                *self.registers.entry(addr).or_insert(0) += self.lookup(v);
            }
            Mul(addr, v) => {
                *self.registers.entry(addr).or_insert(0) *= self.lookup(v);
            }
            Mod(addr, v) => {
                *self.registers.entry(addr).or_insert(0) %= self.lookup(v)
            }
            Rcv(addr) => match self.queue.pop_front() {
                Some(lit) => *self.registers.entry(addr).or_insert(0) = lit,
                None => {
                    self.status = Status::Waiting;
                    incr = false;
                }
            },
            Jgz(v, jump) => {
                if self.lookup(v) > 0 {
                    let new_pos =
                        self.instruction_ptr as Literal + self.lookup(jump);
                    if new_pos < 0 {
                        self.status = Status::Terminated;
                    } else {
                        self.instruction_ptr = new_pos as usize;
                        incr = false;
                    }
                }
            }
        }
        if incr {
            self.instruction_ptr += 1;
        }
        ret
    }
}

fn part2(instructions: &[Operation]) -> usize {
    let mut send_count = 0;
    let mut computers = [Computer::new(0), Computer::new(1)];
    let mut current = 0;
    let mut other = if current == 0 { 1 } else { 0 };
    loop {
        let instr = instructions[computers[current].instruction_ptr];
        if let Some(rcv) = computers[current].perform(instr) {
            computers[other].queue.push_back(rcv);
            if current == 1 {
                send_count += 1;
            }
        }
        if computers[current].instruction_ptr > instructions.len() {
            computers[current].status = Status::Terminated;
        }
        if computers[current].status != Status::Running {
            std::mem::swap(&mut current, &mut other);
            match computers[current].status {
                Status::Running => {}
                Status::Waiting => if computers[current].queue.is_empty() {
                    break; // deadlock
                } else {
                    computers[current].status = Status::Running; // resume
                },
                Status::Terminated => break,
            }
        }
    }
    send_count
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
    println!("Part 2: {}", part2(&instructions));
}

#[derive(StructOpt, Debug)]
#[structopt(name = "day13", about = "Advent of code 2017 day 13")]
struct Opt {
    #[structopt(help = "Input file", parse(from_os_str))] input: PathBuf,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn part1_test() {
        let input = "set a 1\n\
                     add a 2\n\
                     mul a a\n\
                     mod a 5\n\
                     snd a\n\
                     set a 0\n\
                     rcv a\n\
                     jgz a -1\n\
                     set a 1\n\
                     jgz a -2";
        let instructions = parse(input).unwrap();
        assert_eq!(part1(&instructions), Some(4));
    }

    #[test]
    fn part2_test() {
        let input = "snd 1\n\
                     snd 2\n\
                     snd p\n\
                     rcv a\n\
                     rcv b\n\
                     rcv c\n\
                     rcv d";
        let instructions = parse(input).unwrap();
        assert_eq!(part2(&instructions), 3);
    }

    #[test]
    fn part2_test2() {
        let input = "set a 1\n\
                     add a 2\n\
                     mul a a\n\
                     mod a 5\n\
                     snd a\n\
                     set a 0\n\
                     rcv a\n\
                     jgz a -1\n\
                     set a 1\n\
                     jgz a -2";
        let instructions = parse(input).unwrap();
        assert_eq!(part2(&instructions), 1);
    }
}
