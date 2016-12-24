#[macro_use]
extern crate clap;

extern crate regex;

use std::io::prelude::*;
use std::fs::File;
use std::error::Error;
use std::collections::HashMap;
use regex::Regex;

#[derive(Debug)]
struct Bot {
    low: Option<usize>,
    high: Option<usize>,
    instruction: Option<Instruction>,
}

impl Bot {
    fn new() -> Bot {
        Bot {
            low: None,
            high: None,
            instruction: None,
        }
    }
    fn set_instruction(&mut self, instruction: Instruction) {
        self.instruction = Some(instruction);
    }
    fn take(&mut self, val: usize) {
        if self.low.is_none() {
            self.low = Some(val)
        } else if self.high.is_none() {
            let old_low = self.low.unwrap();
            if old_low < val {
                self.high = Some(val);
            } else {
                self.low = Some(val);
                self.high = Some(old_low);
            }
        }
    }
    fn ready(&self) -> bool {
        self.high.is_some() && self.low.is_some() && self.instruction.is_some()
    }
    fn act(&self) -> (Transfer, Transfer) {
        let low = self.low.expect("Called act on non-ready bot!");
        let high = self.high.expect("Called act on non-ready bot!");
        let instruction = self.instruction.as_ref().expect("Called act on non-ready bot!");
        (Transfer::new(low, instruction.low), Transfer::new(high, instruction.high))
    }
}

#[derive(Debug)]
struct Instruction {
    id: usize,
    low: Destination,
    high: Destination,
}

#[derive(Debug, Clone, Copy)]
enum Destination {
    Bot(usize),
    Output(usize),
}

impl std::fmt::Display for Destination {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        match *self {
            Destination::Bot(id) => write!(f, "bot {}", id),
            Destination::Output(id) => write!(f, "output {}", id),
        }
    }
}

#[derive(Debug)]
struct Transfer {
    value: usize,
    destination: Destination,
}

impl Transfer {
    fn new(value: usize, destination: Destination) -> Transfer {
        Transfer {
            value: value,
            destination: destination,
        }
    }
}

#[derive(Debug)]
struct AssemblyLine {
    bots: HashMap<usize, Bot>,
    output: HashMap<usize, usize>,
}

impl AssemblyLine {
    fn new() -> AssemblyLine {
        AssemblyLine {
            bots: HashMap::new(),
            output: HashMap::new(),
        }
    }
    fn run(&mut self) {
        let mut ready_stack: Vec<_> = self.bots
            .iter()
            .filter(|&(_, bot)| bot.ready())
            .map(|(id, _)| id)
            .cloned()
            .collect();
        while let Some(actor_id) = ready_stack.pop() {
            let (low_transfer, high_transfer) = self.bots
                .get(&actor_id)
                .expect(&format!("Actor {} does not exist", actor_id))
                .act();
            println!("bot {} sends {} to {} and {} to {}",
                     actor_id,
                     low_transfer.value,
                     low_transfer.destination,
                     high_transfer.value,
                     high_transfer.destination);
            for transfer in &[low_transfer, high_transfer] {
                self.transfer(transfer);
                if let Destination::Bot(id) = transfer.destination {
                    if self.bots[&id].ready() {
                        ready_stack.push(id);
                    }
                }
            }
        }
    }
    fn transfer(&mut self, &Transfer { value, ref destination }: &Transfer) {
        match *destination {
            Destination::Output(id) => {
                self.output.insert(id, value);
            }
            Destination::Bot(id) => self.bots.get_mut(&id).unwrap().take(value),
        }
    }
}

impl std::str::FromStr for AssemblyLine {
    type Err = BadInstruction;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        fn read_destination(s: &str) -> Destination {
            let (dest_type, id) = s.split_at(s.find(' ').unwrap());
            if dest_type == "bot" {
                Destination::Bot(id.trim().parse().expect(&format!("Bad id: {}", id)))
            } else {
                Destination::Output(id.trim().parse().expect(&format!("Bad id: {}", id)))
            }
        }
        let destination_re = r"(?:bot|output) \d+";
        let initialize_re = Regex::new(&format!(r"value (?P<val>\d+) goes to (?P<dest>{})",
                                                destination_re))
            .unwrap();
        let instruction_re = Regex::new(&format!(
            r"bot (?P<id>\d+) gives low to (?P<low>{}) and high to (?P<high>{})",
            destination_re, destination_re))
            .unwrap();
        let mut assembly_line = AssemblyLine::new();
        for line in s.lines() {
            if let Some(caps) = initialize_re.captures(line.trim()) {
                let val = caps["val"].parse().unwrap();
                let dest = read_destination(&caps["dest"]);
                match dest {
                    Destination::Bot(id) => {
                        assembly_line.bots.entry(id).or_insert_with(Bot::new).take(val);
                    }
                    Destination::Output(id) => {
                        assembly_line.output.insert(id, val);
                    }
                }
            } else if let Some(caps) = instruction_re.captures(line.trim()) {
                let id = caps["id"].parse().unwrap();
                let low_dest = read_destination(&caps["low"]);
                let high_dest = read_destination(&caps["high"]);
                assembly_line.bots
                    .entry(id)
                    .or_insert_with(Bot::new)
                    .set_instruction(Instruction {
                        id: id,
                        low: low_dest,
                        high: high_dest,
                    });
            } else {
                return Err(BadInstruction { bad_line: line.into() });
            }
        }
        Ok(assembly_line)
    }
}

#[derive(Debug)]
struct BadInstruction {
    bad_line: String,
}

impl std::fmt::Display for BadInstruction {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        write!(f, "Bad line: {}", self.bad_line)
    }
}

impl Error for BadInstruction {
    fn description(&self) -> &str {
        "Invalid line"
    }
    fn cause(&self) -> Option<&Error> {
        None
    }
}


fn parse_args() -> std::io::Result<String> {
    let matches = clap::App::new("Day 10")
        .author("Devon Hollowood")
        .arg(clap::Arg::with_name("instructions")
            .index(1)
            .short("f")
            .long("instructions")
            .help("instructions to run. Reads from stdin otherwise")
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
    let input = parse_args().unwrap_or_else(|err| panic!("Error reading input: {}", err));
    let mut assembly_line = match input.parse::<AssemblyLine>() {
        Ok(line) => line,
        Err(e) => panic!("Error: {}", e),
    };
    assembly_line.run();
}
